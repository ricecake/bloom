-module(bloom).

%% API exports
-export([
	new/2,
	new_manual/2,
	add/2,
	exists/2,
	union/2,
	intersection/2,
	difference/2,
	optimal_params/2
]).

-export_type([bloom_state/0]).

-define(BLOCK, 32).
-record(bloom_state, {state :: binary(), width :: pos_integer(), rounds :: pos_integer()}).

-opaque bloom_state() :: #bloom_state{state :: binary(), width :: pos_integer(), rounds :: pos_integer()}.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.


%%====================================================================
%% API functions
%%====================================================================

-spec new(Elements :: pos_integer(),  Odds :: pos_integer()) -> bloom_state().


new(Elements, Odds) ->
	{ok, {Width, Hashes}} = optimal_params(Elements, Odds),
	new_manual(Width, Hashes).


-spec new_manual(Width :: pos_integer(), Rounds :: pos_integer()) -> bloom_state().

new_manual(Width, Rounds) when Width rem ?BLOCK == 0 ->
	#bloom_state{ state= <<0:Width>>, width=Width, rounds=Rounds}.


-spec optimal_params(Elements :: pos_integer(), Odds :: pos_integer()) ->
	{ok, {Width :: pos_integer(), Rounds :: pos_integer() }}.

optimal_params(Elements, Odds) when Elements > 0, Odds > 0 ->
	Probability = math:pow(Odds, -1),
	%% -1/( ln(2)^2 ) == -2.0813689810056077
	Width = nearest_block_size(-2.0813689810056077 * ( Elements * math:log(Probability))),
	Hashes = round((Width/Elements) * math:log(2)),
	{ok, {Width, Hashes}}.

-spec add(Filter :: bloom_state(), Data:: term()) -> NewFilter :: bloom_state().

add(#bloom_state{ state=State, width=Width, rounds=Rounds} = Bloom, Data) when is_binary(Data) ->
	NewState = setBits(State, hash_bits(Width, Data, lists:seq(1, Rounds))),
	Bloom#bloom_state{ state=NewState };
add(State, Data) when not is_binary(Data) ->
	add(State, term_to_binary(Data)).


exists(#bloom_state{ state=State, width=Width, rounds=Rounds}, Data) when is_binary(Data) ->
	lists:all(fun(HashValue) ->
		getBit(State, HashValue)
	end, hash_bits(Width, Data, lists:seq(1, Rounds)));
exists(State, Data) when not is_binary(Data) ->
	exists(State, term_to_binary(Data)).


union(#bloom_state{}, #bloom_state{}) -> error.

intersection(#bloom_state{}, #bloom_state{}) -> error.

difference(#bloom_state{}, #bloom_state{}) -> error.

%%====================================================================
%% Internal functions
%%====================================================================

nearest_block_size(Length) when is_float(Length) -> nearest_block_size(trunc(Length));
nearest_block_size(Length) when Length rem ?BLOCK == 0 -> Length;
nearest_block_size(Length) -> (Length + ?BLOCK) - (Length rem ?BLOCK).

getBit(Bin, N)->
	case Bin of
		<<_:N/bits, 0:1, _/bits>> -> false;
		<<_:N/bits, 1:1, _/bits>> -> true
	end.

hash_bits(Width, Data, Taints) ->
	MainHash = erlang:crc32(Data),
	lists:sort([erlang:crc32(MainHash, <<Taint>>) rem Width || Taint <- Taints]).

setBits(Bin, []) -> Bin;
setBits(Bin, [Offset |Rest]) ->
	case Bin of
		<<_:Offset/bits,1:1,_/bits>> -> setBits(Bin, Rest);
		<<A:Offset/bits,0:1,B/bits>> -> setBits(<<A:Offset/bits,1:1,B/bits>>, Rest)
	end.

-ifdef(TEST).

basic_test_() ->
	{"Bloom Filter Tests", [
		{"basic tests", [
			{"Can simple create", ?_assertMatch(#bloom_state{width=480, rounds=3}, new(100, 10))},
			{"Can manual create", ?_assertMatch(#bloom_state{width=1024, rounds=3}, new_manual(1024, 3))},
			{"Can add", ?_assertMatch(#bloom_state{}, add(new_manual(1024,3), cat))},
			{"Can double add", ?_assertMatch(#bloom_state{}, add(add(new_manual(1024,3), cat), cat))},
			{"Can get optimal params", ?_assertMatch({ok, {_, _}}, optimal_params(100, 10))},
			{"Can check missing", ?_assertNot(exists(new_manual(1024,3), cat))},
			{"Can check present", ?_assert(exists(add(new_manual(1024,3), cat), cat))}
		]}
	]}.

-endif.
