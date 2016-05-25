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

%@doc Returns a new bloom filter, given the approximate number of elements, and the desired conflict resilience. 

-spec new(Elements :: pos_integer(),  Odds :: pos_integer()) -> bloom_state().


new(Elements, Odds) ->
	{ok, {Width, Hashes}} = optimal_params(Elements, Odds),
	new_manual(Width, Hashes).

%@doc Returns a new bloom filter, with manually specified bit width and hash count.

-spec new_manual(Width :: pos_integer(), Rounds :: pos_integer()) -> bloom_state().

new_manual(Width, Rounds) when Width rem ?BLOCK == 0 ->
	#bloom_state{ state= <<0:Width>>, width=Width, rounds=Rounds}.


%@doc Given an estimate of the number of elements in the filter, and the conflict resilience, returns the optimal bit width and hash count

-spec optimal_params(Elements :: pos_integer(), Odds :: pos_integer()) ->
	{ok, {Width :: pos_integer(), Rounds :: pos_integer() }}.

optimal_params(Elements, Odds) when Elements > 0, Odds > 0 ->
	Probability = math:pow(Odds, -1),
	%% -1/( ln(2)^2 ) == -2.0813689810056077
	Width = nearest_block_size(-2.0813689810056077 * ( Elements * math:log(Probability))),
	Hashes = round((Width/Elements) * math:log(2)),
	{ok, {Width, Hashes}}.

%@doc Adds a new element to the filter
%
% If the item is not a binary, it will be converted using term to binary
%
%@end

-spec add(Filter :: bloom_state(), Data:: term()) -> NewFilter :: bloom_state().

add(#bloom_state{state=State, width=Width, rounds=Rounds} = Bloom, Data) when is_binary(Data) ->
	NewState = setBits(State, hash_bits(Width, Data, lists:seq(1, Rounds))),
	Bloom#bloom_state{ state=NewState };
add(State, Data) when not is_binary(Data) ->
	add(State, term_to_binary(Data)).


%@doc Checks for the existence of an element in the filter.
%
% Will convert non-binary terms into binary useing term_to_binary
%
%@end

-spec exists(Filter :: bloom_state(), Data :: term()) -> Exists :: boolean().

exists(#bloom_state{state=State, width=Width, rounds=Rounds}, Data) when is_binary(Data) ->
	lists:all(fun(HashValue) ->
		getBit(State, HashValue)
	end, hash_bits(Width, Data, lists:seq(1, Rounds)));
exists(State, Data) when not is_binary(Data) ->
	exists(State, term_to_binary(Data)).


-spec union(LeftFilter :: bloom_state(), RightFilter :: bloom_state()) -> UnionFilter :: bloom_state().

union(#bloom_state{state=LeftState, width=Width, rounds=Rounds}, #bloom_state{state=RightState, width=Width, rounds=Rounds}) ->
	#bloom_state{state=merge_binary(LeftState, RightState, <<>>), width=Width, rounds=Rounds}.


-spec intersection(LeftFilter :: bloom_state(), RightFilter :: bloom_state()) -> IntersectionFilter :: bloom_state().

intersection(#bloom_state{state=LeftState, width=Width, rounds=Rounds}, #bloom_state{state=RightState, width=Width, rounds=Rounds}) ->
	#bloom_state{state=intersect_binary(LeftState, RightState, <<>>), width=Width, rounds=Rounds}.


-spec difference(LeftFilter :: bloom_state(), RightFilter :: bloom_state()) -> DifferenceFilter :: bloom_state().

difference(#bloom_state{state=LeftState, width=Width, rounds=Rounds}, #bloom_state{state=RightState, width=Width, rounds=Rounds}) ->
	#bloom_state{state=diff_binary(LeftState, RightState, <<>>), width=Width, rounds=Rounds}.

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

merge_binary(<<LeftBlock:?BLOCK/integer>>, <<RightBlock:?BLOCK/integer>>, Acc) -> <<Acc/binary, (LeftBlock bor RightBlock):?BLOCK/integer>>;
merge_binary(<<LeftBlock:?BLOCK/integer, LeftRest/binary>>, <<RightBlock:?BLOCK/integer, RightRest/binary>>, Acc) ->
	merge_binary(LeftRest, RightRest, <<Acc/binary, (LeftBlock bor RightBlock):?BLOCK/integer>>).

intersect_binary(<<LeftBlock:?BLOCK/integer>>, <<RightBlock:?BLOCK/integer>>, Acc) -> <<Acc/binary, (LeftBlock band RightBlock):?BLOCK/integer>>;
intersect_binary(<<LeftBlock:?BLOCK/integer, LeftRest/binary>>, <<RightBlock:?BLOCK/integer, RightRest/binary>>, Acc) ->
	intersect_binary(LeftRest, RightRest, <<Acc/binary, (LeftBlock band RightBlock):?BLOCK/integer>>).

diff_binary(<<LeftBlock:?BLOCK/integer>>, <<RightBlock:?BLOCK/integer>>, Acc) -> <<Acc/binary, (LeftBlock bxor RightBlock):?BLOCK/integer>>;
diff_binary(<<LeftBlock:?BLOCK/integer, LeftRest/binary>>, <<RightBlock:?BLOCK/integer, RightRest/binary>>, Acc) ->
	diff_binary(LeftRest, RightRest, <<Acc/binary, (LeftBlock bxor RightBlock):?BLOCK/integer>>).


-ifdef(TEST).

basic_test_() ->
	{"Bloom Filter Tests", [
		{"basic tests", [
			{"Can get optimal params", ?_assertMatch({ok, {_, _}}, optimal_params(100, 10))},
			{"create tests", [
				{"Can simple create", ?_assertMatch(#bloom_state{width=480, rounds=3}, new(100, 10))},
				{"Can manual create", ?_assertMatch(#bloom_state{width=1024, rounds=3}, new_manual(1024, 3))}
			]},
			{"add tests", [
				{"Can add", ?_assertMatch(#bloom_state{}, add(new_manual(1024,3), cat))},
				{"Can binary add", ?_assertMatch(#bloom_state{}, add(new_manual(1024,3), <<"cat">>))},
				{"Can double add", ?_assertMatch(#bloom_state{}, add(add(new_manual(1024,3), cat), cat))}
			]},
			{"exists checks", [
				{"Can check missing", ?_assertNot(exists(new_manual(1024,3), cat))},
				{"Can check present", ?_assert(exists(add(new_manual(1024,3), cat), cat))}
			]},
			{"Multifilter tests", setup, fun() -> {new_manual(1024,3), new_manual(1024,3)} end, fun({Left, Right})->[
				{"union tests", [
					{"can basic union", ?_assertMatch(#bloom_state{}, union(Left, Right))},
					{"add left, exists", ?_assert(exists(union(add(Left, cat), Right), cat))},
					{"add right, exists", ?_assert(exists(union(add(Right, dog), Left), dog))},
					{"add both, left exists", ?_assert(exists(union(add(Left, cat), add(Right, dog)), cat))},
					{"add both, right exists", ?_assert(exists(union(add(Left, cat), add(Right, dog)), dog))},
					{"add same both and exists", ?_assert(exists(intersection(add(Left, cat), add(Right, cat)), cat))}
				]},
				{"intersect tests", [
					{"can basic intersection", ?_assertMatch(#bloom_state{}, intersection(Left, Right))},
					{"add left, not exists", ?_assertNot(exists(intersection(add(Left, cat), Right), cat))},
					{"add right, not exists", ?_assertNot(exists(intersection(add(Right, dog), Left), dog))},
					{"add both, left not exists", ?_assertNot(exists(intersection(add(Left, cat), add(Right, dog)), cat))},
					{"add both, right not exists", ?_assertNot(exists(intersection(add(Left, cat), add(Right, dog)), dog))},
					{"add same both and exists", ?_assert(exists(intersection(add(Left, cat), add(Right, cat)), cat))}
				]},
				{"difference tests", [
					{"can basic difference", ?_assertMatch(#bloom_state{}, difference(Left, Right))},
					{"add left and exists", ?_assert(exists(difference(add(Left, cat), Right), cat))},
					{"add right and exists", ?_assert(exists(difference(add(Right, dog), Left), dog))},
					{"add both, left exists", ?_assert(exists(difference(add(Left, cat), add(Right, dog)), cat))},
					{"add both, right exists", ?_assert(exists(difference(add(Left, cat), add(Right, dog)), dog))},
					{"add same both not exists", ?_assertNot(exists(difference(add(Left, cat), add(Right, cat)), cat))}
				]}
			] end}
		]}
	]}.

-endif.
