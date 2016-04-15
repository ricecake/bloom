-module(bloom).

%% API exports
-export([
	new/2,
	new_manual/2,
	add/2,
	exists/2,
	optimal_params/2,
	setBits/2,
	getBit/2
]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.


%%====================================================================
%% API functions
%%====================================================================

new(Elements, Odds) ->
	{ok, {Width, Hashes}} = optimal_params(Elements, Odds),
	new_manual(Width, Hashes).

new_manual(Width, Rounds) when Width rem 32 == 0 ->
	{bloom_state, <<0:Width>>, Width, Rounds}.

optimal_params(Elements, Odds) ->
	Probability = math:pow(Odds, -1),
	%%                                v -1/( ln(2)^2 )
	Width = nearest_block_size(-2.0813689810056077 * ( Elements * math:log(Probability))),
	Hashes = round((Width/Elements) * math:log(2)),
	{ok, {Width, Hashes}}.

add({bloom_state, State, Width, Rounds}, Data) when is_binary(Data) ->
	NewState = setBits(State, hash_bits(Width, Data, lists:seq(1, Rounds))),
	{bloom_state, NewState, Width, Rounds};
add(State, Data) when not is_binary(Data) ->
	add(State, term_to_binary(Data)).


exists({bloom_state, State, Width, Rounds}, Data) when is_binary(Data) ->
	lists:all(fun(HashValue) ->
		getBit(State, HashValue)
	end, hash_bits(Width, Data, lists:seq(1, Rounds)));
exists(State, Data) when not is_binary(Data) ->
	exists(State, term_to_binary(Data)).


%%====================================================================
%% Internal functions
%%====================================================================


nearest_block_size(Length) when is_float(Length) -> nearest_block_size(trunc(Length));
nearest_block_size(Length) when Length rem 32 == 0 -> Length;
nearest_block_size(Length) -> (Length + 32) - (Length rem 32).

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
