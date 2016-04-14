-module(bloom).

%% API exports
-export([
	new/2,
	new_manual/2,
	add/2,
	exists/2,
	optimal_params/2,
	setBits/2,
	checkBits/2,
	compute_deltas/3
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
	%NewState = setBits(State, compute_deltas(lists:sort(hash_bits(Width, Data, lists:seq(1, Rounds)))),
	NewState = do_add(State, Data, Width, Rounds),
	{bloom_state, NewState, Width, Rounds};
add(State, Data) when not is_binary(Data) ->
	add(State, term_to_binary(Data)).


exists({bloom_state, State, Width, Rounds}, Data) ->
	lists:all(fun(HashValue) ->
		getBit(State, HashValue)
	end, hash_bits(Width, Data, lists:seq(1, Rounds)));
exists(State, Data) when not is_binary(Data) ->
	exists(State, term_to_binary(Data)).


%%====================================================================
%% Internal functions
%%====================================================================

do_add(State, Data, Width, 1) ->
	HashValue = hash_bit(Width, Data, <<1>>),
	setBit(State, HashValue);
do_add(State, Data, Width, N) ->
	HashValue = hash_bit(Width, Data, <<N>>),
	NewState  = setBit(State, HashValue),
	do_add(NewState, Data, Width, N-1).


nearest_block_size(Length) when is_float(Length) -> nearest_block_size(trunc(Length));
nearest_block_size(Length) when Length rem 32 == 0 -> Length;
nearest_block_size(Length) -> (Length + 32) - (Length rem 32).

getBit(Bin, N)->
	case Bin of
		<<_:N/bits, 0:1, _/bits>> -> false;
		<<_:N/bits, 1:1, _/bits>> -> true
	end.

setBit(Bin, N)->
	case Bin of
		<<_:N/bits,1:1,_/bits>> -> Bin;
		<<A:N/bits,0:1,B/bits>> -> <<A:N/bits,1:1,B/bits>>
	end.

hash_bit(Width, Data, Taint) ->
	erlang:crc32(<<Data/binary, Taint/binary>>) rem Width.

hash_bits(Width, Data, Taints) ->
	MainHash = erlang:crc32(Data),
	[erlang:crc32(MainHash, <<Taint>>) rem Width || Taint <- Taints].

compute_deltas(_Offset, [], Acc) -> lists:reverse(Acc);
compute_deltas(Offset, [Item |Rest], Acc) ->
	compute_deltas(Item, Rest, [Item - Offset | Acc]).

setBits(Bin, []) -> Bin;
setBits(Bin, [Offset |Rest]) ->
	case Bin of
		<<_:Offset/bits,1:1,_/bits>> -> setBits(Bin, Rest);
		<<A:Offset/bits,0:1,B/bits>> -> <<A/bits, 1:1, (setBits(B, [ N - (Offset+1) || N <- Rest]))/bits>>
	end.

checkBits(_Bin, []) -> true;
checkBits(Bin, [Offset |Rest]) ->
	case Bin of
		<<_:Offset/bits, 0:1, _/bits>> -> false;
		<<_:Offset/bits, 1:1, B/bits>> -> checkBits(B, Rest)
	end.
