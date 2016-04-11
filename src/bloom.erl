-module(bloom).

%% API exports
-export([
	new/2,
	new_manual/2,
	add/2,
	exists/2,
	optimal_params/2
]).

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

add({bloom_state, State, Width, Rounds}, Data) ->
	NewState = lists:foldl(fun(El, Acc) ->
		HashValue = hash_bit(Width, Data, <<El>>),
		setBit(Acc, HashValue)
	end, State, lists:seq(1, Rounds)),
	{bloom_state, NewState, Width, Rounds}.

exists({bloom_state, State, Width, Rounds}, Data) ->
	lists:all(fun(El) ->
		HashValue = hash_bit(Width, Data, <<El>>),
		getBit(State, HashValue)
	end, lists:seq(1, Rounds)).


%%====================================================================
%% Internal functions
%%====================================================================

% can be converted into do_intersection
%%do_exists(<<>>, <<>>) -> true;
%%do_exists(<<SByte:32, SRest/binary>>, <<DByte:32, DRest/binary>>) when DByte == SByte band DByte ->
%%	do_exists(SRest, DRest);
%%do_exists(_,_) -> false.

% can be converted into do_union
%%do_add(<<>>, <<>>, Result) -> Result;
%%do_add(<< StateByte:32, StateRest/binary>>, << DataByte:32, DataRest/binary>>, Accumulator) ->
%%	NewAccByte = StateByte bor DataByte,
%%	do_add(StateRest, DataRest, <<Accumulator/binary, NewAccByte:32>>).

% Find a place to stash these.
%lcg_hash(Width, Data) -> lcg_hash(Width, Data, <<>>).
%
%%% m = 2^32, a = 214013, c = 2531011
%%% X_n+1 = (a*X_n+c) mod m
%lcg_hash(Width, Data, Taint) when Width rem 32 == 0 ->
%	BinaryData   = erlang:term_to_binary(Data),
%	BinaryTaint  = erlang:term_to_binary(Taint),
%	HashTaint    = erlang:adler32(<<BinaryTaint/binary, BinaryData/binary>>),
%	InitialState = erlang:crc32(<<HashTaint:32, BinaryData/binary>>),
%	do_lcg_hash(InitialState, <<InitialState:32>>, Width-32).
%
%do_lcg_hash(_State, Acc, 0) -> Acc;
%do_lcg_hash(State, Acc, Width) ->
%	<< NewState:32 >> = << (214013 * State + 2531011):32 >>,
%	do_lcg_hash(NewState, <<Acc/binary, NewState:32>>, Width-32).

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

hash_bit(Width, Data, Taint) when is_binary(Data) ->
	do_hash(Width, <<Data/binary, Taint/binary>>);
hash_bit(Width, Data, Taint) ->
	BinaryData   = erlang:term_to_binary(Data),
	do_hash(Width, <<BinaryData/binary, Taint/binary>>).

do_hash(Width, Data) ->
	erlang:crc32(Data) rem Width.
