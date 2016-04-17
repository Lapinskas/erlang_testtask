%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)

-module(encryption).

-define(CHUNKSIZE,2).

-export([
	enc/1,
	dec/1,
        encrypt64/1,
	decrypt64/1,
	chunks/2,
	check_result/1,
	get_result/1
	]).

enc(Data) ->
    Chunks 	= chunks(Data,?CHUNKSIZE),
    Chipher	= lists:map (
		    fun(X) -> 
			    encrypt64(X)
		    end,
		    Chunks),
    case check_result(Chipher) of
	true ->
	    {ok,get_result(Chipher)};
	false ->
	    {error}
    end.

dec (Chipher) ->
    Chunks = lists:map (
		    fun(X) ->
			decrypt64(X)
		    end,
		    Chipher),
    case check_result(Chunks) of
	true ->
	    Res = 
		lists:flatten(
		    get_result(Chunks)
		),
	    {ok,Res};
	false->
	    {error}
    end.

encrypt64 (Data) ->
    KEYID   = os:getenv("AWS_ENCRYPT_KEY_ID"),
    Data64 = base64:encode(Data),
    case erlcloud_kms:encrypt(
	    list_to_binary([KEYID]),
	    list_to_binary([Data64])
	) of
	    {ok,Result} ->
		[{_,Chipher},_] = Result,
		{ok,Chipher};
	    _Else ->
		{error,{orig,Data}}
    end.

decrypt64 (Chipher) ->
    case erlcloud_kms:decrypt(
    	    list_to_binary([Chipher])
	) of
	    {ok,Result} ->
		[_,{_,Plain64}] = Result,
		Plain = base64:decode(Plain64),
		{ok,binary_to_list(Plain)};
	    _Else ->
		{error}
    end.

% Helper functions

check_result (Res) ->
    lists:all(
	fun(X) ->
	    case X of 
		{ok,_} -> true; 
		_Else -> false
	    end
	end,
	Res
    ).

get_result (Res) ->
    lists:map(
	fun(X) ->
	    case X of 
		{ok,Data} 
		    -> Data;
		_Else -> ""
	    end
	end,
	Res
    ).

chunks([],_) -> [];

chunks(List,Len) when Len > length(List) ->
    [List];
    
chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | chunks(Tail,Len)].