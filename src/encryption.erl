%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)

-module(encryption).

-define(CHUNKSIZE,1000).

-export([
        encrypt/1,
	decrypt/1,
	chunks/2,
	check_result/1,
	check_dec_result/1
	]).

encrypt (Data) ->
    KEYID 	= os:getenv("AWS_ENCRYPT_KEY_ID"),
    Hex 	= hex:bin_to_hexstr(list_to_binary(Data)),
    Chunks 	= chunks(Hex,?CHUNKSIZE),
    Chipher	= lists:map (
		    fun(X) -> 
			Len = string:len(X),
			{Res,Enc} = 
			    erlcloud_kms:encrypt(
				list_to_binary([KEYID]),
				list_to_binary(X)),
			{Res,Len,Enc}
		    end,
		    Chunks),
    case check_result(Chipher) of
	true ->
	    {ok,encrypt_result(Chipher)};
	false ->
	    {error,{keyid,KEYID}}
    end.

decrypt (Chipher) ->
    Chunks = lists:map (
		    fun(X) ->
			{Len,Enc} = X,
			Data = erlcloud_kms:decrypt(list_to_binary([Enc])),
			{Len,Data}
		    end,
		    Chipher),
    case check_dec_result(Chunks) of
	true ->
	    Hex = 
		hex:hexstr_to_bin(
			lists:flatten(
			    decrypt_result(Chunks)
			)
		),
	    {ok,Hex};
	false->
	    {error}
    end.

% Helper functions

check_result (Res) ->
    lists:all(
	fun(X) ->
	    case X of 
		{ok,_,_} -> true; 
		_Else -> false
	    end
	end,
	Res
    ).

check_dec_result (Res) ->
    lists:all(
	fun(X) ->
	    case X of 
		{_,{ok,_}} -> true; 
		_Else -> false
	    end
	end,
	Res
    ).

encrypt_result (Res) ->
    lists:map(
	fun(X) ->
	    case X of 
		{ok,Len,[{_,Data},_]} 
		    -> {Len,Data};
		_Else -> ""
	    end
	end,
	Res
    ).

decrypt_result (Res) ->
    lists:map(
	fun(X) ->
	    case X of 
		{Len,{ok,[_,{_,Data}]}} -> 
		    SData = binary_to_list(Data),
		    string:left(SData,Len);
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