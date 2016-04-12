%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)

-module(encryption).

-export([
        encrypt/1,
	decrypt/1,
	chunks/2,
	check_result/1
	]).

encrypt (Data) ->
    KEYID 	= os:getenv("AWS_ENCRYPT_KEY_ID"),
    Base64 	= base64:encode_to_string(Data),
    Chunks 	= chunks(Base64,2),
    Chipher	= lists:map (
		    fun(X) -> 
			erlcloud_kms:encrypt(
			    list_to_binary([KEYID]),
			    list_to_binary(X))
		    end,
		    Chunks),
    case check_result(Chipher) of
	true ->
	    {ok,Chipher};
	false ->
	    {error,{keyid,KEYID}}
    end.

decrypt (Cipher) ->
    case erlcloud_kms:decrypt(list_to_binary([Cipher])) of
	{ok,[
	    _,
	    {_,Data}
	]} ->
	    {ok,base64:decode(Data)};

	{error,Reason} ->
	    {error,Reason}
	
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

chunks([],_) -> [];

chunks(List,Len) when Len > length(List) ->
    [List];
    
chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | chunks(Tail,Len)].