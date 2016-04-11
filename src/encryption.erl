%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)

-module(encryption).

-export([
        encrypt/1,
	decrypt/1,
	chunks/2
	]).

encrypt (Data) ->
    KEYID = os:getenv("AWS_ENCRYPT_KEY_ID"),
    Base64 = base64:encode_to_string(Data),
    case erlcloud_kms:encrypt(list_to_binary([KEYID]),list_to_binary(Base64)) of
	{ok,[
	    {_,Cipher},
	    _
	]} ->
	{ok,Cipher};

	{error,Reason} ->
	    {error,Reason,{keyid,KEYID}}
	
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

chunks([],_) -> [];

chunks(List,Len) when Len > length(List) ->
    [List];
    
chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | chunks(Tail,Len)].