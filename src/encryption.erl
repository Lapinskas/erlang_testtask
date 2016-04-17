%% Erlang Test Task
%% AWS KMS encryption high-level API
%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)
%% See MIT-LICENSE for licensing information.

-module(encryption).

% AWS KMS API has 4K restriction on the data size
% It is not clear from documentation whether this limit applicable to Base64 or plain data
% Data is chunked transparantly using CHUNKSIZE buffers
% 1K seems to be sufficient for both performance and API limitations.
-define(CHUNKSIZE,1000).


% ================ AWS KMS encryption API ==========================================================
%
% PREREQUISITES:
%  AWS Encryption Key should be obtained on Amazon KMS and exported as AWS_ENCRYPT_KEY_ID
% 
% enc(String)				=> {ok,[list of encrypted chunks]} 
%					=> {error} in case of any chunk encryption error
%
% dec([list of encrypted chunks])	=> {ok,String} 
%					=> {error} in case of any chunk deencryption error
%
% ==================================================================================================

-export([
	enc/1,
	dec/1
	]).

% =========== IMPLEMENTATION ==================================
%
% Implementation is based on Erlcloud fork from Alertlogic.com
% https://github.com/alertlogic/erlcloud.git
%
% =============================================================

enc(Data) ->
    % If Data is longer than CHUNKSIZE, it is splitted to smaller chunks
    Chunks 	= chunks(Data,?CHUNKSIZE),

    % Encryption is done for every chunk
    Chipher	= lists:map (
		    fun(X) -> 
			    encrypt64(X)
		    end,
		    Chunks),
		    
    % Fail in case of any chunk encryption fails
    % TODO: retry for failed chunks
    case check_result(Chipher) of
	true ->
	    {ok,get_result(Chipher)};
	false ->
	    {error}
    end.

dec (Chipher) ->
    % Decryption is done for every chunk
    Chunks = lists:map (
		    fun(X) ->
			decrypt64(X)
		    end,
		    Chipher),

    % Fail in case of any chunk decryption fails
    % TODO: retry for failed chunks
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

% It is NOT documented, that data should be Base64 encoded before encryption
encrypt64 (Data) ->
    KEYID   = os:getenv("AWS_ENCRYPT_KEY_ID"),

    % First, Base64 encode Data
    Data64 = base64:encode(Data),
    
    % Then, encrypt it
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

% For decryption data should be converted back from Base64
decrypt64 (Chipher) ->

    % First, decrypt data
    case erlcloud_kms:decrypt(
    	    list_to_binary([Chipher])
	) of
	    {ok,Result} ->
		[_,{_,Plain64}] = Result,
		
		% Then, Base64 decode data
		Plain = base64:decode(Plain64),
		{ok,binary_to_list(Plain)};
	    _Else ->
		{error}
    end.

% Helper functions

% This function checks if all chunks have been successfully processed
% Rerurns false in case of any encrypt/decript operation failure of any chunk
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

% This function provides a list of encrypt/decript operation result
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

% Classical implementation of chunks
% Not fastest, but clean for understanding
chunks([],_) -> [];

chunks(List,Len) when Len > length(List) ->
    [List];
    
chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | chunks(Tail,Len)].
    
