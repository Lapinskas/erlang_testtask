%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)
%%
%% Based on Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%%
%% See MIT-LICENSE for licensing information.

-module(test_client).

% ========== TEST CLIENT ===========================================
%
% Test client to test functionality of Protocol Buffer Server
%
% PREREQUISITES:
% - protobuff server is running on localhost on 8000 port
%
% Example:
%	1> test_client:set("key1","This is a test value for key1").
%	Test set: ok
%	ok
%	2> test_client:get("key1").
%	Test get: ok
%	Test Key: "key1"
%	Test Value: "This is a test value for key1"
%	ok
%
% ==================================================================

-export([
	    start/0,
	    set/2,
	    get/1
        ]).

-include("kv_pb.hrl").

start() ->
    application:start(inets),
    inets:start(httpc, [{profile, default}]).

set(Key,Value) ->
    Request = gpb:encode(set_request,Key,Value),
    {ok, Response} = pb_call("localhost", 8000, Request),
    Result = gpb:decode(set_response,Response),
    io:fwrite("Test set: ~s~n", [Result]).

get(Key) ->
    Request = gpb:encode(get_request,Key),
    {ok, Response} = pb_call("localhost", 8000, Request),
    Result = gpb:decode(get_response,Response),
    case Result of
	{Res,ResKey,Value} ->
	    io:format("Test get: ~p~n", [Res]),
	    io:format("Test Key: ~p~n", [ResKey]),
	    io:format("Test Value: ~p~n", [Value]);

	{Res} ->
	    io:format("Test get: ~p~n", [Res])
    end.

%%% Helper Functions %%%

pb_call(Host, Port, Req) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 4}, {header, 1}]) of
        {ok, Sock} ->
            Result = pb_call(Sock, Req),
            gen_tcp:close(Sock),
            Result;
        Other ->
            io:format("Unable to establish connection: ~p~n", [Other])
    end.
    
pb_call(Sock, Req) ->
    ok = gen_tcp:send(Sock, Req),
    receive
        {tcp, _Port, Response} ->
            {ok, Response};
        {tcp_closed, Port} ->
            io:format("Connection closed after sending data: ~p~n", [Port]),
            {error, {tcp_closed, Port}};
        Other ->
            io:format("Unexpected message: ~p~n", [Other]),
            {error, Other}
    end.
