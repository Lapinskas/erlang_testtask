%% Erlang Test Task
%% Google Protocol Buffers high-level API
%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)
%% See MIT-LICENSE for licensing information.

-module (gpb).

-include("kv_pb.hrl").

% ===== API =====
-export([
    encode/2,	% encode specific requests
    encode/3,	% encode specific requests
    encode/4,	% encode specific requests
    decode/1,	% universal decode request
    decode/2	% decode specific requests
    ]).

% ===== IMPLEMENTATION =====

% === encode functions ===

encode(set_request,Key,Value) ->
    kv_pb:encode({
		    req_envelope,
		    set_request_t,
		    {set_request,
			{data,list_to_binary(Key),list_to_binary(Value)}
		    },
		    undefined,
		    undefined,
		    undefined
		}).
		
encode(set_response,Result) ->
    kv_pb:encode({
		    req_envelope,
		    set_response_t,
		    undefined,
		    {set_response,Result},
		    undefined,
		    undefined
		});

encode(get_request,Key) ->
    kv_pb:encode({
		    req_envelope,
		    get_request_t,
		    undefined,
		    undefined,
		    {get_request,Key},
		    undefined
		});

encode(get_response,Result) ->
    kv_pb:encode({
		    req_envelope,
		    get_response_t,
		    undefined,
		    undefined,
		    undefined,
		    {get_response,
			Result,
			undefined
		    }
		}).

encode(get_response,Result,Key,Value) ->
    kv_pb:encode({
		    req_envelope,
		    get_response_t,
		    undefined,
		    undefined,
		    undefined,
		    {get_response,
			Result,
			{data,list_to_binary(Key),list_to_binary(Value)}
		    }
		}).

% === decode functions ===

decode(Data) ->
    kv_pb:decode(
		    req_envelope,
		    list_to_binary(Data)
		).

decode(set_request,Data) ->
    {req_envelope,
	set_request_t,
	{set_request,{data,Key,Value}},
	undefined,
	undefined,
	undefined
    }	= kv_pb:decode(
		    req_envelope,
		    list_to_binary(Data)
		),
    {Key,Value};

decode(set_response,Data) ->
    {req_envelope,
	set_response_t,
	undefined,
	{set_response,Result},
	undefined,
	undefined
    }	= kv_pb:decode(
		    req_envelope,
		    list_to_binary(Data)
		),
    Result;
    
decode(get_request,Data) ->
    {req_envelope,
	get_request_t,
	undefined,
	undefined,
	{get_request,Key},
	undefined
    }	= kv_pb:decode(
		    req_envelope,
		    list_to_binary(Data)
		),
    Key;

decode(get_response,Data) ->
    case kv_pb:decode(
		    req_envelope,
		    list_to_binary(Data)
		)
    of

    {req_envelope,
	get_response_t,
	undefined,
	undefined,
	undefined,
	{get_response,
	    Result,
	    undefined
	}
    } -> {Result,undefined};
    
    {req_envelope,
	get_response_t,
	undefined,
	undefined,
	undefined,
	{get_response,
	    Result,
	    {data,Key,Value}
	}
    } -> {Result,Key,Value}

    end.