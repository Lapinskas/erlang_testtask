%% Erlang Test Task
%% Google Protocol Buffers high-level API
%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)
%% See MIT-LICENSE for licensing information.

-module (gpb).

-include("kv_pb.hrl").

% ================ Google Protocol Buffers kv.proto API ===============================================================================
%
% Protocol buffers description: kv.proto file
%
% "encode" functions return Google Protocol Buffer row data
% encode(set_request,	Key,Value)		=> Key:Value strings pair
% encode(set_response,	Result)			=> Result enum {ok,internal}
% encode(get_request,	Key)			=> Key string
% encode(get_response,	Result) 		=> Result enum {ok,not_found,internal}
% encode(get_response,	Result,Key,Value)	=> Result enum {ok,not_found,internal} + Key:Value
%
% "decode" and "type" functions to get row Google Protocol Buffer data and decode it
% decode(Data) 					=> universal decoder, returns decoded structure
% decode(set_request,	Data)			=> returns {Key,Value} tulip
% decode(set_response,	Data)			=> returns Result enum {ok, internal}
% decode(get_request,	Data)			=> returns Key string
% decode(get_response,	Data)			=> returns {Result} or {Result,Key,Value} where Result enum {ok,not_found,internal}
%
% type(Data)					=> type of the data enum {set_requestset_response,get_request,get_response}
% =================================================================================================================================

-export([
    encode/2,
    encode/3,
    encode/4,
    decode/1,
    decode/2,
    type/1
    ]).

% =========== IMPLEMENTATION =================
%
% Implementation is based on Protobuffs v0.8.2
% git://github.com/basho/erlang_protobuffs
%
% ============================================

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

% Universal decode function that returns decodes full message
%
% Example:
% 1> M = gpb:encode(set_request,"Key","Value").
% [[["\b",[1]],
%  [[18],
%   [14],
%   [[["\n","\f",
%      [[["\n",[3],<<"Key">>],[[18],[5],<<"Value">>]]]]]]],
%  [],[],[]]]
% 2> gpb:decode(M).
% {req_envelope,set_request_t,
%              {set_request,{data,"Key","Value"}},
%              undefined,undefined,undefined}

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
    } -> {Result};
    
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
    
% === typee function ===

% Returns one of the following types:
%	set_request
%	set_response
%	get_request
%	get_response
type(Data) ->
    {req_envelope,
	Type,_,_,_,_
    }	= kv_pb:decode(
		    req_envelope,
		    list_to_binary(Data)
		),
    case Type of
	set_request_t 	-> set_request;
	set_response_t	-> set_response;
	get_request_t	-> get_request;
	get_response_t	-> get_response
    end.
