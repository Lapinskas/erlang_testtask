%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)
%%
%% Based on Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%%
%% See MIT-LICENSE for licensing information.

-module(protobuff_server).
-behaviour(gen_server).

% ============  Protocol Buffer Server =================
%
% Listens for Google Protocol buffer messages
% Stores and retrieves Dynamo DB data based on messages
%
%  set_request  -> |
%                  | Protobuff Server | -> DB
%  set_response <- |
% 
%  get_request  -> |
%                  | Protobuff Server | <- DB
%  get_response <- |
%
% Main logic implemented in process_message
%
% ======================================================

-export([
         start_link/0,
         set_socket/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-record(state, { sock }).

%% ===================================================================
%% Public API
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_socket(Pid, Socket) ->
    gen_server:call(Pid, {set_socket, Socket}).

init([]) ->
    {ok, #state{}}.

handle_call({set_socket, Socket}, _From, State) ->
    inet:setopts(Socket, [{active, once}, {packet, 4}, {header, 1}]),
    {reply, ok, State#state{sock = Socket}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, Socket}, State=#state{sock=Socket}) ->
    {stop, normal, State};

handle_info({tcp_error, Socket, _Reason}, State=#state{sock=Socket}) ->
    {stop, normal, State};

handle_info({tcp, _Sock, MsgData}, State=#state{sock=Socket}) ->
    case process_message(MsgData, State) of
        {pause, NewState} ->
            ok;
        NewState ->
            inet:setopts(Socket, [{active, once}])
    end,
    {noreply, NewState};

handle_info({tcp, _Sock, _Data}, State) ->
    %% req =/= undefined: received a new request while another was in
    %% progress -> Error
    lager:error("Received a new PB socket request"
                " while another was in progress"),
    {stop, normal, State};

handle_info(_, State) -> % Ignore any late replies from gen_servers/messages from fsms
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Handle GPB Messages
%% ===================================================================

% Two types of message are expected:
% 	1. set_request to set data in DB
%		- the response is sent in set_response
%	2. get_request to get data from DB
%		- the response is sent in get_response

process_message(MsgData, State) ->

    case gpb:type(MsgData) of

	% Processing set_request
	set_request -> 
	
	    % Decoding Key:Value pair from Google Protocol Buffer message
	    {Key,Value}	= gpb:decode(set_request,MsgData),
	    
	    % Encoding Value using AWS
	    Chipher = encryption:enc(Value),
	    case  Chipher of 
		{ok,Res} ->
		
		    % Result is a list of encrypted chunk blocks of different size
		    % Serialize list before storing
		    Ser = term_to_binary(Res),
		    
		    % Convert binary representation to HEX string,
		    % as erlcloud_ddb2 has a bug in storing binary chars > 127
		    Hex = bin_to_hexstr(Ser),

		    % Finally, store data to AWS Dynamo DB.
		    % Success should result in set_response message "ok"
		    % Any error should result in set_response message with "internal" error
		    case erlcloud_ddb2:put_item(<<"data">>,[{<<"key">>,Key},{<<"value">>,Hex}],[]) of
			{ok,[]} ->
				Pkt = gpb:encode(set_response,ok);
			{error,_} ->
			        Pkt = gpb:encode(set_response,internal)
		    end;
		_Else ->
		    		Pkt = gpb:encode(set_response,internal)
	    end;
	
	% Processing get_request
	get_request-> 
	
	    % Decoding Key from Google Protocol Buffer message
	    Key		= gpb:decode(get_request,MsgData),
	    
	    % Search Dynamo DB by Key
	    case erlcloud_ddb2:get_item(<<"data">>,{<<"key">>,Key}) of
		
		% Value found
		{ok,[{<<"value">>,DynamoValue},{<<"key">>,DynamoKey}]} ->

			% First, convert HEX string to binary data
			Hex = hexstr_to_bin(binary_to_list(DynamoValue)),
			
			% Second, de-serialize data
			Ser = binary_to_term(Hex),
			
			% Finally, decrypt Data
			% Success should result in get_response message "ok" with Key:Value pair
			% Any error should result in set_response message with "internal" error
			case encryption:dec(Ser) of 
			    {ok,Dec} ->
				Pkt = gpb:encode(get_response,ok,[DynamoKey],[Dec]);
			    _Else ->
				Pkt = gpb:encode(get_response,internal)
			end;
		{ok,[]} ->
			Pkt = gpb:encode(get_response,not_found);
		{error,_} ->
			Pkt = gpb:encode(get_response,internal)
	    end
    end,

    % Send Google Protocol Buffer message over network
    gen_tcp:send(State#state.sock, Pkt),
    State.

% Helper Functions for binary <=> HEX string representation

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).
