%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)
%%
%% Based on Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%%
%% See MIT-LICENSE for licensing information.

-module(protobuff_server).
-behaviour(gen_server).

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

%-include("kv_pb.hrl").

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

process_message(MsgData, State) ->

    case gpb:type(MsgData) of
	set_request -> 
	    {Key,Value}	= gpb:decode(set_request,MsgData),
	    Chipher = encryption:enc(Value),
	    case  Chipher of 
		{ok,Res} ->
		    Ser = term_to_binary(Res),
		    Hex = hex:bin_to_hexstr(Ser),
		    case erlcloud_ddb2:put_item(<<"data">>,[{<<"key">>,Key},{<<"value">>,Hex}],[]) of
			{ok,[]} ->
				Pkt = gpb:encode(set_response,ok);
			{error,_} ->
			        Pkt = gpb:encode(set_response,internal)
		    end;
		_Else ->
		    		Pkt = gpb:encode(set_response,internal)
	    end;
	get_request-> 
	    Key		= gpb:decode(get_request,MsgData),
	    case erlcloud_ddb2:get_item(<<"data">>,{<<"key">>,Key}) of
		{ok,[{<<"value">>,DynamoValue},{<<"key">>,DynamoKey}]} ->
			Hex = hex:hexstr_to_bin(binary_to_list(DynamoValue)),
			Ser = binary_to_term(Hex),
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
    gen_tcp:send(State#state.sock, Pkt),
    State.

