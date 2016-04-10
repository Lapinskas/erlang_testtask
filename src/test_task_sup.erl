%% Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%% See MIT-LICENSE for licensing information.

-module(test_task_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    application:start(inets),
    application:start(sasl),
    application:start(os_mon),

    Children = [
        {protobuff_server_sup, {protobuff_server_sup, start_link, []},
            permanent, infinity, supervisor, [protobuff_server_sup]},
        {protobuff_server_listener, {protobuff_server_listener, start_link, []},
            permanent, 5000, worker, [protobuff_server_listener]}
    ],
    {ok, { {one_for_one, 5, 10}, Children}}.

dispatch() ->
    [
        {["sequence", n]}
    ].
