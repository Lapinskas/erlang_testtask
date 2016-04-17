%% Copyright (c) 2016 Vladas Lapinskas (vlad.lapinskas@gmail.com)
%%
%% Based on Erlang RPC Demo
%% Copyright (c) 2011 Rusty Klophaus (@rustyio)
%%
%% See MIT-LICENSE for licensing information.

-module(test_task_app).

-behaviour(application).

%% Application callbacks
-export([
         start/2,
         stop/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    test_task_sup:start_link().

stop(_State) ->
    ok.
