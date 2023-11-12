%%%-------------------------------------------------------------------
%% @doc copy_node_labels public API
%% @end
%%%-------------------------------------------------------------------

-module(copy_node_labels_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    copy_node_labels_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
