%%%-------------------------------------------------------------------
%% @doc ucl public API
%% @end
%%%-------------------------------------------------------------------

-module(ucl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ucl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
