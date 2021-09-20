%%%-------------------------------------------------------------------
%% @doc wx_cfg_library public API
%% @end
%%%-------------------------------------------------------------------

-module(wx_cfg_library_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    wx_cfg_library_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
