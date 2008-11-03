%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the tailor application.

-module(tailor_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for tailor.
start(_Type, _StartArgs) ->
    tailor_deps:ensure(),
    tailor_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for tailor.
stop(_State) ->
    ok.
