%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(tailor).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the tailor server.
start() ->
    tailor_deps:ensure(),
    ensure_started(crypto),
    application:start(tailor).

%% @spec stop() -> ok
%% @doc Stop the tailor server.
stop() ->
    Res = application:stop(tailor),
    application:stop(crypto),
    Res.
