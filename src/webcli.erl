%% @author Zenk Ju<juzejian@gmail.com>
%% @copyright webcli Zenk <juzejian@gmail.com>

%% @doc webcli.

-module(webcli).
-author("Zenk Ju <juzejian@gmail.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the webcli server.
start() ->
    webcli_deps:ensure(),
    ensure_started(crypto),
    application:start(webcli).


%% @spec stop() -> ok
%% @doc Stop the webcli server.
stop() ->
    application:stop(webcli).
