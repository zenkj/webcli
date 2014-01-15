%% @author Zenk Ju<juzejian@gmail.com>
%% @copyright webcli Zenk <juzejian@gmail.com>

%% @doc Callbacks for the webcli application.

-module(webcli_app).
-author("Zenk Ju <juzejian@gmail.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for webcli.
start(_Type, _StartArgs) ->
    webcli_deps:ensure(),
    webcli_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for webcli.
stop(_State) ->
    ok.
