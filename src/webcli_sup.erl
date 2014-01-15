%% @author Zenk Ju<juzejian@gmail.com>
%% @copyright webcli Zenk <juzejian@gmail.com>

%% @doc Supervisor for the webcli application.

-module(webcli_sup).
-author("Zenk Ju <juzejian@gmail.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {ok, Port} = application:get_env(port),
    Web = web_specs(webcli_web, Port),
    Processes = [Web],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, webcli_deps:local_path(["priv", "www"])}
%%                 {docroot, webcli_deps:local_path(["priv", "www"])},
%%                 {ssl, true},
%%                 {ssl_opts, [
%%                    {certfile, webcli_deps:local_path(["priv", "ssl", "10.85.151.63.crt"])},
%%                    {keyfile, webcli_deps:local_path(["priv", "ssl", "10.85.151.63.key"])}
%%                    ]}
                ],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
