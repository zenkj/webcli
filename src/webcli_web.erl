%% @author Zenk Ju<juzejian@gmail.com>
%% @copyright webcli Zenk <juzejian@gmail.com>

%% @doc Web server for webcli.

-module(webcli_web).
-author("Zenk Ju <juzejian@gmail.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    webcli_session_mgr:start_link(self()),
    webcli_guid:start_link(),
    exec:start([]),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, Docroot) ->
    P = Req:get(path),
    M = Req:get(method),
    try
        case {M, P} of
            {'GET',  "/static/" ++ RelPath}  -> static(Req, RelPath, Docroot);
            {'GET',  "/favicon.ico"}         -> favicon(Req, Docroot);
            {'GET',  "/"}                    -> mainpage(Req, Docroot);
            {'GET',  "/login"}               -> loginpage(Req, Docroot);
            {'POST', "/login"}               -> login(Req);
            {'POST', "/logout"}              -> logout(Req);
            {'GET',  "/clisessions"}         -> clisessions(Req);
            {'POST', "/clicmd"}              -> clicmd(Req);
            {'POST', "/stdin"}               -> stdin(Req);
            {'GET',  "/response"}            -> response(Req);
            {_, _}                           -> unsupported(M, P, Req)
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, P},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

unsupported(Method, Path, Req) ->
    io:format("Unsupported method: ~p, path: ~p~n", [Method, Path]),
    Req:respond({200, [{"Content-Type", "text/plain"}], <<"unsupported">>}).

favicon(Req, _Docroot) ->
    Req:respond({200, [], <<"favicon">>}).

static(Req, RelPath, Docroot) ->
    Req:serve_file(RelPath, Docroot).

mainpage(Req, Docroot) ->
    case session(Req) of
        undefined -> redirect(Req, "/login");
        _Session -> Req:serve_file("index.html", Docroot)
    end.

loginpage(Req, Docroot) ->
    case session(Req) of
        undefined -> Req:serve_file("login.html", Docroot);
        _Session -> redirect(Req, "/")
    end.

login(Req) ->
    Post = Req:parse_post(),
    UserId = proplists:get_value("userid", Post),
    Password = proplists:get_value("password", Post),
    {ok, U} = application:get_env(username),
    {ok, P} = application:get_env(password),
    case {UserId, encrypt(UserId, Password)} of
        {U, P} -> login0(Req);
        {_, _} -> redirect(Req, "/login")
    end.

encrypt(U, P) ->
    erlang:md5([U,P]).

login0(Req) ->
    Guid = webcli_guid:gen(),
    SessionId = webcli_guid:string(Guid, "wcli"),
    Spid = webcli_session_mgr:add_session(SessionId),
    webcli_session:add_clisession(Spid),
    redirect(Req, "/", [setsid(SessionId)]).

logout(Req) ->
    logout0(Req),
    redirect(Req, "/login").

logout0(Req) ->
    case session(Req) of
        undefined -> ok;
        Spid -> webcli_session:stop(Spid)
    end.

clisessions(Req) ->
    case session(Req) of
        undefined -> json(Req, [{result, false}]);
        Spid -> 
            Csids = webcli_session:get_clisessions(Spid),
            json(Req, [{result, true}, {csids, Csids}])
    end.

clicmd(Req) ->
    case session(Req) of
        undefined -> json(Req, [{result, false}, {message, 'not log in'}]);
        Spid ->
            Post = Req:parse_post(),
            Cmd = proplists:get_value("cmd", Post),
            CsidStr = proplists:get_value("csid", Post),
            {Csid, []} = string:to_integer(CsidStr),
            clicmd(Req, Spid, Csid, Cmd)
            %%json(Req, [{result, true}])
    end.

clicmd(Req, Spid, Csid, "close-clisession") ->
    webcli_session:del_clisession(Spid, Csid),
    json(Req, [{result, true}]);
clicmd(Req, Spid, _Csid, "open-clisession") ->
    NewCsid = webcli_session:add_clisession(Spid),
    json(Req, [{result, true}, {csid, NewCsid}]).

stdin(Req) ->
    case session(Req) of
        undefined -> json(Req, [{result, false}, {message, 'not log in'}]);
        Spid ->
            Post = Req:parse_post(),
            Stdin = proplists:get_value("stdin", Post),
            CsidStr = proplists:get_value("csid", Post),
            {Csid, []} = string:to_integer(CsidStr),
            webcli_session:stdin(Spid, Csid, Stdin),
            case webcli_session:get_clisession(Spid, Csid) of
                undefined ->json(Req, [{result, false}, {message, "invalid csid"}]);
                Cspid -> webcli_clisession:send(Cspid, list_to_binary(Stdin)),
                         json(Req, [{result, true}])
            end
    end.

response(Req) ->
    case session(Req) of
        undefined -> json(Req, [{result, false}, {message, 'not login'}]);
        Spid ->
            ok = webcli_session:subscribe(Spid),
            receive
                {send_msg, Msgs} ->
                    Ms = [[{csid, Csid}, {type, Type}, {data, Data}] || {Csid, Type, Data} <- Msgs],
                    json(Req, [{result, true}, {count, length(Ms)}, {msgs, Ms}])
            after 10000 ->
                webcli_session:unsubscribe(Spid),
                json(Req, [{result, true}, {count, 0}])
            end
    end.

sid(Req) ->
    Req:get_cookie_value("webcli_sid").

setsid(SID) ->
    mochiweb_cookies:cookie("webcli_sid", SID).
    
session(Req) ->
    case sid(Req) of
        undefined -> undefined;
        Sid when is_list(Sid) ->
            webcli_session_mgr:get_session(Sid);
        _ -> undefined
    end.

redirect(Req, Path) ->
    Req:respond({301, [{"Location", Path}], ""}).

redirect(Req, Path, ExtraHeaders) ->
    Req:respond({301, [{"Location", Path}|ExtraHeaders], ""}).

json(Req, Data) ->
    %%log("json: ~p~n", [Data]),
    Result = mochijson2:encode(Data),
    %%log("json: ~p~n", [Result]),
    Req:respond({200, [], Result}).

%%log(Fmt) ->
%%    io:format(Fmt).

%%log(Fmt, Vars) ->
%%    io:format(Fmt, Vars).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
