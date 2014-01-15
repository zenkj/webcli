-module(webcli_session_mgr).

-behavior(gen_server).

-export([start_link/1, get_sessions/0, get_session/1, add_session/1, del_session/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 60000). %% 1 minute

-record(state, {
                ppid,   %% parent process id
                s2pmap, %% session id to session process id map
                p2smap, %% session process id to session id map
                tref
               }).

start_link(Ppid) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Ppid}, []).

get_sessions() ->
    gen_server:call(?SERVER, get_sessions).
get_session(Sid) ->
    gen_server:call(?SERVER, {get_session, Sid}).
add_session(Sid) ->
    gen_server:call(?SERVER, {add_session, Sid}).
del_session(Sid) ->
    gen_server:call(?SERVER, {del_session, Sid}).


init({Ppid}) ->
    process_flag(trap_exit, true),
    {ok, Tref} = timer:send_after(?TIMEOUT, timeout),
    {ok, #state{ppid=Ppid,
                tref=Tref,
                s2pmap=dict:new(),
                p2smap=dict:new()}}.

handle_call(get_sessions, _From, #state{s2pmap=S2pmap}=State) ->
    {reply, dict:to_list(S2pmap), State};
handle_call({get_session, Sid}, _From, #state{s2pmap=S2pmap}=State) ->
    case dict:find(Sid, S2pmap) of
        error -> {reply, undefined, State};
        {ok, [Pid]}  -> {reply, Pid, State}
    end;
handle_call({del_session, Sid}, _From, State) ->
    State1 = del_session_of_sid(Sid, State),
    {reply, ok, State1};
handle_call({add_session, Sid}, _From, #state{s2pmap=S2pmap, p2smap=P2smap} = State) ->
    error = dict:find(Sid, S2pmap),
    {ok, Pid} = webcli_session:start_link(Sid, self()),
    State1 = State#state{s2pmap=dict:append(Sid, Pid, S2pmap),
                p2smap=dict:append(Pid, Sid, P2smap)},
    {reply, Pid, State1}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(timeout, #state{p2smap=P2smap}=State) ->
    Spids = dict:fetch_keys(P2smap),
    [webcli_session:age(Spid) || Spid <- Spids],
    {ok, Tref} = timer:send_after(?TIMEOUT, timeout),
    {noreply, State#state{tref=Tref}};
handle_info({'EXIT', Ppid, _Reason}, #state{ppid=Ppid}=State) ->
    {stop, State};
handle_info({'EXIT', From, _Reason}, State) ->
    {noreply, del_session_of_pid(From, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

del_session_of_pid(Pid, #state{p2smap=P2smap, s2pmap=S2pmap} = State) ->
    case dict:find(Pid, P2smap) of
        {ok, [Sid]} ->
            State#state{p2smap=dict:erase(Pid, P2smap), s2pmap=dict:erase(Sid, S2pmap)};
        error ->
            State
    end.

del_session_of_sid(Sid, #state{p2smap=P2smap, s2pmap=S2pmap} = State) ->
    case dict:find(Sid, S2pmap) of
        {ok, [Pid]} ->
            State#state{p2smap=dict:erase(Pid, P2smap), s2pmap=dict:erase(Sid, S2pmap)};
        error ->
            State
    end.
