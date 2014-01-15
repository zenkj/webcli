-module(webcli_session).

-behavior(gen_server).

-export([start_link/2, stop/1, subscribe/1, unsubscribe/1, get_clisessions/1, get_clisession/2, add_clisession/1, del_clisession/2, add_response/2, age/1, clicmd/3, stdin/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAXMSG, 10).
-define(MAXAGE, 30). %% close the session after idle for 30 minutes
-record(state, {
                  sid, %% session id
                  ppid, %% session manager process id
                  client_pid, %% web client pid
                  next_csid, %% next cli session's id
                  age,       %% idle age
                  cs2pmap, %% cli session id to cli session process id map
                  p2csmap, %% cli session process id to to cli session id map
                  output   %% stdout and stderr of all cli session in this web session
                 }).

start_link(SID, Parent) ->
    gen_server:start_link(?MODULE, {SID, Parent}, []).

stop(Spid) ->
    gen_server:cast(Spid, stop).
subscribe(Spid) ->
    case process_info(Spid, status) of
        undefined -> ok;
        _ -> gen_server:call(Spid, {subscribe, self()})
    end.
unsubscribe(Spid) ->
    case process_info(Spid, status) of
        undefined -> ok;
        _ -> gen_server:call(Spid, {unsubscribe, self()})
    end.
get_clisession(Spid, Csid) ->
    gen_server:call(Spid, {get_clisession, Csid}).
get_clisessions(Spid) ->
    gen_server:call(Spid, get_clisessions).
add_clisession(Spid) ->
    gen_server:call(Spid, add_clisession).
del_clisession(Spid, Csid) ->
    gen_server:call(Spid, {del_clisession, Csid}).
add_response(Spid, Data) ->
    gen_server:cast(Spid, {add_response, Data}).
age(Spid) ->
    gen_server:cast(Spid, age).
clicmd(Spid, Csid, Cmd) ->
    gen_server:call(Spid, {clicmd, Csid, Cmd}).
stdin(Spid, Csid, Stdin) ->
    gen_server:call(Spid, {stdin, Csid, Stdin}).

init({SID, Parent}) ->
    process_flag(trap_exit, true),
    {ok, #state{sid=SID,
                ppid=Parent,
                client_pid=undefined,
                next_csid=1,
                age=0,
                cs2pmap=dict:new(),
                p2csmap=dict:new(),
                output=queue:new()}}.

handle_call({clicmd, _Csid, _Cmd}, _From, State) ->
    {reply, ok, State#state{age=0}};
handle_call({stdin, _Csid, _Stdin}, _From, State) ->
    {reply, ok, State#state{age=0}};
handle_call({subscribe, Cpid}, _From, State) ->
    State1 = State#state{client_pid=Cpid},
    {reply, ok, maybe_send_msg(State1)};
handle_call({unsubscribe, _Cpid}, _From, State) ->
    {reply, ok, State#state{client_pid=undefined}};
handle_call(get_clisessions, _From, #state{cs2pmap=Cs2pmap} = State) ->
    {reply, dict:fetch_keys(Cs2pmap), State};
handle_call({get_clisession, Csid}, _From, #state{cs2pmap=Cs2pmap} = State) ->
    case dict:find(Csid, Cs2pmap) of
        error -> {reply, undefined, State};
        {ok, [Pid]} -> {reply, Pid, State}
    end;
handle_call({del_clisession, Csid}, _From, State) ->
    #state{cs2pmap=Cs2pmap} = State1 = del_clisession_of_csid(Csid, State),
    case dict:size(Cs2pmap) of
        0 -> {stop, "no cli session left", State1};
        _ -> {reply, ok, State1}
    end;
handle_call(add_clisession, _From, #state{next_csid=NextCsid, cs2pmap=Cs2pmap, p2csmap=P2csmap} = State) ->
    {ok, Pid} = webcli_clisession:start_link(NextCsid, self()),
    State1 = State#state{next_csid=NextCsid+1,
                    cs2pmap=dict:append(NextCsid, Pid, Cs2pmap),
                    p2csmap=dict:append(Pid, NextCsid, P2csmap)},
    {reply, NextCsid, State1}.

handle_cast(stop, State) ->
    {stop, State};
handle_cast({add_response, Data}, #state{output=Output}=State) ->
    State1 = State#state{output=queue:in(Data, Output)},
    {noreply, maybe_send_msg(State1)};
handle_cast(age, #state{age=Age}=State) ->
    Age1 = Age+1,
    case Age1 > ?MAXAGE of
        true ->
            {stop, State};
        false ->
            {noreply, State#state{age=Age1}}
    end;
handle_cast(_Cast, State) ->
    {noreply, State}.

maybe_send_msg(#state{client_pid=undefined}=State) ->
    State;
maybe_send_msg(#state{client_pid=Cpid, output=Output}=State) ->
    case queue:len(Output) of
        0 -> State;
        N when N < ?MAXMSG ->
            List = queue:to_list(Output),
            Cpid ! {send_msg, List},
            State#state{client_pid=undefined, output=queue:new()};
        _N ->
            {Q1, Q2} = queue:split(?MAXMSG, Output),
            Cpid ! {send_msg, queue:to_list(Q1)},
            State#state{client_pid=undefined, output=Q2}
    end.

handle_info({'EXIT', Ppid, _Reason}, #state{ppid=Ppid}=State) ->
    {stop, State};
handle_info({'EXIT', From, _Reason}, State) ->
    #state{cs2pmap=Cs2pmap} = State1 = del_clisession_of_pid(From, State),
    case dict:size(Cs2pmap) of
        0 -> {stop, "no cli session left", State1};
        _ -> {noreply, State1}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

del_clisession_of_csid(Csid, #state{cs2pmap=Cs2pMap, p2csmap=P2csMap} = State) ->
    case dict:find(Csid, Cs2pMap) of
        {ok, [Pid]} ->
            State#state{cs2pmap=dict:erase(Csid, Cs2pMap), p2csmap=dict:erase(Pid, P2csMap)};
        error ->
            State
    end.

del_clisession_of_pid(Pid, #state{cs2pmap=Cs2pMap, p2csmap=P2csMap} = State) ->
    case dict:find(Pid, P2csMap) of
        {ok, [Csid]} ->
            State#state{cs2pmap=dict:erase(Csid, Cs2pMap), p2csmap=dict:erase(Pid, P2csMap)};
        error ->
            State
    end.
