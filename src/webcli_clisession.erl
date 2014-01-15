-module(webcli_clisession).

-behavior(gen_server).

-export([start_link/2, stop/1, send/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
                  csid,     %% cli session id
                  ppid,     %% parent web session process id
                  shpid,    %% process id corresponding to the shell
                  shospid   %% os process id correponding to the shell
                 }).

start_link(Csid, Ppid) ->
    gen_server:start_link(?MODULE, {Csid, Ppid}, []).

stop(Cspid) ->
    gen_server:cast(Cspid, stop).

send(Cspid, Data) when is_binary(Data) ->
    gen_server:cast(Cspid, {send, Data}).

init({Csid, Ppid}) ->
    process_flag(trap_exit, true),
    {ok, Pid, Ospid} = exec:run_link("$SHELL", [stdin, stdout, stderr, pty, noecho]),
    {ok, #state{csid=Csid,
                ppid=Ppid,
                shpid=Pid,
                shospid=Ospid}}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, State};
handle_cast({send, Data}, #state{shospid=Ospid}=State) ->
    exec:send(Ospid, Data),
    {noreply, State};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({stdout, Ospid, Data}, #state{shospid=Ospid}=State) ->
    add_output({stdout, Data}, State),
    {noreply, State};
handle_info({stderr, Ospid, Data}, #state{shospid=Ospid}=State) ->
    add_output({stderr, Data}, State),
    {noreply, State};
handle_info({'EXIT', Ppid, _Reason}, #state{ppid=Ppid}=State) ->
    {stop, State};
handle_info({'EXIT', Shpid, _Reason}, #state{shpid=Shpid}=State) ->
    {stop, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_output({Cmd, Data}, #state{csid=Csid, ppid=Ppid}=_State) ->
    webcli_session:add_response(Ppid, {Csid,Cmd,Data}).
