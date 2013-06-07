%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_3d).
-behaviour(gen_server).

%% API
-export([start_link/0,
         subscribe/0,
         unsubscribe/0,
         trace_pid/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {subscribers = []}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

unsubscribe() ->
    gen_server:cast(?MODULE, {unsubscribe, self()}).

trace_pid(Pid) ->
    gen_server:call(?MODULE, {trace_pid, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(no_args) ->
    ok = epl:subscribe(),
    {ok, #state{}}.

handle_call({trace_pid, Pid}, _From, State) ->
    ok = epl:trace_pid(Pid),
    {ok, ProcessInfo} = epl:process_info(Pid),
    {reply, {ok, ProcessInfo}, State};
handle_call(Request, _From, _State) ->
    exit({not_implemented, Request}).

handle_cast({subscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = [Pid|Subs]}};
handle_cast({unsubscribe, Pid}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};
handle_cast(Request, _State) ->
    exit({not_implemented, Request}).


handle_info({data, {N, T}, Proplist}, State = #state{subscribers = Subs}) ->
    %%  {send,[{{#Port<5984.431>,<5984.28.0>},0,8},
    %%         {{<5984.28.0>,<5984.30.0>},2,6}]},
    %%  {send_self,[]},
    %%  {receive,[{<5984.28.0>,8,314},
    %%             {<5984.30.0>,2,24}]}]

    Id = << (epl:to_bin(N))/binary, $:, (epl:timestamp(T))/binary >>,
    Proplist1 = [{id, Id} | Proplist],
    Proplist2 =
        lists:map(fun({spawn, Spawn}) ->
                          %% Example of spawn trace
                          %% {<13104.1201.0>,{erlang,apply,[m,f,[a]]},TS}
                          {spawn,
                           [[{<<"id">>, epl:to_bin(P)},
                             {<<"m">>, epl:to_bin(M)},
                             {<<"f">>, epl:to_bin(F)},
                             {<<"t">>, epl:timestamp(TS)}]
                            || {P, {M, F, _A}, TS} <- Spawn]};
                     ({exit, Exit}) ->
                          %% Example of exit trace
                          %% {<13104.1201.0>,normal,{1370,391207,93745}}
                          {exit,
                           [[{<<"id">>, epl:to_bin(P)},
                             {<<"r">>, reason(R)},
                             {<<"t">>, epl:timestamp(TS)}]
                            || {P, R, TS} <- Exit]};
                     ({send, Send}) ->
                          %% Examples of send trace:
                          %% {{global_name_server,<13104.13.0>},0,1}
                          %% {#Port<13104.431>,<13104.28.0>},0,72}
                          %% {{<13104.12.0>,{alias,'erlangpl@127.0.0.1'}},2,0}
                          {send,
                           [[{<<"v1">>, epl:to_bin(P1)},
                             {<<"v2">>, epl:to_bin(P2)},
                             {<<"c1">>, epl:to_bin(Count1)},
                             {<<"c2">>, epl:to_bin(Count2)}]
                            || {{P1, P2}, Count1, Count2} <- Send,
                               not is_tuple(P1), not is_tuple(P2)]};
                     ({send_self, SendSelf}) ->
                          {send_self,
                           [[{<<"id">>, epl:to_bin(P)},
                             {<<"c">>, epl:to_bin(Count)}]
                            || {P, Count} <- SendSelf]};
                     ({'receive', Receive}) ->
                          {'receive',
                           [[{<<"id">>, epl:to_bin(P)},
                             {<<"c">>, epl:to_bin(Count)},
                             {<<"s">>, epl:to_bin(Size)}]
                            || {P, Count, Size} <- Receive]};
                     ({trace, _Trace}) ->
                          %% TODO: process trace messages
                          {trace, []};
                     (Item) ->
                          Item
                  end,
                  Proplist1),

    JSON = ej:encode(Proplist2),

    [Pid ! {data, JSON} || Pid <- Subs],

    {noreply, State};
handle_info(Info, _State) ->
    exit({not_implemented, Info}).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reason(normal) ->
    <<"normal">>;
reason({ok, _Eval, _Arity}) ->
    %% 'ok' is returned from eval process spawned by shell process
    %% this is considered to be a normal exit
    <<"normal">>;
reason({Reason, _Stack}) ->
    epl:to_bin(io_lib:format("~w", [Reason]));
reason(Reason) when is_atom(Reason) ->
    epl:to_bin(Reason);
reason(Reason) ->
    epl:to_bin(io_lib:format("~w", [Reason])).
