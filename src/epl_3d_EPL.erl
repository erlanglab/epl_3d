%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_3d_EPL).
-behaviour(cowboy_websocket_handler).

%% EPL plugin callbacks
-export([start_link/1,
         init/1]).

%% cowboy_websocket_handler callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%%%===================================================================
%%% EPL plugin callbacks
%%%===================================================================
start_link(_Options) ->
    {ok, spawn(fun() -> receive _ -> ok end end)}.

init(_Options) ->
    MenuItem = <<"<li class=\"glyphicons share_alt\">",
                 "<a href=\"/epl_3d/epl_3d.html\"><i></i><span>3D</span></a>",
                 "</li>">>,
    Author = <<"Erlang Lab">>,
    {ok, [{menu_item, MenuItem}, {author, Author}]}.

%%%===================================================================
%%% cowboy_websocket_handler callbacks
%%%===================================================================
init({tcp, http}, _Req, _Opts) ->
    epl_3d:subscribe(),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, Id}, Req, State) ->
    NodeId = binary_to_list(Id),
    case catch list_to_pid(NodeId) of
        {'EXIT',{badarg,_}} ->
            JSON = epl:proplist_to_json([{status,
                                          "Ports are not supported yet."}]),
            {reply, {text, JSON}, Req, State};
        Pid ->
            ProcessInfo = case epl_3d:trace_pid(Pid) of
                              {ok, undefined} ->
                                  [{pid, Pid},
                                   {status, exited}];
                              {ok, PI} ->
                                  [{pid, Pid} | PI]
                          end,
            JSON = epl:proplist_to_json(ProcessInfo),
            {reply, {text, JSON}, Req, State}
    end;
websocket_handle(Data, _Req, _State) ->
    exit({not_implemented, Data}).

websocket_info({data, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_info(Info, _Req, _State) ->
    exit({not_implemented, Info}).

websocket_terminate(_Reason, _Req, _State) ->
    epl_3d:unsubscribe(),
    ok.
