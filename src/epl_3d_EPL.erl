%%
%% %CopyrightBegin%
%%
%% Copyright Michal Slaski 2013. All Rights Reserved.
%%
%% %CopyrightEnd%
%%
-module(epl_3d_EPL).
-behaviour(cowboy_websocket_handler).

%% EPL plugin callback
-export([start_link/1]).

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

%%%===================================================================
%%% cowboy_websocket_handler callbacks
%%%===================================================================
init({tcp, http}, _Req, _Opts) ->
    epl_3d:subscribe(),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({data, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    epl_3d:unsubscribe(),
    ok.
