%%
%% Copyright (C) 2014 
%% Authors: Patricia Nuñez <cpattynue@gmail.com>
%%          Jorge Garrido <zgbjgg@gmail.com>
%% All rights reserved.
%%
%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%%
-module(thiago).

-behaviour(gen_server).

%% API
-export([start_link/0, create/1, update/1, get/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {subscription=[], references=[]}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Create new binary file 
%%
%% @spec create( Args :: tuple() ) -> ok
%%--------------------------------------------------------------------
-spec create(Args :: tuple()) -> ok.
create(Args) ->
    gen_server:call(?MODULE,{create, Args}).


%%--------------------------------------------------------------------
%% @doc Update binary file 
%%
%% @spec update( Args :: tuple() ) -> ok
%%--------------------------------------------------------------------
-spec update(Args :: tuple()) -> ok.
update(Args) ->
    gen_server:call(?MODULE, {update, Args}).

%%--------------------------------------------------------------------
%% @doc Get binary file 
%%
%% @spec get( Args :: string()) -> ok
%%--------------------------------------------------------------------
-spec get( Args :: string()) -> ok.
get(Args) ->
    gen_server:call(?MODULE, {get, Args}).

%%--------------------------------------------------------------------
%% @doc Delete 
%%
%% @spec delete(Args :: tuple()) -> ok
%%--------------------------------------------------------------------
-spec delete(Args :: tuple()) -> ok.
delete(Args) ->
    gen_server:call(?MODULE, {delete, Args}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, Ref} = gnuart:subscribe(),
    {ok, #state{references = [Ref]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(open, _From, State)         ->
    ok = gnuart:open(),
    {reply, ok, State};
handle_call(close, _From, State)         ->
    ok = gnuart:close(),
    {reply, ok, State};
handle_call({send, Cmd}, _From, State)  ->
    {ok, flush} = gnuart:flush(Cmd),
    {reply, ok, State};
handle_call(subscribe, {Pid, _}, #state{subscription=S, references=Refs})  ->
    Reference = erlang:make_ref(),
    {reply, {ok, Reference}, #state{subscription=[ {Reference, Pid} | S], references=Refs}};
handle_call(unsubscribe, {Pid, _}, #state{subscription=S, references=Refs})  ->
    {reply, ok, #state{subscription=[ {Ref, SPid} || {Ref, SPid} <- S, Pid =/= SPid], references=Refs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({got, Ref, Got}, #state{subscription=S, references=[Ref]})  ->
    io:format("arygon nfc got: ~p \n", [Got]),
    GotValue = arygon_nfc_cmds:split(Got),
    io:format("arygon nfc got value: ~p \n", [GotValue]),
    [ Pid ! {nfc, Ref, GotValue} || {_, Pid} <- S ],  
    {noreply, #state{subscription=S, references=[Ref]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
