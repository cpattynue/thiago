-module(thiago).

-behaviour(gen_server).

%% API
-export([start_link/0, create/1, update/1, get/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {path_to_save_files}).


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
    gen_server:call(?MODULE,{create_or_update, Args}).

%%--------------------------------------------------------------------
%% @doc Update binary file 
%%
%% @spec update( Args :: tuple() ) -> ok
%%--------------------------------------------------------------------
-spec update(Args :: tuple()) -> ok.
update(Args) ->
    gen_server:call(?MODULE, {create_or_update, Args}).

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

    % Get the path from our env 
    {ok, Path} = application:get_env(thiago, path),   
 
    {ok, #state{path_to_save_files=Path}}.

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
handle_call({create_or_update, {Name, Terms}}, _From, State=#state{path_to_save_files=Path}) ->  
    DBPath = Path ++ "/" ++ Name ++ ".db",
    {ok, Name} = dets:open_file(Name, [{file, DBPath},{type, duplicate_bag}]),
    ok = dets:insert( Name, Terms ),
    ok = dets:close( Name ),
    {reply, ok, State};
handle_call({get, {Name, Key}}, _From, State=#state{path_to_save_files=Path})  	     ->
    DBPath = Path ++ "/" ++ Name ++ ".db",
    {ok, Name}  = dets:open_file(Name,[{file, DBPath},{type, duplicate_bag}]),
    Found = dets:lookup(Name, Key),
    ok = dets:close( Name ),
    {reply, {ok, Found}, State};
handle_call({delete, {Name, Key}}, _From, State=#state{path_to_save_files=Path})  ->
    DBPath = Path ++ "/" ++ Name ++ ".db",
    {ok, Name}  = dets:open_file(Name,[{file, DBPath},{type, duplicate_bag}]),
    
    case Key of
        all ->  dets:delete_all_objects(Name);
        _   ->  dets:delete_object(Name, Key)
    end,
    {reply, ok, State}.

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
handle_info(_Info, State)  ->
    {noreply, State}.

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
