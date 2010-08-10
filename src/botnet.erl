-module(botnet).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([add_host/1]).
-export([lock_node/0, unlock_node/1]).

-record(state, {available_nodes=queue:new()}).

%% API functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_host(Local) when Local == <<"127.0.0.1">>; Local == <<"localhost">> ->
    gen_server:call(?MODULE, {add_node, node()});

add_host(Ip) when is_binary(Ip) ->
    {ok, Addr} = inet:getaddr(binary_to_list(Ip), inet),
    Host = "asdf",
    inet_db:add_host(Addr, [Host]),
    case slave:start_link(Host) of
        {ok, Node} -> gen_server:call(?MODULE, {add_node, Node});
        {error, Reason} -> {error, Reason}
    end.

lock_node() ->
    gen_server:call(?MODULE, lock_node).

unlock_node(Node) ->
    gen_server:cast(?MODULE, {unlock_node, Node}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------
handle_call({add_node, Node}, _From, #state{available_nodes=Available}=State) ->
    Available1 = queue:in(Node, Available),
    {reply, ok, State#state{available_nodes=Available1}};

handle_call({remove_node, Node}, _From, #state{available_nodes=Available}=State) ->
    Available1 = queue:filter(fun(Item) -> Node =/= Item end, Available),
    {reply, ok, State#state{available_nodes=Available1}};

handle_call(lock_node, _From, #state{available_nodes=Available}=State) ->
    case queue:out(Available) of
        {{value, Node}, Available1} ->
            {reply, Node, State#state{available_nodes=Available1}};
        {empty, _} ->
            {reply, undefined, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({unlock_node, Node}, #state{available_nodes=Available}=State) ->
    Available1 = queue:in(Node, Available),
    {noreply, State#state{available_nodes=Available1}};
    
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> 
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
