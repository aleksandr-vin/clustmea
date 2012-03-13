%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012
%%% @doc
%%% Clustmea Task Server
%%% @end
%%% Created : 1 Mar 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(clustmea_task).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([new/1, delete/1, list/0]).
-export([start_task/2, stop_task/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,  {tasks}).

-record(new,    {module}).
-record(delete, {tid}).
-record(list,   {}).

-record(start,  {tid,cid}).
-record(stop,   {tid}).

-record(task,   {status,module,pid}).

-define(CONFIG,   clustmea_conf).
-define(REPORTER, clustmea_reporter).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new task.
%%
%% @spec new(Module) -> {ok, Tid} | {error, Why}
%%         Module -> atom()
%% @end
%%--------------------------------------------------------------------
new(Module) when is_atom(Module) ->
    gen_server:call(?SERVER, #new{module=Module}).

%%--------------------------------------------------------------------
%% @doc
%% Deletes the task.
%%
%% @spec delete(Tid) -> ok | {error, Why}
%% @end
%%--------------------------------------------------------------------
delete(Tid) ->
    gen_server:cast(?SERVER, #delete{tid=Tid}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of available tasks.
%%
%% @spec list() -> {ok, TidList} | {error, Why}
%% @end
%%--------------------------------------------------------------------
list() ->
    gen_server:call(?SERVER, #list{}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the task with the configuration.
%%
%% @spec start(Tid, Cid) -> ok | {error, Why}
%% @end
%%--------------------------------------------------------------------
start_task(Tid, Cid) ->
    gen_server:call(?SERVER, #start{tid=Tid, cid=Cid}).

%%--------------------------------------------------------------------
%% @doc
%% Stops the task.
%%
%% @spec stop(Tid) -> ok | {error, Why}
%% @end
%%--------------------------------------------------------------------
stop_task(Tid) ->
    gen_server:cast(?SERVER, #stop{tid=Tid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Tasks = dict:new(),
    {ok, #state{tasks=Tasks}}.

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
handle_call(#new{module=Module}, _From, State) ->
    {state, Tasks1} = State,
    Tid = make_ref(),
    NewTask = #task{status=new, module=Module},
    Tasks2 = dict:store(Tid, NewTask, Tasks1),
    NewState = State#state{tasks=Tasks2},
    Reply = {ok, Tid},
    {reply, Reply, NewState};

handle_call(#list{}, _From, State) ->
    #state{tasks=Tasks} = State,
    Tids = dict:fetch_keys(Tasks),
    Reply = {ok, Tids},
    {reply, Reply, State};

handle_call(#start{tid=Tid, cid=Cid}, _From, State1) ->
    #state{tasks=Tasks1} = State1,
    {ok, Config} = ?CONFIG:get(Cid),
    Tasks2 = dict:update(Tid,
                         fun (Task) ->
                                 NewTask = start_the_task(Config, Task),
                                 ok = ?REPORTER:task_started(Tid, Task, Config),
                                 NewTask
                         end,
                         Tasks1),
    Reply = ok,
    State2 = State1#state{tasks=Tasks2},
    {reply, Reply, State2}.


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
handle_cast(#delete{tid=Tid}, State) ->
    {state, Tasks1} = State,
    Tasks2 = dict:erase(Tid, Tasks1),
    NewState = State#state{tasks=Tasks2},
    {noreply, NewState};

handle_cast(#stop{tid=Tid}, State1) ->
    #state{tasks=Tasks1} = State1,
    Tasks2 = dict:update(Tid,
                         fun (Task) ->
                                 NewTask = stop_the_task(Task),
                                 ok = ?REPORTER:task_stopped(Tid, directly_by_task_server),
                                 NewTask
                         end,
                         Tasks1),
    State2 = State1#state{tasks=Tasks2},
    {noreply, State2}.


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
handle_info(_Info, State) ->
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the Task operation on the Config.
%% Return a new task record indicating that Task was started.
%%
%% @spec start_the_task(Config, Task) -> NewTask
%% @end
%%--------------------------------------------------------------------
start_the_task(Config, Task = #task{status=Status}) when Status =/= running ->
    %%
    %% Here we must start the task and get the ack!
    %%
    Module = Task#task.module,
    {ok, Pid} = Module:start_child(Config),
    ok = gen_uploader:run(Pid),
    %% Now we update the status
    NewTask = Task#task{status=running, pid=Pid},
    NewTask.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops the Task operation.
%% Return a new task record indicating that Task was stoppped.
%%
%% @spec stop_the_task(Task) -> NewTask
%% @end
%%--------------------------------------------------------------------
stop_the_task(Task = #task{status=Status}) when Status =:= running ->
    %%
    %% Here we must stop the task and get the ack!
    %%
    Pid = Task#task.pid,
    ok = gen_uploader:stop(Pid),
    NewTask = Task#task{status=stopping},
    NewTask.
