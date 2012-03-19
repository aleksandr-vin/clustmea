%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012
%%% @doc
%%% Generic Uploader worker
%%%
%%% To create a custom uploader worker you should make a module
%%% and use gen_uploader behavior. Then you must export two API functions:
%%% make_producer/3 and make_uploader/1.
%%% @end
%%% Created : 13 Mar 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_uploader).

-behaviour(gen_server).

-callback make_producer(Seed :: term(), Quantity :: term(), ValueSize :: term()) ->
    ProducerFunction :: term().
-callback make_uploader(Connection :: term()) ->
    UploaderFunction :: term().

%% API
-export([start_link/4]).

-export([run/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



-record(state, {config,module}).

-record(iterate, {iteration_state}).

-include("uploader_conf.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% Name and Options are transparent arguments for gen_server:start_link/4
%%
%% @spec start_link(Name, Module, Args, Options) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Module, Args, Options) ->
    gen_server:start_link(Name, ?MODULE, {Module,Args}, Options).


%%
%%  Public API
%%
run(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, run).

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).


%%
%% Internal API
%%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Cast a message to oneself to perform a next step of the task iteration
%%
%% @spec iterate(State) -> ok
%% @end
%%--------------------------------------------------------------------
iterate(State) ->
    gen_server:cast(self(), #iterate{iteration_state=State}).

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
init({Module,Config}) ->
    {ok, #state{config=Config, module=Module}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(run, State) ->
    TaskConf = State#state.config,
    UploaderConf = get_uploader_config(TaskConf),
    IterationPolicy = get_policy(iteration, TaskConf, _DefaultPolicy = soft),
    Module = State#state.module,
    run(Module, IterationPolicy, UploaderConf),
    {noreply, State};

handle_cast(#iterate{iteration_state=IterationState}, State) ->
    ok = apply(fun casted_iterator/4, IterationState),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.


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


get_uploader_config(TaskConf) ->
    Name = uploader_conf,
    {Name, UploaderConf} = lists:keyfind(Name, 1, TaskConf),
    UploaderConf.


get_policy(Name, TaskConf, DefaultPolicy) ->
    is_policy(Name),
    case lists:keyfind(Name, 1, TaskConf) of
        false -> DefaultPolicy;
        {Name, Policy} -> Policy
    end.

is_policy(iteration) -> ok.

%%
%% Refactor it!
%%
run(Module, IterationPolicy, Conf) ->
    Seed       = Conf#uploader_conf.seed,
    ValueSize  = Conf#uploader_conf.value_size,
    Quantity   = Conf#uploader_conf.quantity,
    Connection = Conf#uploader_conf.connection,
    Iterator = get_iterator(IterationPolicy),
    {KV_Producer, State0} =
        Module:make_producer(Seed, Quantity, ValueSize),
    Uploader = Module:make_uploader(Connection),
    Iterator(KV_Producer, Uploader, Quantity, State0).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return iterator function (arity 4) for the specified policy
%%
%% @spec get_iterator(Policy) -> fun/4
%%        Policy -> soft | hard
%% @end
%%--------------------------------------------------------------------
get_iterator(soft) ->
    fun casted_iterator/4;
get_iterator(hard) ->
    fun recursive_iterator/4.


%%
%% Recursive abstract "Upload" task iterator
%%
%% Uses recursion to iterate task, thus not allowing stopping the task but killing it
%%
recursive_iterator(_,_,0,_) -> ok;

recursive_iterator(KV_Producer, Uploader, Quantity, State1) ->
    {K,V, State2} = KV_Producer(State1),
    ok = Uploader(K,V),
    recursive_iterator(KV_Producer, Uploader, Quantity-1, State2).

%%
%% Casted abstract "Upload" task iterator
%%
%% Uses self handle_cast for task iteration, allowing stopping the task softly
%%
casted_iterator(_,_,0,_) -> ok;

casted_iterator(KV_Producer, Uploader, Quantity, State1) ->
    {K,V, State2} = KV_Producer(State1),
    ok = Uploader(K,V),
    IterationState = [KV_Producer, Uploader, Quantity-1, State2],
    iterate(IterationState).
