%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012
%%% @doc
%%% Uploader worker
%%% @end
%%% Created : 12 Mar 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(my_uploader).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([run/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(make_server_name, misc:make_ref_atom(?MODULE)).

-record(state, {config}).

-record(execute, {state}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Config) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_server:start_link({local, ?make_server_name}, ?MODULE, Config, []).


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
execute(State) ->
    gen_server:cast(self(), #execute{state=State}).

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
init(Config) ->
    {ok, #state{config=Config}}.

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
    {upploader_conf, UpploaderConf} = lists:keyfind(upploader_conf, 1, TaskConf),
    {Seed, ValueSize, Quantity, Connection} = UpploaderConf,
    run(Seed, ValueSize, Quantity, Connection),
    {noreply, State};

handle_cast(#execute{state=IterationState}, State) ->
    ok = apply(fun casted_executor/4, IterationState),
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

run(Seed, ValueSize, Quantity, Connection) ->
    {KV_Producer, State0} =
        clustmea_worker:make_succeeding_kv_producer(Seed, Quantity, ValueSize),
    Uploader = clustmea_worker:curl_uploader(Connection),
    casted_executor(KV_Producer, Uploader, Quantity, State0).

%%
%% Recursive abstract "Upload" task executor
%%
recursive_executor(_,_,0,_) -> ok;

recursive_executor(KV_Producer, Uploader, Quantity, State1) ->
    {K,V, State2} = KV_Producer(State1),
    ok = Uploader(K,V),
    recursive_executor(KV_Producer, Uploader, Quantity-1, State2).

%%
%% Casted abstract "Upload" task executor
%%
casted_executor(_,_,0,_) -> ok;

casted_executor(KV_Producer, Uploader, Quantity, State1) ->
    {K,V, State2} = KV_Producer(State1),
    ok = Uploader(K,V),
    IterationState = [KV_Producer, Uploader, Quantity-1, State2],
    execute(IterationState).
