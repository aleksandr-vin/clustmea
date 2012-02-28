%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012,
%%% @doc
%%% Clustmea Configuration Server
%%% @end
%%% Created : 25 Feb 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(clustmea_conf).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([new/0, delete/1, list/0]).
-export([set/2, reset/2, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Implementation functions -- exporting for tests
-export([reset_options/2, set_options/2]).

-define(SERVER, ?MODULE).

-record(state,  {configs}).

-record(new,    {}).
-record(delete, {cid}).
-record(list,   {}).

-record(set,    {cid, options}).
-record(reset,  {cid, options}).
-record(get,    {cid}).

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
%% Creates a new configuration.
%%
%% @spec new() -> {ok, Cid} | {error, Why}
%% @end
%%--------------------------------------------------------------------
new() ->
    gen_server:call(?SERVER, #new{}).

%%--------------------------------------------------------------------
%% @doc
%% Deletes the configuration.
%%
%% @spec delete(Cid) -> ok | {error, Why}
%% @end
%%--------------------------------------------------------------------
delete(Cid) ->
    gen_server:cast(?SERVER, #delete{cid=Cid}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of available configurations.
%%
%% @spec list() -> {ok, CidList} | {error, Why}
%% @end
%%--------------------------------------------------------------------
list() ->
    gen_server:call(?SERVER, #list{}).

%%--------------------------------------------------------------------
%% @doc
%% Sets option(s) of the configuration.
%%
%% @spec set(Cid, Options) -> ok | {error, Why}
%%         Options -> [{OptionName, Value}]
%% @end
%%--------------------------------------------------------------------
set(Cid, Options) ->
    gen_server:cast(?SERVER, #set{cid=Cid, options=Options}).

%%--------------------------------------------------------------------
%% @doc
%% Resets option(s) of the configuration.
%%
%% @spec reset(Cid, OptionNames) -> ok | {error, Why}
%% @end
%%--------------------------------------------------------------------
reset(Cid, Options) ->
    gen_server:cast(?SERVER, #reset{cid=Cid, options=Options}).

%%--------------------------------------------------------------------
%% @doc
%% Returns options of the configuration.
%%
%% @spec get(Cid) -> {ok, Options} | {error, Why}
%% @end
%%--------------------------------------------------------------------
get(Cid) ->
    gen_server:call(?SERVER, #get{cid=Cid}).

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
    Configs = dict:new(),
    {ok, #state{configs=Configs}}.

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
handle_call(#new{}, _From, State) ->
    {state, Configs1} = State,
    Cid = make_ref(),
    Configs2 = dict:store(Cid, [], Configs1),
    NewState = State#state{configs=Configs2},
    Reply = {ok, Cid},
    {reply, Reply, NewState};

handle_call(#list{}, _From, State) ->
    #state{configs=Configs} = State,
    Cids = dict:fetch_keys(Configs),
    Reply = {ok, Cids},
    {reply, Reply, State};

handle_call(#get{cid=Cid}, _From, State) ->
    #state{configs=Configs} = State,
    Options = dict:fetch(Cid, Configs),
    Reply = {ok, Options},
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
handle_cast(#delete{cid=Cid}, State) ->
    {state, Configs1} = State,
    Configs2 = dict:erase(Cid, Configs1),
    NewState = State#state{configs=Configs2},
    {noreply, NewState};

handle_cast(#set{cid=Cid, options=Opts}, State) ->
    {state, Configs1} = State,
    Configs2 = dict:update(Cid,
                           fun (Old) -> set_options(Old, Opts) end,
                           Configs1),
    NewState = State#state{configs=Configs2},
    {noreply, NewState};

handle_cast(#reset{cid=Cid, options=OptionNames}, State) ->
    {state, Configs1} = State,
    Configs2 = dict:update(Cid,
                           fun (Old) -> reset_options(Old, OptionNames) end,
                           Configs1),
    NewState = State#state{configs=Configs2},
    {noreply, NewState}.

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

reset_options(OptionList, OptionNames) ->
    lists:foldl(fun (OptionName, Acc) ->
                        lists:keydelete(OptionName, 1, Acc)
                end,
                OptionList,
                OptionNames).

set_options(OptionList, Options) ->
    lists:foldl(fun ({OptionName,_} = Option, Acc) ->
                        lists:keystore(OptionName, 1, Acc, Option)
                end,
                OptionList,
                Options).
