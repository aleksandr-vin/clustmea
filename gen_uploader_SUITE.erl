%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012
%%% @doc
%%% Test suite for gen_uploader behaviour.
%%% @end
%%% Created : 13 Mar 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_uploader_SUITE).

-compile(export_all).

%%%===================================================================
%%% gen_uploader callback module definitions start
%%%===================================================================

-behaviour(gen_uploader).

%% API
-export([make_producer/3, make_uploader/1]).

%%%===================================================================
%%% gen_uploader callback module definitions end
%%%===================================================================

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [simple_run].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
simple_run() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
simple_run(_Config) ->
    Config = [{upploader_conf, {1, 2, 3, fake_connection}}],
    {ok, Pid} = start_link(Config),
    ok = gen_uploader:run(Pid),
    ok = gen_uploader:stop(Pid).


%%%===================================================================
%%% gen_uploader callback module implementation
%%%===================================================================

-define(make_server_name, misc:make_ref_atom(?MODULE)).

%%%
%%%  Public API
%%%

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Config) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_uploader:start_link({local, ?make_server_name}, ?MODULE, Config, []).


%%%===================================================================
%%% gen_uploader callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns the Key-Value producer function
%%
%% @spec make_producer(Seed, Quantity, ValueSize) ->
%%                                 function/1 | {error, Error}
%% @end
%%--------------------------------------------------------------------
make_producer(Seed, Quantity, ValueSize) ->
    clustmea_worker:make_succeeding_kv_producer(Seed, Quantity, ValueSize).

%%--------------------------------------------------------------------
%% @doc
%% Returns a function to perform actual Key-Value uploads
%%
%% @spec make_uploader(Connection) -> function/2 | {error, Error}
%% @end
%%--------------------------------------------------------------------
make_uploader(Connection) ->
    clustmea_worker:curl_uploader(Connection).
