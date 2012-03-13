%%%-------------------------------------------------------------------
%%% @author Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%% @copyright (C) 2012
%%% @doc
%%% Uploader worker
%%% @end
%%% Created : 12 Mar 2012 by Aleksandr Vinokurov <aleksandr.vin@gmail.com>
%%%-------------------------------------------------------------------
-module(my_uploader).

-behaviour(gen_uploader).

-define(make_server_name, misc:make_ref_atom(?MODULE)).

%% API
-export([start_link/1]).

%% gen_uploader callbacks
-export([make_producer/3, make_uploader/1]).

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
