-module(clustmea).
-behaviour(application).

%% application behaviour callbacks
-export([start/2, stop/1]).

%% extra API
-export([restart/0]).

start(_Type, _Args) ->
    clustmea_sup:start_link().

stop(_State) ->
    ok.

restart() ->
    lists:map(
      fun (F) ->
              {F, application:F(?MODULE)}
      end,
      [stop, unload, start]).
