-module(clustmea).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    clustmea_sup:start_link().

stop(_State) ->
    ok.
