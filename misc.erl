-module(misc).
-compile(export_all).

-define(SV, clustmea_sup).


launchpad() ->
    register(launchpad, spawn(fun () ->
                                      {ok, Pid} = ?SV:start_link(),
                                      unlink(Pid)
                              end)).

