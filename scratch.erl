-module(scratch).

-compile(export_all).

-include("uploader_conf.hrl").

check1() ->
    {ok, C} = clustmea_conf:new(),
    ok = clustmea_conf:set(C, [{uploader_conf,
                                #uploader_conf{seed=1,
                                               value_size=5,
                                               quantity=100,
                                               connection=xx_connection}},
                               {iteration, hard}]),
    {ok, T} = clustmea_task:new(my_uploader),
    clustmea_task:start_task(T,C),
    clustmea_task:stop_task(T).
