-module(scratch).

-compile(export_all).


check1() ->
        {ok, C} = clustmea_conf:new(),
        ok = clustmea_conf:set(C, [{uploader_conf, {1, 5, 100, xx_connection}},
                                   {iteration, soft}]),
        {ok, T} = clustmea_task:new(my_uploader),
        clustmea_task:start_task(T,C),
        clustmea_task:stop_task(T).
