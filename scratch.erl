-module(scratch).

-compile(export_all).

-include("uploader_conf.hrl").

check_my_uploader() ->
    run(worker, my_uploader,
        on_conf, [{uploader_conf,
                   #uploader_conf{seed=1,
                                  value_size=10,
                                  quantity=200,
                                  connection=xx_connection}},
                  {iteration, soft}],
       and_stop_ASAP).

check_riak_inets() ->
    run(worker, riak_inets_uploader,
        on_conf, [{uploader_conf,
                   #uploader_conf{seed=1,
                                  value_size=10,
                                  quantity=20,
                                  connection=xx_connection}},
                  {iteration, soft}]).

%%
%% Internal functions
%%

run(worker, Worker, on_conf, Conf) ->
    {ok,C} = clustmea_conf:new(),
    ok = clustmea_conf:set(C, Conf),
    {ok,T} = clustmea_task:new(Worker),
    clustmea_task:start_task(T, C),
    {ok,T}.

run(worker, Worker, on_conf, Conf, and_stop_ASAP) ->
    {ok,T} = run(worker, Worker, on_conf, Conf),
    clustmea_task:stop_task(T).
