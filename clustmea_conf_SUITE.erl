-module(clustmea_conf_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

groups() ->
    [{implementation_group, [parallel],
      [reset_options,set_options]},
     {simple_use_cases_group, [sequence],
      [list_configs,get_config_options]},
     {parallel_group, [parallel],
      [{group,implementation_group},{group,simple_use_cases_group}]}].

all() ->
    [{group,parallel_group}].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

reset_options(_Config) ->
    Data = [{[[],[a]],       '=>', []},
            {[[{a,1}],[a]],  '=>', []},
            {[[],[]],        '=>', []},
            {[[{a,1},{b,2}],[a,b]], '=>', []},
            {[[{a,1},{b,2}],[b]], '=>', [{a,1}]},
            {[[{a,1},{b,2}],[]], '=>', [{a,1},{b,2}]}],
    check_fun_on_data(clustmea_conf, reset_options, Data).

set_options(_Config) ->
    Data = [{[[],[{a,1}]],       '=>', [{a,1}]},
            {[[],[{b,1},{c,2}]], '=>', [{b,1},{c,2}]},
            {[[],[]],            '=>', []},
            {[[{a,1},{b,2}],[{b,3}]], '=>', [{a,1},{b,3}]}],
    check_fun_on_data(clustmea_conf, set_options, Data).

list_configs(_Config) ->
    CC = clustmea_conf,
    {ok, _Pid} = CC:start_link(),
    {ok, []} = CC:list(),
    {ok, Cid} = CC:new(),
    {ok, [Cid]} = CC:list(),
    ok = CC:delete(Cid),
    {ok, []} = CC:list().

get_config_options(_Config) ->
    CC = clustmea_conf,
    {ok, _Pid} = CC:start_link(),
    {ok, Cid} = CC:new(),
    {ok, []} = CC:get(Cid),
    ok = CC:set(Cid, [{a,1}]),
    {ok, [{a,1}]} = CC:get(Cid),
    ok = CC:reset(Cid, [a]),
    {ok, []} = CC:get(Cid),
    ok = CC:set(Cid, [{a,2}, {b,b1}]),
    ok = CC:set(Cid, [{a,3}]),
    case CC:get(Cid) of
        {ok, [{a,3}, {b,b1}]} -> ok_1;
        {ok, [{b,b1}, {a,3}]} -> ok_2
    end.

%%--------------------------------------------------------------------
%% HELPER FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Check function across data
%%
%% @spec check_fun_on_data(M, F, InOutList) -> [M:F(InOut)]
%%         InOutList -> [InOut]
%%         InOut -> {In,'=>',Out}
%%         In -> list()
%%         Out -> term()
%% @end
%%--------------------------------------------------------------------
check_fun_on_data(documentation) ->
    "Checks function M:F on Data."
        "InOutList -> [InOut]"
        "InOut -> {In,'=>',Out}"
        "In -> list()"
        "Out -> term()".
check_fun_on_data(M, F, InOutList) ->
    lists:map(fun ({In,'=>',Out}) ->
                      Out = apply(M, F, In)
              end,
              InOutList).

