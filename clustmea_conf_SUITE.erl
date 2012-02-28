-module(clustmea_conf_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

groups() ->
    [{implementation_group, [parallel], [reset_options, set_options]}].

all() ->
    [{group,implementation_group}].

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
