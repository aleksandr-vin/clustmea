-module(misc_SUITE).

-compile(export_all).

groups() ->
    [{g1, [parallel], [generate_check]}].

all() ->
    [{group, g1}].


generate_check(_Config) ->
    {[1,2,3,4],5} =
        misc:generate(fun (S) -> {S,S+1} end, 1, 4).
