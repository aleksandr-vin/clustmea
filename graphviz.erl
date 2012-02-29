-module(graphviz).
-compile(export_all).

format(dot, Graph) ->
    io_lib:format("digraph process_map {~n"
                  "init [style=filled,color=\".7 .3 .9\"];~n", []) ++
        lists:foldr(fun ({A,B}, Acc) ->
                            Record = io_lib:format("~p -> ~p;~n", [A,B]),
                            Record ++ Acc
                    end,
                    "",
                    Graph) ++
        "}".
