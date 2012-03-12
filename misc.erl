-module(misc).
-compile(export_all).

-define(SV, clustmea_sup).


launchpad() ->
    register(launchpad, spawn(fun () ->
                                      {ok, Pid} = ?SV:start_link(),
                                      unlink(Pid)
                              end)).

crawl_registered(documentation) ->
    "Gets ancestors of a registered process (must conform OTP)."
        " And the ancestors of them recursively."
        " Starting from registered().".
crawl_registered() ->
    crawl_ancestors(registered(), []).


%% This code can be used to flatten the graph:
%% AGF = lists:flatmap(fun ({P,A}) -> lists:map(fun (E) -> {P,E} end, A) end, AG).

crawl_ancestors(documentation) ->
    "Gets ancestors of all the processes or a process (must conform OTP)."
        " And the ancestors of them recursively.".

crawl_ancestors(PNames, Links) when is_list(PNames) ->
    lists:foldl(fun misc:crawl_ancestors/2, Links, PNames);
crawl_ancestors(PName, Links) -> % when is_atom(PName) ->
    case proplists:is_defined(PName, Links) of
        true -> Links;
        _ ->
            io:format("PName: ~p~n", [PName]),
            Ancestors = get_ancestors(PName),
            Link = proplists:property(PName, Ancestors),
            crawl_ancestors(Ancestors, [Link | Links])
    end.

get_ancestors(documentation) ->
    "Returns ancestors of the process that conform to OTP.";
get_ancestors(PName) when is_pid(PName) -> [];
get_ancestors(PName) when is_atom(PName) ->
    case catch sys:get_status(PName) of
        {status,_,_,Status} ->
            Ancestors = proplists:get_value('$ancestors', hd(Status), []),
            Ancestors;
        _ -> []
    end.


%%--------------------------------------------------------------------
%% @doc
%% Generates a list of Gen(State) values from calling it Quantity times
%% starting from State0
%%
%% @spec generate(Gen, State0 :: state(), Quantity) -> {List, StateNext}
%%           Gen(State :: state()) -> fun()
%%           Quantity -> integer()
%% @end
%%--------------------------------------------------------------------
generate(Gen, State0, Quantity) when is_function(Gen),
                                     is_integer(Quantity) andalso Quantity > 0 ->
    {OutList, OutState} = generate(Gen, State0, Quantity, []),
    {lists:reverse(OutList), OutState}.

generate(_, State, 0, OutList) -> {OutList, State};

generate(Gen, State1, Quantity, OutList) ->
    {Val,State2} = Gen(State1),
    generate(Gen, State2, Quantity-1, [Val|OutList]).


%%--------------------------------------------------------------------
%% @doc
%% Make a referenced atom with Prefix
%%
%% @spec make_ref_atom(Prefix :: atom()) -> atom()
%% @end
%%--------------------------------------------------------------------
make_ref_atom(Prefix) when is_atom(Prefix) ->
    list_to_atom(lists:flatten([erlang:atom_to_list(Prefix),
                                erlang:ref_to_list(make_ref())])).
