-module(clustmea_worker_SUITE).

-compile(export_all).

groups() ->
    [{g1, [parallel], [make_succeeding_kv_producer_3Q, make_succeeding_kv_producer_Unique]}].

all() ->
    [{group, g1}].

-define(CW, clustmea_worker).


make_succeeding_kv_producer_3Q(_Config) ->
    Seed = 1,
    Quantity = 3,
    ValueSize = 10,
    {KVP,State0} = ?CW:make_succeeding_kv_producer(Seed, Quantity, ValueSize),
    {_,_,State1} = KVP(State0),
    {_,_,State2} = KVP(State1),
    {_,_,State3} = KVP(State2),
    exhausted = KVP(State3).


make_succeeding_kv_producer_Unique(_Config) ->
    Seed = 1,
    Quantity = 10,
    ValueSize = 10,
    {KVP,State0} = ?CW:make_succeeding_kv_producer(Seed, Quantity, ValueSize),
    {Data,_} =
        misc:generate(fun (S) ->
                              V = {_,_,S2} = KVP(S),
                              {V,S2}
                      end,
                      State0,
                      Quantity),
    {Keys,Values,States} = lists:unzip3(Data),

    %% Testing unique
    check_unique(States),
    check_unique(Keys),
    check_unique(Values).


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

check_unique(List) ->
    EstLength = length(List),
    EstLength = length(lists:usort(List)).
