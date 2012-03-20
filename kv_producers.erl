-module(kv_producers).

-export([make_succeeding_kv_producer/3]).


make_succeeding_kv_producer(Seed, Quantity, ValueSize) ->
    Payload = hd("g"), % char of the Sized Value payload
    KV_Producer = fun ({_, 0}) -> exhausted;
                      ({Seed1, Left}) ->
                          {K,V,Seed2} = succeeding_gen(Seed1),
                          SizedV = resize_value(V, ValueSize, Payload),
                          State2 = {Seed2,Left-1},
                          {K,SizedV,State2}
                  end,
    State0 = {Seed, Quantity},
    {KV_Producer, State0}.


succeeding_gen(Seed1) ->
    {I, Seed2} = {Seed1, Seed1+1},
    Key   = ["key-", integer_to_list(I)],
    Value = ["val-", integer_to_list(I)],
    {Key, Value, Seed2}.


resize_value(V, Size, Payload) ->
    string:left(lists:flatten(V), Size, Payload).
