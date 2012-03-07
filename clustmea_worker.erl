-module(clustmea_worker).

-compile(export_all).


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
    Key   = io_lib:format("key-~w", [I]),
    Value = io_lib:format("val-~w", [I]),
    {Key, Value, Seed2}.


resize_value(V, Size, Payload) ->
    string:left(V, Size, Payload).


%%
%% Abstract "Upload" task executor
%%
executor(_,_,0,_) -> ok;

executor(KV_Producer, Uploader, Quantity, State1) ->
    {K,V, State2} = KV_Producer(State1),
    ok = Uploader(K,V),
    executor(KV_Producer, Uploader, Quantity-1, State2).


run(Seed, ValueSize, Quantity, Connection) ->
    {KV_Producer, State0} = make_succeeding_kv_producer(Seed, Quantity, ValueSize),
    Uploader = curl_uploader(Connection),
    executor(KV_Producer, Uploader, Quantity, State0).

curl_uploader(Connection) ->
    fun (K,V) ->
            error_logger:info_msg("curl_uploader (~p connection):"
                                  " ~p key, ~p value~n", [Connection, K, V]),
            ok
    end.
