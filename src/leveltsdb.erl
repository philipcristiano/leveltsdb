-module(leveltsdb).

-export([get/3,
         aggregate/6,
         open/1,
         close/1,
         fold_metric/4,
         fold_metric/6,
         metrics/1,
         metrics_with_prefix/2,
         write/4,
         write/5
         ]).

open(Path) ->
    eleveldb:open(Path, [{create_if_missing, true}]).

close(Ref) ->
    eleveldb:close(Ref).

write(Ref, Metric, TS, Value) when is_binary(Metric); is_integer(TS) ->
    write(Ref, Metric, TS, Value, [{sync, false}]).

write(Ref, Metric, TS, Value, Opts) when is_binary(Metric); is_integer(TS) ->
    Key = <<"m:", Metric/binary, <<":">>/binary, TS:32/integer>>,
    KeyKey = <<"k:", Metric/binary>>,
    ok = write_to_db(Ref, KeyKey, <<"">>, Opts),
    ok = write_to_db(Ref, Key, Value, Opts).

-spec get(eleveldb:db_ref(), binary(), integer()) -> {ok, _}.
get(Ref, Metric, TS) when is_binary(Metric); is_integer(TS) ->
    Key = <<"m:", Metric/binary, <<":">>/binary, TS:32/integer>>,
    {ok, Value} = eleveldb:get(Ref, Key, []),
    {ok, erlang:binary_to_term(Value)}.

fold_metric(Ref, Metric, Func, InAcc) ->
    Key = <<"m:", Metric/binary, <<":">>/binary, 0>>,
    Acc =
        try
            eleveldb:fold(Ref, fold_while_metric(Metric, Func), InAcc, [{first_key, Key}])
        catch
            {done, Val} -> Val
        end,
    Acc.

fold_metric(Ref, Metric, TS1, TS2, Func, InAcc) ->
    Key = <<"m:", Metric/binary, <<":">>/binary, TS1:32/integer>>,
    Acc =
        try
            eleveldb:fold(Ref, fold_range(Metric, TS2, Func), InAcc, [{first_key, Key}])
        catch
            {done, Val} -> Val
        end,
    Acc.

aggregate(Ref, Metric, TS1, TS2, Alg, Opts) ->
    Key = <<"m:", Metric/binary, <<":">>/binary, TS1:32/integer>>,
    {F, Agg} = leveltsdb_buckets:online_fold(
                proplists:get_value(aggregation, Opts, Alg),
                proplists:get_value(bucket_size, Opts, 60)),
    Acc =
        try
            eleveldb:fold(Ref, fold_range(Metric, TS2, F), Agg, [{first_key, Key}])
        catch
            {done, Val} -> Val
        end,
    ListAcc = F({eoi, eoi}, Acc),
    ForwardAcc = lists:reverse(ListAcc),
    {ok, ForwardAcc}.

metrics(Ref) ->
    Acc =
        try
            eleveldb:fold(Ref, fold_while_keys(), [], [{first_key, <<"k:">>}])
        catch
            {done, Val} -> Val
        end,
    {ok, lists:reverse(Acc)}.

metrics_with_prefix(Ref, Prefix) ->
    Key = <<"k:", Prefix/binary>>,
    Acc =
        try
            eleveldb:fold(Ref, fold_while_prefix(Prefix), [], [{first_key, Key}])
        catch
            {done, Val} -> Val
        end,
    {ok, lists:reverse(Acc)}.

metrics(_Ref, StartMetric) ->
    ok.
metrics(_Ref, StartMetric, StopMetric) ->
    ok.


%% Internal
%%
fold_while_keys() ->
    fun ({Key, _Value}, Acc)->
        case Key of
            <<"k:", MetricName/binary >> ->

                [MetricName |Acc];
            _ ->
                throw({done, Acc})
        end
    end.

fold_while_prefix(Prefix) ->
    PrefixLength = size(Prefix),
    fun ({Key, _Value}, Acc)->
        case Key of
            <<"k:", Prefix:PrefixLength/binary, Rest/binary>> ->
                [<<Prefix/binary, Rest/binary>> |Acc];
            _ ->
                throw({done, Acc})
        end
    end.

fold_while_metric(MetricName, Callback) ->
    PrefixLength = size(MetricName),
    fun ({Key, Value}, Acc)->
        case Key of
            <<"m:", MetricName:PrefixLength/binary, ":", EncodedTS:32/integer>> ->

                Callback({EncodedTS, erlang:binary_to_term(Value)}, Acc);
            _ ->
                throw({done, Acc})
        end
    end.

fold_range(MetricName, EncodedEndTS, Callback) ->
    PrefixLength = size(MetricName),
    fun ({Key, Value}, Acc)->
        case Key of
            <<"m:", MetricName:PrefixLength/binary, ":", EncodedTS:32/integer>> ->
               case EncodedTS > EncodedEndTS of
                    true ->
                        throw({done, Acc});
                    false ->
                        Callback({EncodedTS, erlang:binary_to_term(Value)}, Acc)
               end;
            _ ->
                throw({done, Acc})
        end
    end.

write_to_db(Ref, Key, Value, Opts) when is_binary(Key) ->
    eleveldb:write(Ref, [{put, Key, erlang:term_to_binary(Value)}], Opts).
