-module(leveltsdb).

-export([get/3,
         open/1,
         fold_metric/4,
         write/4
         ]).

open(Path) ->
    eleveldb:open(Path, [{create_if_missing, true}]).

write(Ref, Key, Value) ->
    eleveldb:write(Ref, [{put, Key, erlang:term_to_binary(Value)}], [{sync, false}]).

write(Ref, Metric, TS, Value) ->
    Key = <<"m:", Metric/binary, <<":">>/binary, TS:32/integer>>,
    write(Ref, Key, Value),
    ok.

get(Ref, Metric, TS) ->
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

fold_while_metric(MetricName, Callback) ->
    PrefixLength = size(MetricName),
    fun ({Key, Value}, Acc)->
        case Key of
            <<"m:", MetricName:PrefixLength/binary, ":", EncodedTS:32/integer>> ->

                Callback({MetricName, EncodedTS, erlang:binary_to_term(Value)}, Acc);
            _ ->
                throw({done, Acc})
        end
    end.
