# leveltsdb

A TSDB wrapper around eleveldb. It uses eleveldb but provides an interface for
timeseries requests instead of just KV.

## Usage

Open a DB

    1> {ok, Ref} = leveltsdb:open("data").


Write some data, including the DB Ref, the metric name, timestamp, and value

    2> leveltsdb:write(Ref, <<"key">>, 1000, <<"value">>).
    ok
    3> leveltsdb:write(Ref, <<"key">>, 1001, <<"value-1">>).
    ok
    4> leveltsdb:write(Ref, <<"key">>, 1002, <<"value-2">>).
    ok
    5> leveltsdb:write(Ref, <<"key">>, 1003, <<"value-3">>).
    ok
    6> leveltsdb:write(Ref, <<"key">>, 1004, <<"value-4">>).


Fold over all `<<"key">>`s

    7> leveltsdb:fold_metric(Ref, <<"key">>, fun({Key, TS, Value}, Acc) -> [{Key, TS, Value}|Acc] end, []).
    {<<"key">>,1004,<<"value-4">>},
    {<<"key">>,1003,<<"value-3">>},
    {<<"key">>,1002,<<"value-2">>},
    {<<"key">>,1001,<<"value-1">>},
    {<<"key">>,1000,<<"value">>}]


Fold over a specific TS range for a key

    9> leveltsdb:fold_metric(Ref, <<"key">>, 1001, 1003, fun({Key, TS, Value}, Acc) -> [{Key, TS, Value}|Acc] end, []).
    [{<<"key">>,1003,<<"value-3">>},
     {<<"key">>,1002,<<"value-2">>},
     {<<"key">>,1001,<<"value-1">>}]
