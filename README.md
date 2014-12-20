# leveltsdb

A TSDB wrapper around eleveldb. It uses eleveldb but provides an interface for
timeseries requests instead of just KV.

## Usage

Open a DB

    1> {ok, Ref} = leveltsdb:open("data").


Write some data, including the DB Ref, the metric name, timestamp, and value

    2> leveltsdb:write(Ref, <<"key">>, 1000, 0).
    ok
    3> leveltsdb:write(Ref, <<"key">>, 1001, 1).
    ok
    4> leveltsdb:write(Ref, <<"key">>, 1002, 2).
    ok
    5> leveltsdb:write(Ref, <<"key">>, 1003, 3).
    ok
    6> leveltsdb:write(Ref, <<"key">>, 1004, 4).
    ok
    7> leveltsdb:write(Ref, <<"key">>, 1005, 5).


Fold over all `<<"key">>`s

    8> leveltsdb:fold_metric(Ref, <<"key">>, fun({TS, Value}, Acc) -> [{TS, Value}|Acc] end, []).
    {1004,4},
    {1003,3},
    {1002,2},
    {1001,1},
    {1000,0}]


Fold over a specific TS range for a key

    9> leveltsdb:fold_metric(Ref, <<"key">>, 1001, 1003, fun({TS, Value}, Acc) -> [{TS, Value}|Acc] end, []).
    [{1003,3},
     {1002,2},
     {1001,1}]

Bucket aggregate values (defaults to 60 seconds). Useful when rolling up values on the fly without having to preaggregate. Supports ranges of timestamps.

    10> leveltsdb:aggregate(Ref, <<"key">>, 1000, 1001, <<"avg">>, []).
    {ok,[{960,0.5}]}

In this case the mod 60 was 960, the only timestamps for `<<"key">>` in the range `1000..1001` were 1000 and 1001, with the average value of 0.5.

To set a new bucket size you can pass the option `bucket_size`

    11> leveltsdb:aggregate(Ref, <<"key">>, 1000, 1005, <<"avg">>, [{bucket_size, 2}]).
    {ok,[{1000,0.5},{1002,2.5},{1004,4.5}]}


