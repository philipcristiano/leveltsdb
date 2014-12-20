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



## Getting the names of the metrics in the DB

First we store something other than `<<"key">>`

    23> leveltsdb:write(Ref, <<"a">>, 1, 1).
    ok
    24> leveltsdb:write(Ref, <<"ab">>, 1, 1).
    ok
    25> leveltsdb:write(Ref, <<"ac">>, 1, 1).
    ok
    26> leveltsdb:write(Ref, <<"b">>, 1, 1).
    ok
    27> leveltsdb:write(Ref, <<"c">>, 1, 1).
    ok
    28> leveltsdb:write(Ref, <<"de">>, 1, 1).

Then we can list all metrics

    29> leveltsdb:metrics(Ref).
    {ok,[<<"a">>,<<"ab">>,<<"ac">>,<<"b">>,<<"c">>,<<"de">>]}

Or just metrics with a particular prefix

    30> leveltsdb:metrics_with_prefix(Ref, <<"a">>).
    {ok,[<<"a">>,<<"ab">>,<<"ac">>]}

Each metric name is stored as a separate key in LevelDB so that lookups don't require going over every metric in the DB.
