-module(leveltsdb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).
-export([groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([avg_buckets/1,
         fold_metric/1,
         fold_metric_large_range/1,
         fold_metric_with_range/1,
         fold_data/1,
         all_metrics/1,
         prefix_metrics/1,
         write_read/1,
         write_multiple/1]).

all() -> [{group, db}].

groups() -> [{db,
             [],
             [write_read,
              write_multiple,
              fold_metric,
              fold_metric_large_range,
              fold_metric_with_range,
              fold_data,
              all_metrics,
              prefix_metrics,
              avg_buckets]}].

init_per_testcase(_, Config) ->
    Dir = ?config(priv_dir, Config),
    {ok, DB} = leveltsdb:open(Dir),
    [{db, DB} | Config].

end_per_testcase(_, Config) ->
    ok = leveltsdb:close(db_for_config(Config)),
    ok.

db_for_config(Config) ->
    ?config(db, Config).

write_read(Config) ->
    DB = db_for_config(Config),
    {K, V} = {<<"Key">>, <<"Value">>},
    TS = 1418223408,
    leveltsdb:write(DB, K, TS, V),
    {ok, Read_V} = leveltsdb:get(DB, K, TS),
    ?assertEqual(V, Read_V).

write_multiple(Config) ->
    DB = db_for_config(Config),
    {K, V} = {<<"Key">>, <<"Value">>},
    TS = 1418223408,
    leveltsdb:write(DB, K, 1418223407, V),
    leveltsdb:write(DB, K, TS, V),
    leveltsdb:write(DB, K, 1418223409, V),
    {ok, Read_V} = leveltsdb:get(DB, K, TS),
    ?assertEqual(V, Read_V).

fold_metric(Config) ->
    DB = db_for_config(Config),
    {K, V} = {<<"Key">>, <<"Value">>},
    TSS = [1418223409, 1418223410, 1418223411, 1418223412],
    lists:map(fun(TS) -> leveltsdb:write(DB, K, TS, V) end, TSS),
    Acc = leveltsdb:fold_metric(DB, K, fun acc_ts_as_list/2, []),
    ReversedAcc = lists:reverse(Acc),
    ?assertEqual(TSS, ReversedAcc).

fold_metric_large_range(Config) ->
    DB = db_for_config(Config),
    {K, V} = {<<"Key">>, <<"Value">>},
    TSS = [9, 80, 700, 6000, 50000, 612345],
    lists:map(fun(TS) -> leveltsdb:write(DB, K, TS, V) end, TSS),
    Acc = leveltsdb:fold_metric(DB, K, fun acc_ts_as_list/2, []),
    ReversedAcc = lists:reverse(Acc),
    ?assertEqual(TSS, ReversedAcc).

fold_metric_with_range(Config) ->
    DB = db_for_config(Config),
    {K, V} = {<<"Key">>, <<"Value">>},
    TSS = [100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200],
    lists:map(fun(TS) -> leveltsdb:write(DB, K, TS, V) end, TSS),
    Acc = leveltsdb:fold_metric(DB, K, 130, 170, fun acc_ts_as_list/2, []),
    ReversedAcc = lists:reverse(Acc),
    ?assertEqual([130, 140, 150, 160, 170], ReversedAcc).

fold_data(Config) ->
    DB = db_for_config(Config),
    V = <<"Value">>,
    TSS = [100, 200],
    lists:map(fun(TS) -> leveltsdb:write(DB, <<"K1">>, TS, V) end, TSS),
    lists:map(fun(TS) -> leveltsdb:write(DB, <<"K2">>, TS, V) end, TSS),
    lists:map(fun(TS) -> leveltsdb:write(DB, <<"K3">>, TS, V) end, TSS),
    Acc = leveltsdb:fold_data(DB, fun acc_all_as_list/2, []),
    ReversedAcc = lists:reverse(Acc),
    ?assertEqual([{<<"K1">>, 100, V},
                  {<<"K1">>, 200, V},
                  {<<"K2">>, 100, V},
                  {<<"K2">>, 200, V},
                  {<<"K3">>, 100, V},
                  {<<"K3">>, 200, V}], ReversedAcc).

avg_buckets(Config) ->
    DB = db_for_config(Config),
    K = <<"Key">>,
    leveltsdb:write(DB, K, 100, 1),
    leveltsdb:write(DB, K, 101, 2),
    leveltsdb:write(DB, K, 102, 3),
    leveltsdb:write(DB, K, 120, 6),
    leveltsdb:write(DB, K, 121, 8),
    leveltsdb:write(DB, <<"FOOODO">>, 1000, 8),
    {ok, Acc} = leveltsdb:aggregate(DB, K, 0, 180, <<"avg">>, []),
    ?assertEqual([{60, 2.0}, {120, 7.0}], Acc).

all_metrics(Config) ->
    DB = db_for_config(Config),
    V = 0,
    Metrics = [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>],
    lists:map(fun(M) -> leveltsdb:write(DB, M, 100, V) end, Metrics),
    {ok, ReturnedMetrics} = leveltsdb:metrics(DB),
    ?assertEqual(Metrics, ReturnedMetrics).

prefix_metrics(Config) ->
    DB = db_for_config(Config),
    V = 0,
    Metrics = [<<"a">>, <<"ab">>, <<"abc">>, <<"abd">>, <<"ac">>],
    lists:map(fun(M) -> leveltsdb:write(DB, M, 100, V) end, Metrics),
    {ok, ReturnedMetrics} = leveltsdb:metrics_with_prefix(DB, <<"ab">>),
    ?assertEqual([<<"ab">>,<<"abc">>,<<"abd">>], ReturnedMetrics).


acc_ts_as_list({TS, _V}, Acc) ->
    [TS | Acc].

acc_all_as_list({Metric, TS, V}, Acc) ->
    [{Metric, TS, V} | Acc].
