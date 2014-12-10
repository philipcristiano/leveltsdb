-module(leveltsdb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0]).
-export([groups/0, init_per_group/2, end_per_group/2]).
-export([fold_metric/1,
         fold_metric_large_range/1,
         write_read/1,
         write_multiple/1]).

all() -> [{group, db}].

groups() -> [{db,
             [],
             [write_read,
              write_multiple,
              fold_metric,
              fold_metric_large_range]}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

db_for_config(Config) ->
    Dir = ?config(priv_dir, Config),
    {ok, DB} = leveltsdb:open(Dir),
    DB.

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

acc_ts_as_list({_K, TS, _V}, Acc) ->
    [TS | Acc].
