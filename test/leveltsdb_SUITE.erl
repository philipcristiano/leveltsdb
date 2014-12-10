-module(leveltsdb_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([groups/0, init_per_group/2, end_per_group/2]).
-export([write_read/1, write_multiple/1]).

all() -> [{group, db}].

groups() -> [{db,
             [shuffle],
             [write_read, write_multiple]}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

write_read(Config) ->
    Dir = ?config(priv_dir, Config),
    {ok, DB} = leveltsdb:open(Dir),
    {K, V} = {<<"Key">>, <<"Value">>},
    TS = 1418223408,
    leveltsdb:write(DB, K, TS, V),
    {ok, Read_V} = leveltsdb:get(DB, K, TS),
    Read_V = V.

write_multiple(Config) ->
    Dir = ?config(priv_dir, Config),
    {ok, DB} = leveltsdb:open(Dir),
    {K, V} = {<<"Key">>, <<"Value">>},
    TS = 1418223408,
    leveltsdb:write(DB, K, 1418223407, V),
    leveltsdb:write(DB, K, TS, V),
    leveltsdb:write(DB, K, 1418223409, V),
    {ok, Read_V} = leveltsdb:get(DB, K, TS),
    Read_V = V.
