-module(leveltsdb_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([groups/0, init_per_group/2, end_per_group/2]).
-export([write_read/1]).

all() -> [{group, db}].

groups() -> [{db,
             [parallel, {repeat, 10}],
             [write_read]}].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

write_read(Config) ->
    Dir = ?config(priv_dir, Config),
    {ok, DB} = leveltsdb:open(Dir),
    {K, V} = {<<"Key">>, <<"Value">>},
    leveltsdb:write(DB, K, V),
    {ok, New_V} = leveltsdb:get(DB, K),
    New_V = V.
