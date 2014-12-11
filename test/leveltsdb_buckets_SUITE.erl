%%%-------------------------------------------------------------------
%%% @author Philip Cristiano
%%% @copyright 2014 Philip Cristiano
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-module(leveltsdb_buckets_SUITE).
-define(MUT, leveltsdb_buckets).

-export([all/0, groups/0]).
-export([first_fold/1,
         first_fold_float/1,
         same_bucket/1,
         multiple_buckets/1,
         avg_first_test/1,
         avg_two_items_test/1,
         avg_three_items_test/1,
         avg_four_items_test/1,
         avg_second_bucket_test/1,
         avg_second_bucket_two_item_test/1,
         avg_second_bucket_three_item_test/1,
         min_first_test/1,
         min_two_items_test/1,
         min_buckets_test/1]).

all() -> [{group, buckets}].

groups() -> [{buckets,
              [],
              [first_fold,
               first_fold_float,
               same_bucket,
               multiple_buckets,
               avg_first_test,
               avg_two_items_test,
               avg_three_items_test,
               avg_four_items_test,
               avg_second_bucket_test,
               avg_second_bucket_two_item_test,
               avg_second_bucket_three_item_test,
               min_first_test,
               min_two_items_test,
               min_buckets_test]}].

first_fold(_Config) ->
    F = ?MUT:first_fold(60),
    Acc = lists:foldl(F, [], [{1, 10}]),
    [?_assertEqual([{0, 10}], Acc)].

first_fold_float(_Config) ->
    F = ?MUT:first_fold(60),
    Acc = lists:foldl(F, [], [{1, 10.0}]),
    [?_assertEqual([{0, 10.0}], Acc)].

same_bucket(_Config) ->
    F = ?MUT:first_fold(60),
    Acc = lists:foldl(F, [], [{1, 10},
                              {2, 11}]),
    [?_assertEqual([{0, 10}], Acc)].

multiple_buckets(_Config) ->
    F = ?MUT:first_fold(60),
    Acc = lists:foldl(F, [], [{1, 10},
                              {2, 11},
                              {61, 12},
                              {62, 13}]),
    [?_assertEqual([{60, 12}, {0, 10}], Acc)].

avg_first_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0}, {eoi, eoi}]),
    ?assertEqual([{0, 10.0}], EndAcc).

avg_two_items_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {eoi, eoi}]),
    ?assertEqual([{0, 7.5}], EndAcc).

avg_three_items_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {eoi, eoi}]),
    ?assertEqual([{0, 20.0}], EndAcc).

avg_four_items_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {3, 80.0},
                                  {eoi, eoi}]),
    ?assertEqual([{0, 35.0}], EndAcc).

avg_second_bucket_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {3, 80.0},
                                  {60, 1.0},
                                  {eoi, eoi}]),
    ?assertEqual([{60, 1.0}, {0, 35.0}], EndAcc).

avg_second_bucket_two_item_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {3, 80.0},
                                  {60, 1.0},
                                  {61, 3.0},
                                  {eoi, eoi}]),
    ?assertEqual([{60, 2.0}, {0, 35.0}], EndAcc).

avg_second_bucket_three_item_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {3, 80.0},
                                  {60, 1.0},
                                  {61, 3.0},
                                  {62, 5.0},
                                  {eoi, eoi}]),
    ?assertEqual([{60, 3.0}, {0, 35.0}], EndAcc).


min_first_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"min">>, 60),
    EndAcc = lists:foldl(F, Acc, [{1, 10.0}, {eoi, eoi}]),
    ?assertEqual([{0, 10.0}], EndAcc).

min_two_items_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"min">>, 60),
    EndAcc = lists:foldl(F, Acc, [{1, 10.0}, {2, 5.0}, {eoi, eoi}]),
    ?assertEqual([{0, 5.0}], EndAcc).

min_buckets_test(_Config) ->
    {F, Acc} = leveltsdb_buckets:online_fold(<<"min">>, 60),
    EndAcc = lists:foldl(F, Acc, [{1, 10.0},
                                  {2, 5.0},
                                  {60, 1.0},
                                  {61, 2.0},
                                  {eoi, eoi}]),
    ?assertEqual([{60, 1.0}, {0, 5.0}], EndAcc).
