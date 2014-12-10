-module(leveltsdb).

-export([get/3,
         open/1,
         write/4
         ]).

open(Path) ->
    eleveldb:open(Path, [{create_if_missing, true}]).

write(Ref, Key, Value) ->
    eleveldb:write(Ref, [{put, Key, Value}], [{sync, false}]).

write(Ref, Metric, TS, Value) ->
    Key = <<"m:", Metric/binary, <<":">>/binary, TS:32/integer>>,
    write(Ref, Key, Value),
    ok.

get(Ref, Metric, TS) ->
    Key = <<"m:", Metric/binary, <<":">>/binary, TS:32/integer>>,
    eleveldb:get(Ref, Key, []).
