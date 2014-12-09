-module(leveltsdb).

-export([get/2,
         open/1,
         write/3]).


open(Path) ->
    eleveldb:open(Path, [{create_if_missing, true}]).

write(Ref, Key, Value) ->
    eleveldb:write(Ref, [{put, Key, Value}], [{sync, false}]).

get(Ref, Key) ->
    eleveldb:get(Ref, Key, []).
