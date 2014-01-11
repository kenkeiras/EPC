-module(tests).
-export([test/0]).

try_to_install() ->
    try epc_dba:install([node()]) of
        ok -> ok
    catch _:{badmatch, {error, {_, {already_exists, _}}}} ->
            ok
    end.

test1()->
    master:getImages(["http://elpais.com/"]).

test()->
    master:init(),
    indexer:start(),
    phash:start(),
    try_to_install(),
    epc_dba:start([node()]),
    web:init(),
    test1().
