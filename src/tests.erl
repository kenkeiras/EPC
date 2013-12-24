-module(tests).
-export([test/0]).

test1()->
    master:getImages(["http://elpais.com/"]).

test()->
    master:init(),
    indexer:start(),
    phash:start(),
    epc_dba:start([node()]),
    test1().
