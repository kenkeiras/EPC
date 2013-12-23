-module(tests).
-export([test/0]).

test1()->
    master:getImages(["http://www.mixing.dj"]).

test()->
    master:init(),
    indexer:start(),
    phash:start(),
    test1().
    
