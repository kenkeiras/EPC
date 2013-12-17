EPC
===

EPC is a Erlang-based parallel crawler for an image indexation engine.


Install
-------

Similarity search is based on pHash, which is used as a
[port](http://www.erlang.org/doc/tutorial/c_port.html),
so it must be compiled like this:

    unix> g++ -o phash_port ../include/phash_port.c $PATH_TO_PHASH/src/pHash.o -I$PATH_TO_PHASH/ -I$PATH_TO_PHASH/src/  -lpng -lpthread -ljpeg


Mnesia DB must be installed by calling epc_dba:install(list_of_nodes), where list_of_nodes can be simply [node()].

Start up
--------

Get to the *src*̂ directory `cd src`
Compile the erlang code `erlc *`
Run an erlang interpreter `erl`
Load the master, indexer and phash port

    1> c(master).
    {ok,master}
    2> c(indexer).
    {ok,indexer}
    3> c(phash).
    {ok,phash}`


And start the services

    4> master:init().
    true
    5> indexer:start().
    ok
    6> phash:start().
    <0.66.0>`
