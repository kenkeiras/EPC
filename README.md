EPC
===

EPC is a Erlang-based parallel crawler for an image indexation engine.


Install
-------

Similarity search is based on pHash, which is used as a
[port](http://www.erlang.org/doc/tutorial/c_port.html),
so it must be compiled like this:

    unix> g++ -o phash_port ../include/phash_port.c $PATH_TO_PHASH/src/pHash.o -I$PATH_TO_PHASH/ -I$PATH_TO_PHASH/src/  -lpng -lpthread -ljpeg
