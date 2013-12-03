EPC
===

EPC is a Erlang-based parallel crawler for an image indexation engine.


Install
-------

Similarity search is based on pHash, which is used as a
[port](http://www.erlang.org/doc/tutorial/c_port.html),
so it must be compiled like this:

    unix> gcc -o src/phash_port include/phash_port.c
