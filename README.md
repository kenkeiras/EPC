EPC
===

EPC is a Erlang-based parallel crawler for an image indexation engine.


Install
-------

####Clone repo

    git clone https://github.com/kenkeiras/EPC
     
####Install phash-port

    cd EPC/
    mkdir priv
    cd priv/
    wget "codigoparallevar.com/phash_port"
    sudo chmod 766 phash_port
    cd ..

#### Install the HTTP client port

    cd priv/
    gcc ../include/http_get.c -lcurl -o http_get.o
    cd ..

###Compile the files

    erl -make

###Install mnnesia

    erl -pa ebin/
    1> epc_dba:install([node()]).

Start up
--------

###Start the interpreter on the `ebin/` directory

    erl -pa ebin/ 

###And start the services

    1> master:init().
    true
    2> indexer:start().
    ok
    3> phash:start().
    <0.66.0>
    4> epc_dba:start([node()]).
    Starting database... database started.
    ok
    5> web:init().
    <0.92.0>

In a web browser go to localhost:8080
