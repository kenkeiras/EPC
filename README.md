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

###Install mnnesia

    cd src
    erlc *.erl
    erl
    epc_dba:install([node()]).
    

###Start up web server

     master:init().
     phash:start().
     indexer:start().
     web:init().

In a web browser go to localhost:8080


