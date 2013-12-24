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
    

Start up
--------

###Compile the files

    unix> erl -make

###Start the interpreter on the `ebin/` directory

    erl -pa ebin/ 

###And start the services

    1> master:init().
    true
    2> indexer:start().
    ok
    3> phash:start().
    <0.66.0>`
