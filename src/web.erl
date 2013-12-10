-module(web).

-export([start/0]).
-define(HTTPD_PORT, 8080).
-define(SERVER_NAME, "EPC").
-define(SERVER_ROOT, "web/").
-define(DOCUMENT_ROOT, "web/htdocs").
-define(BIND_ADDRESS, "localhost").


start() ->
    inets:start(),
    {ok, Pid} = inets:start(httpd, [{port, ?HTTPD_PORT},
                                     {server_name, ?SERVER_NAME},
                                     {server_root, ?SERVER_ROOT},
                                     {document_root, ?DOCUMENT_ROOT},
                                     {bind_address, ?BIND_ADDRESS}]),
    Pid.
