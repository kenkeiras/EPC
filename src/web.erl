-module(web).

-export([init/0]).
-define(HTTPD_PORT, 8080).
-define(SERVER_NAME, "EPC").
-define(SERVER_ROOT, "include/web/").
-define(DOCUMENT_ROOT, "include/web/htdocs").
-define(ERROR_LOG_FILE, "error_log").

init() ->
    inets:start(),
    {ok, Pid} = inets:start(httpd, [{port, ?HTTPD_PORT},
                                    {server_name, ?SERVER_NAME},
                                    {server_root, ?SERVER_ROOT},
                                    {document_root, ?DOCUMENT_ROOT},
                                    {bind_address, {127,0,0,1}},
                                    {debug, all_functions},
                                    {modules,
                                     [mod_alias, mod_esi, mod_get, mod_log]},
                                    {directory_index, ["index.html"]},
                                    {erl_script_alias, {"/imgsearch",
                                                        [httpd_search]}},
                                    {error_log, ?ERROR_LOG_FILE}
                                   ]),
    Pid.