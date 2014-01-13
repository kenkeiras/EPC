-module(httpd_stats).
-export([stats/3]).

statsJson() ->
    io_lib:format("{\"crawled_urls\": ~w, \"indexed_images\": ~w}",
                  [epc_dba:get_crawled_url_count(),
                   epc_dba:get_indexed_image_count()]).

%% @doc Manage search petition from the HTTP daemon
stats(SessionID, _Env, _Input) ->
    mod_esi:deliver(SessionID, statsJson()).
