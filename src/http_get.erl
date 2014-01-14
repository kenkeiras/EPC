%%% @doc
%%% pHash port interface.
%%% @end

-module(http_get).
-export([client/0, request/2]).

-export([http_get/1]).

% Machine endianness
-define(ENDIANNESS, little).
-define(HTTP_GET_BINARY_DIRECTORY, "priv").
-define(HTTP_GET_BINARY_FILE_NAME, "http_get").
-define(HTTP_GET_BINARY_PATH, filename:join(?HTTP_GET_BINARY_DIRECTORY,
                                            ?HTTP_GET_BINARY_FILE_NAME)).



%% Create a HTTP client
client() ->
    spawn(?MODULE, http_get, [self()]).

%% Client initialization
http_get(Master) ->
    process_flag(trap_exit, true),
    link(Master),
    Port = open_port({spawn, ?HTTP_GET_BINARY_PATH}, [{packet, 4}]),
    loop(Port).

%% Client calling
request(Client, URL) ->
    Client ! {self(), {call, URL}},
    receive
        {Client, {http_get, Data}} ->
            Data
    end.


%% HTTP Get port encoder
encode(Msg) ->
    Msg.


%% @TODO retrieve real return code
process_headers([]) ->
    [];

process_headers(Headers) ->
    Lines = lists:nthtail(1, lists:filter(fun (X) -> length(X) > 0 end,
                                          string:tokens(Headers, [$\n, $\r]))),

    lists:map(fun (H) ->  X = lists:splitwith(fun (X) -> X =/= $: end, H),
                          case X of
                              {K, V} ->
                                  {string:to_lower(K),
                                   string:to_lower(lists:nthtail(2, V))};
                              ANY ->
                                  {string:to_lower(ANY), ""}
                          end
              end, Lines).


%% HTTP Get port controller loop
loop(Port) ->
    receive
        {From, {call, Msg}} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Headers}} ->
                    receive
                        {Port, {data, Body}} ->
                            From ! {self(), {http_get, {ok, {200,
                                                             process_headers(Headers),
                                                             Body}}}}
                    end
            end,
            loop(Port);
        {'EXIT', _PID, _} ->
            ok;
        stop ->
            ok;
        X ->
            io:format("~p~n", [X])
    end.
