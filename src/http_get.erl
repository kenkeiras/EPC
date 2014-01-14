%%% @doc
%%% pHash port interface.
%%% @end

-module(http_get).
-export([client/0, request/2]).

-export([http_get/0]).

% Machine endianness
-define(ENDIANNESS, little).
-define(HTTP_GET_BINARY_DIRECTORY, "priv").
-define(HTTP_GET_BINARY_FILE_NAME, "http_get").
-define(HTTP_GET_BINARY_PATH, filename:join(?HTTP_GET_BINARY_DIRECTORY,
                                            ?HTTP_GET_BINARY_FILE_NAME)).



%% Create a HTTP client
client() ->
    spawn(?MODULE, http_get, []).

%% Client initialization
http_get() ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ?HTTP_GET_BINARY_PATH}, [stream]),
    loop(Port).

%% Client calling
request(Client, URL) ->
    Client ! {self(), {call, URL}},
    receive
        {Client, {http_get, Data}} ->
            Data
    end.

%% Create a list of zeros with the given length
zeros(Num) when Num > 0 ->
    [0 | zeros(Num - 1)];

zeros(_) ->
    [].


%% Pad a list appending 0's until it reaches the given length
pad(List, Length) ->
    lists:append(List, zeros(Length - length(List))).


%% HTTP Get port encoder
encode(Msg) ->
    lists:append(pad(binary:bin_to_list(
                       binary:encode_unsigned(length(Msg), ?ENDIANNESS)), 8),
                 Msg).


%% @TODO retrieve real return code
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


divide_data(RawData) ->
    HeaderLength = binary:decode_unsigned(
                     binary:list_to_bin(lists:sublist(RawData, 8)),
                     ?ENDIANNESS),
    Header = lists:sublist(RawData, 9, HeaderLength),
    BodyLength = binary:decode_unsigned(
                     binary:list_to_bin(lists:sublist(RawData,
                                                      9 + HeaderLength, 8)),
                   ?ENDIANNESS),
    Body = lists:sublist(RawData, 17 + HeaderLength, BodyLength),
    {process_headers(Header), Body}.


%% HTTP Get port decoder
decode(Data) ->
    {Headers, Body} = divide_data(Data),
    {ok, {200, Headers, Body}}.


%% pHash port controller loop
loop(Port) ->
    receive
        {From, {call, Msg}} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    From ! {self(), {http_get, decode(Data)}}
            end,
            loop(Port);
        stop ->
            ok
    end.
