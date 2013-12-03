%%% @doc
%%% Downloads, processes and indexes the given images.
%%% @end

-module(indexer).
-export([indexImage/1]).

-on_load(on_load/0).
-export([pHash/0]).


% Machine endianness
-define(ENDIANNESS, little).

%% Extract image feature
extractPerception(ImageData) ->
    phash ! {self(), {call, ImageData}},
    receive
        {phash, Perception} ->
            Perception
    end.


%% Add image url and perception to index database
addToIndex(ImageUrl, Perception) ->
    io:format("[~p] ~p~n", [Perception, ImageUrl]).


%% Main function
indexImage(ImageUrl) ->
    {ok, AnswerData} = httpc:request(ImageUrl),
    {_ReturnedCode, _ReturnedHeaders, ImageData} = AnswerData,
    Perception = extractPerception(ImageData),
    addToIndex(ImageUrl, Perception).

%% pHash port
pHash() ->
    register(phash, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, "./phash_port"}, [stream]),
    portLoop(Port).


%% Create a list of zeros with the given length
zeros(Num) when Num > 0 ->
    [0 | zeros(Num - 1)];

zeros(_) ->
    [].


%% Pad a list appending 0's until it reaches the given length
pad(List, Length) ->
    lists:append(List, zeros(Length - length(List))).


%% pHash port encoder
encode(Msg) ->
    lists:append(pad(binary:bin_to_list(
                       binary:encode_unsigned(length(Msg), ?ENDIANNESS)), 8),
                 Msg).


%% pHash port decoder
decode(Data) ->
     binary:decode_unsigned(binary:list_to_bin(Data), ?ENDIANNESS).


%% pHash port controller loop
portLoop(Port) ->
    receive
        {From, {call, Msg}} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    From ! {phash, decode(Data)}
            end
    end,
    portLoop(Port).


%% Executes on module load, initializes the needed structures
on_load() ->
    % Inets required for HTTP client requests
    inets:start(),
    % pHash port
    spawn(?MODULE, pHash, []),
    ok.
