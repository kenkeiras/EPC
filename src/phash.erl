%%% @doc
%%% pHash port interface.
%%% @end

-module(phash).
-export([hash/1, start/0, stop/0]).

-export([pHash/0]).

% Machine endianness
-define(ENDIANNESS, little).
-define(PHASH_BINARY_DIRECTORY, "priv").
-define(PHASH_BINARY_FILE_NAME, "phash_port").
-define(PHASH_BINARY_PATH, filename:join(?PHASH_BINARY_DIRECTORY,
                                         ?PHASH_BINARY_FILE_NAME)).

%%% pHash port implementation
%% Extract perception hash from the given image file data
%% *NOT* an image url or 2D matrix or anything of the like, an 1D array
%% representing a image file.
hash(ImageData) ->
    phash ! {self(), {call, ImageData}},
    receive
        {phash, Hash} ->
            Hash
    end.


%% Stop phash service
stop() ->
    phash ! stop.

%% pHash port registration
pHash() ->
    register(phash, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ?PHASH_BINARY_PATH}, [stream]),
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
     Data.


%% pHash port controller loop
portLoop(Port) ->
    receive
        {From, {call, Msg}} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    From ! {phash, decode(Data)}
            end,
            portLoop(Port);
        stop ->
            ok
    end.


start() ->
    % pHash port service
    spawn(?MODULE, pHash, []).
