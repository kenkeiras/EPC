%%% @doc
%%% Downloads, processes and indexes the given images.
%%% @end

-module(indexer).
-export([indexImage/1, start/0]).

-export([indexer/0]).

%%% Indexer service implementation
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
    indexer ! {self(), {index, ImageUrl}}.


%% indexing loop
indexer_loop() ->
    receive
        {_From, {index, ImageUrl}} ->
            {ok, AnswerData} = httpc:request(ImageUrl),
            {_ReturnedCode, _ReturnedHeaders, ImageData} = AnswerData,
            Perception = extractPerception(ImageData),
            addToIndex(ImageUrl, Perception),
            indexer_loop()
    end.


%% indexer service
indexer() ->
    register(indexer, self()),
    indexer_loop().


%% Executes on module load, initializes the needed structures
start() ->
    % Inets required for HTTP client requests
    inets:start(),
    % indexer service
    spawn(?MODULE, indexer, []),
    ok.
