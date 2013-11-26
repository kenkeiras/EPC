%%% @doc
%%% Downloads, processes and indexes the given images.
%%% @end

-module(indexer).
-export([indexImage/1]).

-on_load(on_load/0).


%% Extract image feature
%% Dummy implementation
extractPerception(ImageData) ->
    ImageData.


%% Add image url and perception to index database
%% Dummy implementation
addToIndex(ImageUrl, Perception) ->
    io:format("[~p bytes] ~p~n", [length(Perception), ImageUrl]).


%% Main function
indexImage(ImageUrl) ->
    {ok, AnswerData} = httpc:request(ImageUrl),
    {_ReturnedCode, _ReturnedHeaders, ImageData} = AnswerData,
    Perception = extractPerception(ImageData),
    addToIndex(ImageUrl, Perception).


%% Executes on module load, initializes the needed structures
on_load() ->
    % Inets required for HTTP client requests
    inets:start(),
    ok.
