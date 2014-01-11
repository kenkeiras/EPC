-module(httpd_search).
-export([search/3]).

-define(EXPECTED_FORM, "multipart/form-data;").

-define(MAX_RESULTS, 20).

separateImageAndData(ImageBlock) ->
    Block = string:strip(string:strip(ImageBlock, left, $\r), left, $\n),
    separateImageAndData(Block, []).

separateImageAndData(ImageBlock, Headers) ->
    HasCarryReturn = ImageBlock /= [] andalso
        lists:nth(1, ImageBlock) == $\r,
    Block = if HasCarryReturn ->
                    lists:nthtail(1, ImageBlock);
               true ->
                    ImageBlock
                 end,
    {Line, [$\n | Rest]} = lists:splitwith(fun (X) -> X /= $\n end,
                                           Block),
    if Line == [] ->
            {Headers, Rest};
       true ->
            separateImageAndData(Rest, [string:strip(Line, right, $\r)
                                        | Headers])
    end.

%%% Convert image URL list to HTML
showResults(R) ->
    showResults(R, [], ?MAX_RESULTS).

showResults([], Acc, _) ->
    %% Join the results with the separator
    "[\"" ++ string:join(lists:reverse(Acc), "\",\"") ++ "\"]";

showResults(_, Acc, 0) ->
    showResults([], Acc, 0);

showResults([Url | T], Acc, ResultsToGo) ->
    showResults(T, [Url | Acc], ResultsToGo - 1).


%% @doc Manage search petition from the HTTP daemon
search(SessionID, Env, Input) ->
                                                % Check content type
    ContentTypeHeader = lists:keyfind(http_content_type, 1, Env),
    if ContentTypeHeader == false ->
            mod_esi:deliver(SessionID, "Error, no Content-Type");

       true ->
            % Check that the content type is the expected one
            {http_content_type, ContentType} = ContentTypeHeader,
            Fits = ?EXPECTED_FORM == lists:map(
                                       fun (X) ->
                                               lists:nth(X, ContentType)
                                       end,
                                       lists:seq(1, length(?EXPECTED_FORM))),
            if not Fits ->
                    mod_esi:deliver(SessionID,
                                    io_lib:format(
                                      "Error, unknown Content-Type%% : ~p",
                                      [ContentType]));
               Fits ->
                    % Break down the data and do the actual search
                    {_, ABound} = lists:splitwith(
                                    fun (X) -> X /= $= end,
                                    lists:nthtail(length(?EXPECTED_FORM) + 1,
                                                  ContentType)),
                    Boundary = lists:nthtail(1, ABound),


                    DataBlocks = string:tokens(Input, [Boundary]),
                    InterestingBlocks = lists:filter(fun (X) ->
                                                             length(X) > 10
                                                     end, DataBlocks),

                    %% @FIXME should be a more clean way buried in RFC 2388
                    if length(InterestingBlocks) /= 1 ->
                            mod:deliver(
                              SessionID,
                              "Sorry, couldn't find the data block :P");
                       true ->
                            [ImageBlock] = InterestingBlocks,
                            {_Head, Image} = separateImageAndData(ImageBlock),
                            Hash = phash:hash(Image),
                            Results = epc_dba:get_by_simhash(Hash),
                            mod_esi:deliver(SessionID, showResults(Results))
                    end
            end
    end.
