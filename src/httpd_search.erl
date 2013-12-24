-module(httpd_search).
-export([search/3]).

-define(EXPECTED_FORM, "multipart/form-data;").
-define(THRESHOLD, 1000).

splitDataBlocks(Boundary, Input) ->
    splitDataBlocks(Boundary, Input, [], []).

splitDataBlocks(_Boundary, [], Acc, Curr) ->
    if Curr /= [] ->
            [lists:reverse(Curr) | Acc];
       true ->
            Acc
    end;

splitDataBlocks(Boundary, Input, Acc, Curr) ->
    OnBoundary = (length(Input) >= length(Boundary))
        andalso (Boundary == lists:map(fun(X) -> lists:nth(X, Input) end,
                                       lists:seq(1, length(Boundary)))),
    if OnBoundary ->
            NewAcc = if Curr /= [] ->
                             [lists:reverse(Curr) | Acc];
                        true ->
                             Acc
                     end,
            splitDataBlocks(Boundary, lists:nthtail(length(Boundary), Input),
                            NewAcc, []);
       not OnBoundary ->
            splitDataBlocks(Boundary, lists:nthtail(1, Input),
                            Acc, [lists:nth(1, Input) | Curr])
    end.


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


                    DataBlocks = splitDataBlocks(Boundary, Input),
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
                            Result = epc_dba:get_by_simhash(Hash, ?THRESHOLD),
                            mod_esi:deliver(SessionID,
                                            io_lib:format("~p~n",
                                                          [Result]))
                    end
            end
    end.
    %mod_esi:deliver(SessionID, lists:map(fun (X) -> lists:nth(X, _Input) end,
    %                                     lists:seq(1, 100))).
    % lists:flatten(io_lib:format("~p", [ContentType]))
