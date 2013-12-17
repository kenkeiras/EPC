%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% slaveMock:init(Master, URL).
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(slaveMock).

%% Interface
-export([init/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simulates found url
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
randomString(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).


returnFoundURLs(Master, FoundURLs) ->
	Master ! {foundURLs, self(), {FoundURLs, []}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Master, _URL) ->
	% Start crawling URL
	timer:sleep(500), % Crawls
	timer:sleep(500), % Sends pics to database
	A = randomString(5, "abcde"), % This is a found URL
	B = randomString(5, "abcde"), % This is a found URL
	C = randomString(5, "abcde"), % This is a found URL
	returnFoundURLs(Master, [A, B, C]). % Return found URLs to master
	
    


