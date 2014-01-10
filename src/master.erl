%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% master:init().
% master:getImages(URLs).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(master).

%% Interface
-export([init/0, getImages/1]).

%% Debug methods
-export([printPendingURLs/0]).

%% Internal Exports
-export([loop/3, removeDuplicatedURLs/2, printList/1]).

-define(SLAVE_LIMIT, 50).

-define(PENDING_URLS_LIMIT, 100).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
	process_flag(trap_exit, true),
	PID = spawn(master, loop, [[], [], 0]),
	register(master, PID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printPendingURLs() ->
	master ! printPendingURLs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tells the master to enqueue new URLs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getImages(URLs) ->
	master ! {getImages, URLs}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Removes duplicated URLs from incoming ones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removeDuplicatedURLs(URLsToAdd, ProcessedURLs) ->
	[URL || URL <- URLsToAdd, not lists:any(fun(ProcessedURL) -> ProcessedURL == URL end, ProcessedURLs)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Removes URLs already in databse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removeDuplicatedURLs(URLs) ->
	[URL || URL <- URLs, not epc_dba:checkURL(URL)].
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creates slaves to crawl URLs if we have available slots
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assignURLs([], Slaves, _) -> % No new URLs to assign
	{[], Slaves};

assignURLs(PendingURLs, Slaves, 0) -> % No Slave slots
	{PendingURLs, Slaves};

assignURLs([URL | T], Slaves, SlaveSlots) ->
	Slave = spawn(epc_slave, init, [ URL]),
	epc_dba:storeCrawledURL([URL]),
	assignURLs(T, [Slave | Slaves], SlaveSlots - 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets URL sublist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trim(URLs, Count) when Count =< ?PENDING_URLS_LIMIT ->
	URLs;

trim(URLs, _) ->
	lists:sublist(URLs, 1, ?PENDING_URLS_LIMIT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pritns list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printList([]) ->
	io:fwrite("~n", []);
printList([H | T]) ->
	io:fwrite("~s~n", [H]),
	printList(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Store urls in database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
storeURLs([]) ->
	ok;
storeURLs([H | T]) ->
	epc_dba:storeCrawledURL(H),
	storeURLs(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Receive messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkMessages(PendingURLs, Slaves, SlaveCount) ->
	receive
		% Debug messages
        stop ->
            ok;
        printPendingURLs ->
            printList(PendingURLs),
            loop(PendingURLs, Slaves, SlaveCount);
        % Client messages
        {foundURLs, Slave, {CrawledURLs, NewUrls}} ->
		    CurrentSlaves = lists:delete(Slave, Slaves),
			storeURLs(CrawledURLs),
		    UrlsToAdd = removeDuplicatedURLs(NewUrls), % Remove duplicated URLs already in database
		    loop( sets:to_list(sets:from_list(UrlsToAdd ++ PendingURLs)), CurrentSlaves, SlaveCount - 1);
        {getImages, URLsToAdd} ->
                NewURLs = removeDuplicatedURLs(URLsToAdd), % Remove duplicated URLs
        	L = NewURLs ++ PendingURLs,
        	CurrentRemaingURLs = trim(L, length(L)), % Trims the URL list to the maximum number of pending URLs
        	loop(CurrentRemaingURLs, Slaves, SlaveCount);
        {'EXIT', _, _} ->
        	io:fwrite("Something wrong has happened", []),
        	exit(self(), error)
    after
        100 ->
            loop(PendingURLs, Slaves, SlaveCount) % Loop forever
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(PendingURLs, Slaves, SlaveCount) when SlaveCount < ?SLAVE_LIMIT ->
	{RemainingURLs, CurrentSlaves} = assignURLs(PendingURLs, Slaves, ?SLAVE_LIMIT - SlaveCount),
	checkMessages(RemainingURLs, CurrentSlaves, length(CurrentSlaves));

loop(PendingURLs, Slaves, SlaveCount) ->
	checkMessages(PendingURLs, Slaves, SlaveCount).
