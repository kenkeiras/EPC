%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% master:init().
% master:getImages(URLs). 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(master).

%% Interface
-export([init/0, getImages/1]).

%% Internal Exports
-export([loop/1]).

-define(SLAVE_LIMIT, 50).

-define(PENDING_URLS_LIMIT, 100).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init() ->
	process_flag(trap_exit, true),
	PID = spawn(master, loop, [[], [], [], 0]),
	register(master, PID).
    

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
% Creates slaves to crawl URLs if we have available slots
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assignURLs([], ProcessedURLs, Slaves, _) -> % No new URLs to assign
	{[], ProcessedURLs, Slaves};
	
assignURLs(PendingURLs, ProcessedURLs, Slaves, 0) -> % No Slave slots
	{PendingURLs, ProcessedURLs, Slaves};
	
assignURLs([URL | T], ProcessedURLs, Slaves, SlaveSlots) ->
	Slave = spawn(slave, init, [TODO]),
	slave:crawlURL(Slave, URL),
	assignURLs(T, [URL | ProcessedURLs], [Slave | Slaves], SlaveSlots - 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gets URL sublist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trim(URLs, Count) -> when Count =< PENDING_URLS_LIMIT
	URLs;
trim(URLs, Count) ->
	lists:sublist(URLs, 1, PENDING_URLS_LIMIT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Receive messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkMessages(PendingURLs, ProcessedURLs, Slaves, SlaveCount) ->
	receive
        stop ->
            ok;
        {getImages, URLsToAdd} ->
        	newURLs = removeDuplicatedDomains(URLsToAdd, ProcessedURLs), % Remove duplicated URLs
        	L = newURLs ++ RemainingURL,
        	currentRemaningURLs = trim(L, length(L)), % Trims the URL list to the maximum number of pending URLs
        	loop(currentRemaningURLs, ProcessedURLs, Slaves), SlaveCount;
        {'EXIT', _, _} ->
        	io:fwrite("Something wrong has happened", []),
        	exit(self(), error)
    after
        100 ->
            loop() % Loop forever
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Main loop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(PendingURLs, ProcessedURLs, Slaves, SlaveCount) when SlaveCount < SLAVE_LIMIT ->
	{RemainingURLs, CurrentProcessedURLs, CurrentSlaves} = assignURLs(PendingURLs, ProcessedURLs, Slaves, SLAVE_LIMIT - SlaveCount),
	checkMessages(RemainingURLs, CurrentProcessedURLs, CurrentSlaves, length(CurrentSlaves));
	
loop(PendingURLs, ProcessedURLs, Slaves, SlaveCount) ->
	checkMessages(PendingURLs, ProcessedURLs, Slaves, SlaveCount).
		
    

