-module(epc_slave).
-export([init/1]).

-define(MAX_PAGES_DOMAIN, 20).

init(Url) ->
    % Stop when master is done
    link(whereis(master)),
    process_flag(trap_exit, true),
    crawl(Url),
    ok.

getLinks(URL) ->
    try web_lib:get_data(URL) of
	{WebLinks,NewImageUrls} ->
	    {WebLinks,NewImageUrls}
    catch _:_ ->
	    {[], []}
    end.


crawl(Url)->
    {CrawledUrls, OtherDomainUrls, ImageLinks} = crawl(Url,[],[],[],[], 0),
    indexImages(ImageLinks),
    sendNewUrls(CrawledUrls,OtherDomainUrls).

crawl(Url,CurrentDomainNotCrawledUrls,CurrentDomainCrawledUrls,OtherDomainUrls,ImageUrls, LinkCount) when LinkCount < ?MAX_PAGES_DOMAIN ->
	Links = getLinks(Url),
    case Links of
	    {[], []} ->  {CurrentDomainCrawledUrls,OtherDomainUrls, ImageUrls };
		{WebLinks,NewImageUrls} ->
   CurrentDomainUrls = [ Link || Link <- WebLinks, belongsToDomain(Link,Url)],
   OtherUrls = [ Link || Link <- WebLinks, not belongsToDomain(Link,Url)],
   % TODO REMOVE DUPLICATES
   PendingURLs =  CurrentDomainNotCrawledUrls ++ master:removeDuplicatedURLs(CurrentDomainUrls,CurrentDomainCrawledUrls),

   case PendingURLs of
        [Head | Tail] ->
           % Only continue crawling if master works
           receive
               {'EXIT', Pid, Reason} ->
                   % Keep the info around for the moment of sending the data to the master.
                   self() ! {'EXIT', Pid, Reason},
                   {[Url | CurrentDomainCrawledUrls], OtherDomainUrls ++ OtherUrls, ImageUrls ++ NewImageUrls }
           after
               0 ->
                   crawl(Head,Tail,[Head | CurrentDomainCrawledUrls], OtherDomainUrls ++ OtherUrls, ImageUrls ++ NewImageUrls, LinkCount + 1)
           end;
        _ ->
            {[Url | CurrentDomainCrawledUrls], OtherDomainUrls ++ OtherUrls, ImageUrls ++ NewImageUrls }
    end

    end;
crawl(_Url,_CurrentDomainNotCrawledUrls,CurrentDomainCrawledUrls,OtherDomainUrls,ImageUrls, _LinkCount) ->
    {CurrentDomainCrawledUrls, OtherDomainUrls, ImageUrls }.


% Not best way of doing this
belongsToDomain(String,Domain) ->
    string:equal(Domain,string:sub_string(String,1,string:len(Domain))).

indexImages(ImageList) ->
    %master:printList(ImageList),
    lists:map(fun (X) -> indexer:indexImage(X) end,ImageList).

sendNewUrls(Crawled,NewUrls) ->
    %io:format("~p~n", [NewUrls]),
    receive
        {'EXIT', _Pid, _Reason} ->
            ok
    after
        0 ->
            master ! {foundURLs, self(), {Crawled, NewUrls}}
    end.
