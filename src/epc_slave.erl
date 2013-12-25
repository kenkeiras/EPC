-module(epc_slave).
-export([init/1]).

-define(MAX_PAGES_DOMAIN, 20).

init(Url) ->
    % Stop when master is done
    link(whereis(master)),
    process_flag(trap_exit, true),
    crawl(Url),
    ok.


crawl(Url)->
    {CrawledUrls, OtherDomainUrls, Images} = crawl(Url,[],[],[],[], 0),
    indexImages(Images),
    sendNewUrls(CrawledUrls,OtherDomainUrls).

crawl(Url,CurrentDomainNotCrawledUrls,CurrentDomainCrawledUrls,OtherDomainUrls,Images, LinkCount) when LinkCount < ?MAX_PAGES_DOMAIN ->
	Links = web_lib:get_data(Url),
    case Links of
	    {[], []} ->  {CurrentDomainCrawledUrls,OtherDomainUrls, Images };
		{WebLinks,NewImages} ->
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
                   {[Url | CurrentDomainCrawledUrls], OtherDomainUrls ++ OtherUrls, Images ++ NewImages }
           after
               0 ->
                   crawl(Head,Tail,[Head | CurrentDomainCrawledUrls], OtherDomainUrls ++ OtherUrls, Images ++ NewImages, LinkCount + 1)
           end;
        _ ->
            {[Url | CurrentDomainCrawledUrls], OtherDomainUrls ++ OtherUrls, Images ++ NewImages }
    end

    end;
crawl(_Url,_CurrentDomainNotCrawledUrls,CurrentDomainCrawledUrls,OtherDomainUrls,Images, _LinkCount) ->
    {CurrentDomainCrawledUrls, OtherDomainUrls, Images }.


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
