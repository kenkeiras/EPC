-module(epc_slave).
-export([init/1]).

init(Url) ->
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
    {CrawledUrls, OtherDomainUrls, ImageLinks} = crawl(Url,[],[],[],[]),
    indexImages(ImageLinks),
    sendNewUrls(CrawledUrls,OtherDomainUrls).

crawl(Url,CurrentDomainNotCrawledUrls,CurrentDomainCrawledUrls,OtherDomainUrls,ImageUrls) ->
	Links = getLinks(Url),
    case Links of
	    {[], []} ->  {CurrentDomainCrawledUrls,OtherDomainUrls, ImageUrls };
		{WebLinks,NewImageUrls} -> 
   CurrentDomainUrls = [ Link || Link <- WebLinks, belongsToDomain(Link,Url)],
   OtherUrls = [ Link || Link <- WebLinks, not belongsToDomain(Link,Url)],
   % TODO REMOVE DUPLICATES
   CurrentDomainNotCrawledUrls =  CurrentDomainNotCrawledUrls ++ CurrentDomainUrls,

   case CurrentDomainNotCrawledUrls of
        [Head | Tail] ->
            crawl(Head,Tail,[Head | CurrentDomainCrawledUrls], OtherDomainUrls ++ OtherUrls, ImageUrls ++ NewImageUrls);
        [] ->
            {[Url | CurrentDomainCrawledUrls], OtherDomainUrls ++ OtherUrls, ImageUrls ++ NewImageUrls }
    end

    end.

% Depends on  the input format(Lastra) + parser
belongsToDomain(String,Domain) ->
    string:equal(Domain,string:sub_string(String,1,string:len(Domain))).

indexImages(ImageList) ->
    %io:format("~p~n", [ImageList]),
    lists:map(fun (X) -> indexer:indexImage(X) end,ImageList).

sendNewUrls(Crawled,NewUrls) ->
    %io:format("~p~n", [NewUrls]),
    master ! {foundURLs, self(), {Crawled, NewUrls}}.
