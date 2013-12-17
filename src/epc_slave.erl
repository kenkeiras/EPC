-module(epc_slave).
-export([init/1]).

init(Url) ->
    crawl(Url),
    ok.

crawl(Url)->
    {CrawledUrls, OtherDomainUrls, ImageLinks} = crawl(Url,[],[],[],[]),
    indexImages(ImageLinks),
    sendNewUrls(CrawledUrls,OtherDomainUrls).

crawl(Url,CurrentDomainNotCrawledUrls,CurrentDomainCrawledUrls,OtherDomainUrls,ImageUrls) ->
    io:format("= ~p~n", [Url]),
   {WebLinks,NewImageUrls} = web_lib:get_data(Url),
   CurrentDomainUrls = [ Link || Link <- WebLinks, belongsToDomain(Link,Url)],
   OtherUrls = [ Link || Link <- WebLinks, not belongsToDomain(Link,Url)],
   CurrentDomainNotCrawledUrls =  CurrentDomainNotCrawledUrls ++ master:removeDuplicatedURLs(CurrentDomainUrls,CurrentDomainCrawledUrls),

   case CurrentDomainNotCrawledUrls of
        [Head | Tail] ->
            crawl(Head,Tail,[Head | CurrentDomainCrawledUrls],[OtherDomainUrls ++ OtherUrls],[ImageUrls ++ NewImageUrls]);
        [] ->
            {[Url | CurrentDomainCrawledUrls],[OtherDomainUrls ++ OtherUrls], [ImageUrls ++ NewImageUrls] }
    end.

% Depends on  the input format(Lastra) + parser
belongsToDomain(String,Domain) ->
    io:format("::~p - ~p~n", [String, Domain]),
    string:equal(Domain,string:sub_string(String,1,string:len(Domain))).

indexImages(ImageList) ->
    io:format("~p~n", [ImageList]),
    lists:map(fun (X) -> indexer:indexImage(X) end,ImageList).

sendNewUrls(Crawled,NewUrls) ->
    master ! {foundURLs, self(), {Crawled,NewUrls}}.
