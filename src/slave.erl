-module(slave).
-export([init/1]).

init(Url) ->
    crawl(Url),
    ok.

crawl(Url)->
    {CrawledUrls, OtherDomainUrls, ImageLinks} = crawl(Url,[],[],[],[]),
    indexImages(ImageLinks),
    sendNewUrls(CrawledUrls,OtherDomainUrls).

crawl(Url,CurrentDomainNotCrawledUrls,CurrentDomainCrawledUrls,OtherDomainUrls,ImageUrls) ->
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
    string:equal(Domain,string:sub_string(String,1,string:len(Domain))). 

indexImages(ImageList) ->
    lists:map(indexer:indexImage(),ImageList). 

sendNewUrls(Crawled,NewUrls) ->
    master ! {foundURLs, self(), {Crawled,NewUrls}}.    


