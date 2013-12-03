-module(slave).
-export([init/0, crawlURL/1]).
-export([loop/0, indexImageList/1]).

crawlURL(WebUrl) ->
    slave ! {self(), {crawl, ImageUrl}}.
	
indexImageURL(ImageUrlList) ->
    slave ! {self(), {index, ImageUrlList}}.

loop() ->
    receive
        {_From, {crawl, webUrl}} ->
            webLib:getImages(webUrl),
            loop();
		    {_From, {index, ImageUrlList}} ->
            indexImageList(indexImageList),
            loop()
    end.

indexImageList([ImageUrl|T]) ->
    indexer:indexImage(ImageUrl), 
	indexImageList(T);
indexImageList([ImageUrl|[]]) ->
    indexer:indexImage(ImageUrl).

slave() ->
   register(slave, self()),
   loop().

init() ->
    spawn(?MODULE, slave, []),
    ok.
