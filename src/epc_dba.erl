-module(epc_dba).
-include_lib("stdlib/include/qlc.hrl").
-export([test/0, install/1, uninstall/1, start/1, stop/1, put_im/2, get_im/1,
         get_by_hash/1, get_by_simhash/1, clear/0, get_crawled_url_count/0,
         get_indexed_image_count/0]).

-export([hammingDistance/2]). % Just for testing reasons

-export([storeCrawledURL/1, checkURL/1]).

-record(epc_images, {url, image_hash}).
-record(epc_crawled_urls, {url, crawl_date}). % Crawl again when date too old

% Install the schemas (DB files) on these nodes and start Mnesia on them.
% Use only once in your whole life.
install(Nodes) ->
	io:format("Installing database... "),
	ok = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, application, start, [mnesia]),
	
	mnesia:create_table(epc_images,
		[{attributes, record_info(fields, epc_images)},
		{index, [#epc_images.image_hash]},
		{disc_copies, Nodes}]),
		
	mnesia:create_table(epc_crawled_urls,
		[{attributes, record_info(fields, epc_crawled_urls)},
		{index, [#epc_crawled_urls.crawl_date]},
		{disc_copies, Nodes}]),
	
	rpc:multicall(Nodes, mnesia, wait_for_tables, [[epc_images, epc_crawled_urls], 5000]),
	io:format("done installing.~n").


% Permanently delete the DB on these nodes. Don't use it. Ever.
uninstall(Nodes) ->
	io:format("Removing database... "),
	rpc:multicall(Nodes, application, stop, [mnesia]),
	ok = mnesia:delete_schema(Nodes),
	io:format("done removing.~n").


% Starts DB on these nodes. Run once at the beggining of your program.
start(Nodes) ->
	io:format("Starting database... "),
	{_, []} = rpc:multicall(Nodes, application, start, [mnesia]),
	rpc:multicall(Nodes, mnesia, wait_for_tables, [[epc_images, epc_crawled_urls], 5000]),
	io:format("database started.~n").


% Stops DB on these nodes.
stop(Nodes) ->
	io:format("Stopping database... "),
	{_, []} = rpc:multicall(Nodes, application, stop, [mnesia]),
	io:format("database stahped.~n").


storeCrawledURL(URL) ->
	D = date(),
	mnesia:activity(async_dirty, fun()-> mnesia:write(#epc_crawled_urls{url=URL, crawl_date=D}) end).


checkURL(URL) ->
	case mnesia:activity(async_dirty, fun()-> mnesia:read({epc_crawled_urls, URL}) end) of
		[#epc_crawled_urls{crawl_date=_}] -> true;
		[] -> false
	end.


% Writes image URL and data. Any types are valid for the parameters.
put_im(URL, Hash) ->
    %[Hash0,Hash1,Hash2,Hash3,Hash4,Hash5,Hash6,Hash7] = splitHash(Hash),
	mnesia:activity(async_dirty, fun()-> mnesia:write(#epc_images{url=URL, image_hash = Hash}) end).


% Returns all stored data from an image URL.
get_im(URL) ->
	case mnesia:activity(async_dirty, fun()-> mnesia:read({epc_images, URL}) end) of
		[#epc_images{image_hash = Hash}] -> 
			{URL, Hash};
		[] -> not_found
	end.
			

% Returns images that got that hash.
get_by_hash(Hash) ->
	mnesia:activity(async_dirty, fun()-> mnesia:match_object(#epc_images{_='_', image_hash=Hash}) end).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hamming distance (count of different bits between hashes)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
hammingDistance(A, B) ->
	hammingDistance(A, B, 0).
	
hammingDistance(<<>>, <<>>, Distance) ->
	Distance;
	
hammingDistance(<<BitA:1,TA/bits>>, <<BitB:1,TB/bits>>, Distance) ->
	case BitA == BitB of
		true -> hammingDistance(TA, TB, Distance);
		_ -> hammingDistance(TA, TB, Distance + 1)
	end;

hammingDistance(_, _, Distance) -> % This should never be reached (A and B have different length)
	Distance.

% Get all the images with similarity under the Threshold parameter. Returns a list of 10-element tuples:
% { URL: The URL of the image.
%	Hash0,Hash1,Hash2,Hash3,Hash4,Hash5,Hash6,Hash7: The eight bytes of the hash.
%   Similarity: A numerical difference between the given hash and the one from above.}
% The returned list is sorted by the Similarity value.
% qlc:sort(qlc:q( [ImageRecord#epc_images.image_hash || ImageRecord <- mnesia:table(epc_images)], {unique, true}))
get_by_simhash(Hash) ->	

	Q = qlc:q( [{ImageRecord#epc_images.url, ImageRecord#epc_images.image_hash} || ImageRecord <- mnesia:table(epc_images)], {unique, true}),
	
	Order = fun(A, B) ->
		{_, HashA} = A,
		{_, HashB} = B,
    	hammingDistance(Hash, HashA) 
    	< 
    	hammingDistance(Hash, HashB)
	end,
	
	Query = qlc:sort(Q, [{order, Order}]),
	
	F = fun() -> qlc:eval(Query) end,
	
    {atomic, Val} = mnesia:transaction(F),
    
    [URL || {URL, _} <- Val].

% @doc Return the number of crawled urls
get_crawled_url_count() ->
    mnesia:table_info(epc_crawled_urls, size).

% @doc Return the number of images in the table
get_indexed_image_count() ->
    mnesia:table_info(epc_images, size).

clear() ->
	mnesia:clear_table(epc_images).

% Tests are not updated with the new interfaces (numeric hashes, not lists)
test() ->
	io:format("Hello world!~n"),
	DB_nodes = [node()],
	epc_dba:uninstall(DB_nodes),
	epc_dba:install(DB_nodes),
	epc_dba:put_im("http://www.meretricessinfronteras.com/personal/pepa.jpg", [1,2,3,4,5,6,7,8]),
	epc_dba:put_im(a, [10,11,12,13,14,15,16,17]),
	epc_dba:put_im(b, [10,11,12,13,14,15,16,17]),
	epc_dba:put_im(c, [17,16,15,14,13,12,11,10]),
	epc_dba:put_im(d, [12,11,12,13,14,15,16,19]),
	epc_dba:put_im(e, [20,11,12,13,14,15,16,30]),
	epc_dba:stop(DB_nodes),
	epc_dba:start(DB_nodes),
	io:format("get_im: ~p~n", [epc_dba:get_im("http://www.meretricessinfronteras.com/personal/pepa.jpg")]),
	io:format("get_by_hash([10,11,12,13,14,15,16,17]): ~p~n", [epc_dba:get_by_hash([10,11,12,13,14,15,16,17])]),
	io:format("get_by_hash([17,16,15,14,13,12,11,10]): ~p~n", [epc_dba:get_by_hash([17,16,15,14,13,12,11,10])]),
	io:format("get_by_hash([23,16,15,14,13,12,11,10]): ~p~n", [epc_dba:get_by_hash([23,16,15,14,13,12,11,10])]),
	io:format("get_by_simhash([23,16,15,14,13,12,11,10],10): ~p~n", [epc_dba:get_by_simhash([23,16,15,14,13,12,11,10], 10)]),
	io:format("get_by_simhash([9,11,15,13,14,14,14,20], 10)): ~p~n", [epc_dba:get_by_simhash([9,11,15,13,14,14,14,20], 10)]),
	io:format("get_by_simhash([8,11,15,13,14,14,14,20], 10)): ~p~n", [epc_dba:get_by_simhash([8,11,15,13,14,14,14,20], 10)]),
	io:format("get_by_simhash([8,11,15,13,14,14,14,20], 11): ~p~n", [epc_dba:get_by_simhash([8,11,15,13,14,14,14,20], 11)]),
	io:format("get_by_simhash([15,11,12,13,14,15,16,17], 30): ~p~n", [epc_dba:get_by_simhash([15,11,12,13,14,15,16,17], 30)]).
