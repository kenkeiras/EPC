-module(epc_dba).
-export([test/0, install/1, uninstall/1, start/1, stop/1, put_im/2, get_im/1, get_by_hash/1]).

-record(epc_images, {url,imhash}).

% Install the schemas (DB files) on these nodes and start Mnesia on them.
% Use only once in your whole life.
install(Nodes) ->
	io:format("Installing database... "),
	ok = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, application, start, [mnesia]),
	
	mnesia:create_table(epc_images,
		[{attributes, record_info(fields, epc_images)},
		{index, [#epc_images.imhash]},
		{disc_copies, Nodes}]),
		
	rpc:multicall(Nodes, mnesia, wait_for_tables, [[epc_images], 5000]),
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
	rpc:multicall(Nodes, mnesia, wait_for_tables, [[epc_images], 5000]),
	io:format("database started.~n").


% Stops DB on these nodes.
stop(Nodes) ->
	io:format("Stopping database... "),
	{_, []} = rpc:multicall(Nodes, application, stop, [mnesia]),
	io:format("database stahped.~n").


% Writes image URL and data. Any types are valid for the parameters.
put_im(URL, Hash) ->
	mnesia:activity(async_dirty, fun()-> mnesia:write(#epc_images{url=URL, imhash=Hash}) end).


% Returns all stored data from an image URL.
get_im(URL) ->
	case mnesia:activity(async_dirty, fun()-> mnesia:read({epc_images, URL}) end) of
		[#epc_images{imhash=Hash}] -> {URL, Hash};
		[] -> not_found
	end.
			

% Returns images that got that hash.
get_by_hash(Hash) ->
	mnesia:activity(async_dirty, fun()-> mnesia:match_object(#epc_images{_='_', imhash=Hash}) end).


test() ->
	io:format("Hello world!~n"),
	DB_nodes = [node()],
	epc_dba:uninstall(DB_nodes),
	epc_dba:install(DB_nodes),
	epc_dba:put_im("http://www.meretricessinfronteras.com/personal/pepa.jpg", "0123456789abcedf"),
	epc_dba:put_im(a, h1),
	epc_dba:put_im(b, h1),
	epc_dba:put_im(c, h2),
	epc_dba:stop(DB_nodes),
	epc_dba:start(DB_nodes),
	io:format("get_im: ~p~n", [epc_dba:get_im("http://www.meretricessinfronteras.com/personal/pepa.jpg")]),
	io:format("get_by_hash(h1): ~p~n", [epc_dba:get_by_hash(h1)]),
	io:format("get_by_hash(h2): ~p~n", [epc_dba:get_by_hash(h2)]),
	io:format("get_by_hash(h3): ~p~n", [epc_dba:get_by_hash(h3)]).
