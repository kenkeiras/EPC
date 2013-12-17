-module(epc_dba).
-export([test/0, install/1, uninstall/1, start/1, stop/1, put_im/2, get_im/1, get_by_hash/1, get_by_simhash/2, clear/0]).

-record(epc_images, {url,hash0,hash1,hash2,hash3,hash4,hash5,hash6,hash7}).

% Install the schemas (DB files) on these nodes and start Mnesia on them.
% Use only once in your whole life.
install(Nodes) ->
	io:format("Installing database... "),
	ok = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, application, start, [mnesia]),
	
	mnesia:create_table(epc_images,
		[{attributes, record_info(fields, epc_images)},
		{index, [#epc_images.hash0, #epc_images.hash1, #epc_images.hash2, #epc_images.hash3, 
			#epc_images.hash4, #epc_images.hash5, #epc_images.hash6, #epc_images.hash7]},
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
put_im(URL, [Hash0,Hash1,Hash2,Hash3,Hash4,Hash5,Hash6,Hash7]) ->
	mnesia:activity(async_dirty, fun()-> mnesia:write(#epc_images{url=URL, hash0=Hash0, 
		hash1=Hash1, hash2=Hash2, hash3=Hash3, hash4=Hash4, hash5=Hash5, hash6=Hash6, 
		hash7=Hash7}) end).


% Returns all stored data from an image URL.
get_im(URL) ->
	case mnesia:activity(async_dirty, fun()-> mnesia:read({epc_images, URL}) end) of
		[#epc_images{hash0=Hash0, hash1=Hash1, hash2=Hash2, hash3=Hash3, hash4=Hash4, 
			hash5=Hash5, hash6=Hash6, hash7=Hash7}] -> 
			{URL, Hash0,Hash1,Hash2,Hash3,Hash4,Hash5,Hash6,Hash7};
		[] -> not_found
	end.
			

% Returns images that got that hash.
get_by_hash([Hash0,Hash1,Hash2,Hash3,Hash4,Hash5,Hash6,Hash7]) ->
	mnesia:activity(async_dirty, fun()-> mnesia:match_object(#epc_images{_='_', hash0=Hash0, 
		hash1=Hash1, hash2=Hash2, hash3=Hash3, hash4=Hash4, hash5=Hash5, hash6=Hash6, 
		hash7=Hash7}) end).
		

get_by_simhash([Hash0,Hash1,Hash2,Hash3,Hash4,Hash5,Hash6,Hash7], Threshold) ->
	mnesia:activity(async_dirty, fun() -> mnesia:select(epc_images, [{#epc_images{hash0='$0', 
		hash1='$1', hash2='$2', hash3='$3', hash4='$4', hash5='$5', hash6='$6', hash7='$7', url='$8'},
		%[{'=<', {abs, {'-', Hash0, '$0'}}, Threshold}],
		[{'=<', {'+', 
			{'+', 
				{'+', {abs, {'-', Hash0, '$0'}}, {abs, {'-', Hash1, '$1'}}},
				{'+', {abs, {'-', Hash2, '$2'}}, {abs, {'-', Hash3, '$3'}}}
			},
			{'+', 
				{'+', {abs, {'-', Hash4, '$4'}}, {abs, {'-', Hash5, '$5'}}},
				{'+', {abs, {'-', Hash6, '$6'}}, {abs, {'-', Hash7, '$7'}}}
			}}, 
			Threshold}],
		[{{'$8', '$0', '$1', '$2', '$3', '$4', '$5', '$6', '$7'}}]}]) end).


clear() ->
	mnesia:clear_table(epc_images).


test() ->
	io:format("Hello world!~n"),
	DB_nodes = [node()],
	epc_dba:uninstall(DB_nodes),
	epc_dba:install(DB_nodes),
	epc_dba:put_im("http://www.meretricessinfronteras.com/personal/pepa.jpg", [1,2,3,4,5,6,7,8]),
	epc_dba:put_im(a, [10,11,12,13,14,15,16,17]),
	epc_dba:put_im(b, [10,11,12,13,14,15,16,17]),
	epc_dba:put_im(c, [17,16,15,14,13,12,11,10]),
	epc_dba:stop(DB_nodes),
	epc_dba:start(DB_nodes),
	io:format("get_im: ~p~n", [epc_dba:get_im("http://www.meretricessinfronteras.com/personal/pepa.jpg")]),
	io:format("get_by_hash([10,11,12,13,14,15,16,17]): ~p~n", [epc_dba:get_by_hash([10,11,12,13,14,15,16,17])]),
	io:format("get_by_hash([17,16,15,14,13,12,11,10]): ~p~n", [epc_dba:get_by_hash([17,16,15,14,13,12,11,10])]),
	io:format("get_by_hash([23,16,15,14,13,12,11,10]): ~p~n", [epc_dba:get_by_hash([23,16,15,14,13,12,11,10])]),
	io:format("get_by_simhash([23,16,15,14,13,12,11,10],10): ~p~n", [epc_dba:get_by_simhash([23,16,15,14,13,12,11,10], 10)]),
	io:format("get_by_simhash([9,11,15,13,14,14,14,20], 10)): ~p~n", [epc_dba:get_by_simhash([9,11,15,13,14,14,14,20], 10)]),
	io:format("get_by_simhash([8,11,15,13,14,14,14,20], 10)): ~p~n", [epc_dba:get_by_simhash([8,11,15,13,14,14,14,20], 10)]),
	io:format("get_by_simhash([8,11,15,13,14,14,14,20], 11): ~p~n", [epc_dba:get_by_simhash([8,11,15,13,14,14,14,20], 11)]).
