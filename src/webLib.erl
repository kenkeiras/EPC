% 
%% webLib.erl
%%
%% @author Iago Lastra Rodriguez <iago.lastra@gmail.com>

-module(webLib).
-export([getHeaderTags/1]).

-include_lib("xmerl/include/xmerl.hrl"). 

%Retunrs title if url = "http://www.youtube.com"
getHeaderTags(URL) ->
	inets:start(),
	{_, { _, _, Data}} = httpc:request(URL),
	L = element(3,mochiweb_html:parse(Data)),
	H = lists:nth(1,L),
	E3 = element(3,H),
	lists:nth(6,E3).


