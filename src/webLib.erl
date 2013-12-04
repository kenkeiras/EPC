% 
%% webLib.erl
%%
%% @author Iago Lastra Rodriguez <iago.lastra@gmail.com>

-module(webLib).
-export([getData/1]).

-include_lib("xmerl/include/xmerl.hrl"). 

%Retunrs title if url = "http://www.youtube.com"
getData(URL) ->
	inets:start(),
	{_, { _, _, Data}} = httpc:request(URL),
	L1 = crawl_link(mochiweb_html:parse(Data),[]),
	L2 = crawl_image(mochiweb_html:parse(Data),[]),
	Links = [ V || {P, V} <- L1, P == <<"href">>],
	Images = [ V || {P, V} <- L2, P == <<"src">>],
	{Links, Images}.


crawl_link({<<"a">>, Properties, Children}, LinksList) ->
	L = lists:flatten([ crawl_link(Child, [Properties | LinksList]) || Child <- Children]),
	L;
crawl_link({_, _Properties, Children}, LinksList) ->
	L = lists:flatten([ crawl_link(Child, LinksList) || Child <- Children]),
	L;
crawl_link(_, LinksList) -> 
	LinksList.

		
crawl_image({<<"img">>, _Properties, Children}, ImageList) ->
	L = lists:flatten([ crawl_image(Child, [1 | ImageList]) || Child <- Children]),
	L;
crawl_image({_, _, Children}, ImageList) ->
	L = lists:flatten([ crawl_image(Child, ImageList) || Child <- Children]),
	L;
crawl_image(_, ImageList) -> 
	ImageList.