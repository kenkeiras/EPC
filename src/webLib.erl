% 
%% webLib.erl
%%
%% @author Iago Lastra Rodriguez <iago.lastra@gmail.com>

-module(webLib).
-export([getBody/1, parse/1]).

-include_lib("xmerl/include/xmerl.hrl"). 

getBody(URL) ->
	inets:start(),
	{Status,{Response,Headers,Body}} = httpc:request("http://www.google.com"),
	Body.


parse(Body) ->
	Test = "
		<Bookstore>
		  <Book>
		    <ISBN>9781401309657</ISBN>
		    <Name>The Last Letcture</Name>
		    <Author>Randy Pausch</Author>
		  </Book>
		</Bookstore>",

	{Xml, _} = xmerl_scan:string(Test),
    [val(xmerl_xpath:string("//ISBN", Xml)),
     val(xmerl_xpath:string("//Name", Xml)),
     val(xmerl_xpath:string("//Author", Xml))
    ].


val(X) ->
    [#xmlElement{ name = Name, content=[#xmlText{value=Value} | _ ]}] = X,
    {N, V}.