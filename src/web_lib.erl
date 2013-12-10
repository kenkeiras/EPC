%% 
%% web_lib.erl Module to extract info from URL resources
%%
%% @author Iago Lastra Rodriguez <iago.lastra@gmail.com>
%% @reviewer Laura Castro <laura.castro@gmail.com>
 
-module(web_lib).
-export([get_data/1]).
 
-include_lib("xmerl/include/xmerl.hrl"). 
 
%% @doc Returns a list of tuples containin links and images.
%%      Example url = "http://www.youtube.com/{relative}"
%% @spec get_data(URL :: string()) -> {Links :: string(), Images :: string()}
get_data(URL) ->
    inets:start(),
    {_, { _, _, Data}} = httpc:request(URL),
    ParsedData = mochiweb_html:parse(Data),
    Links = crawl_for(<<"a">>, ParsedData, []),
    Images = crawl_for(<<"img">>, ParsedData, []),
    LinkURLs = filter_second_by(<<"href">>, Links),
    ImageURLs = filter_second_by(<<"src">>, Images),
    {relative_to_abs(LinkURLs, URL), relative_to_abs(ImageURLs, URL)}.
 
%% @doc Internal utility function
crawl_for(Tag, {Tag, Properties, Children}, List) ->
    L = lists:flatten([ crawl_for(Tag, Child, [Properties | List]) || Child <- Children]),
    [Properties | L];
crawl_for(Tag, {_DifferentTag, _Properties, Children}, List) ->
    lists:flatten([ crawl_for(Tag, Child, List) || Child <- Children]);
crawl_for(_Tag, _DifferentElement, List) -> 
    List.
 
%% @doc Internal utility function
filter_second_by(Tag, List) ->
    [ Second || {First, Second} <- List, First == Tag ].


% @doc gets a binary link and if starts with slash, appends Base url
add_domain(RelativeUrl, BaseUrl) ->
    Link =  binary:bin_to_list(RelativeUrl),
    case lists:nth(1, Link) of
        $/ -> BaseUrl ++ Link;      %Starts with slash
        CompleteUrl -> CompleteUrl  %Is a complete url.
    end
    .

%% @doc transforms relative links into absolute links
relative_to_abs(LinkUrls, URL) ->
    [add_domain(CurrentURL, URL) || CurrentURL <- LinkUrls].

