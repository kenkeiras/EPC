%%
%% web_lib.erl Module to extract info from URL resources
%%
%% @author Iago Lastra Rodriguez <iago.lastra@gmail.com>
%% @reviewer Laura Castro <laura.castro@gmail.com>

-module(web_lib).
-export([get_data/1, downloadImage/1]).

-include_lib("xmerl/include/xmerl.hrl").
-export([add_domain/2]).

%% @doc Returns a list of tuples containin links and images.
%%      Example url = "http://www.youtube.com/{relative}"
%% @spec get_data(URL :: string()) -> {Links :: string(), Images :: string()}
get_data(URL) ->
    inets:start(),
    % {_, { _, _, Data}} = httpc:request(URL)
    % This can cause web_lib to throw a "no match" exception:
    % {error,{failed_connect,[{to_address,{"www.asdfasdf.es", 80}}, {inet,[inet],nxdomain}]}}
    Response = httpc:request(URL),
    case Response of
	{ok, {_ResponseCode, ResponseHeaders, ResponseBody}} -> 
		parseResponse(URL, ResponseHeaders, ResponseBody);
	_ -> 
		{[], []}		
    end.


parseResponse(URL, ResponseHeaders, ResponseBody) ->
	case lists:keyfind("content-type", 1, ResponseHeaders) of
		{"content-type", ContentType} ->
			case string:str(ContentType, "text/html") of % check that we have a web page
				0 -> 
					{[], []};
				_ ->
					ParsedData = mochiweb_html:parse(ResponseBody),
					Anchors = crawl_for(<<"a">>, ParsedData, []),
					Iframes = crawl_for(<<"iframe">>, ParsedData, []),
					Frames = crawl_for(<<"frame">>, ParsedData, []),
					ImageTags = crawl_for(<<"img">>, ParsedData, []),
					LinkURLs = filter_second_by(<<"href">>, Anchors) ++ filter_second_by(<<"src">>, Iframes) ++ filter_second_by(<<"src">>, Frames),
					ImageURLs = filter_second_by(<<"src">>, ImageTags),
					ImageURLsAbs = relative_to_abs(ImageURLs, URL),
					{relative_to_abs(LinkURLs, URL), ImageURLsAbs}
			end;
		_ -> 
			{[], []}
	end.
			

downloadImage(URL) ->
	ImageTypes = ["image/jpg", "image/jpeg", "image/png"], % IMPORTANT NOTE, AS FAR AS I KNOW PHASH ONLY SUPPORTS JPEGs AND PNGs
	Response = httpc:request(URL),
	case Response of
		{ok, {_ResponseCode, ResponseHeaders, ResponseBody}} ->
			case lists:keyfind("content-type", 1, ResponseHeaders) of
				{"content-type", ContentType} ->
					case length([Type || Type <- ImageTypes, 0 =/= string:str(ContentType, Type)]) of
						0 -> noImage;
						_ -> ResponseBody
					end;
			_ -> noImage
			end;
		_ -> noImage
	end.
						
	
	

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


get_domain(Url) ->
    {_ , NS} = lists:splitwith(fun (X) -> X /= $: end, Url),
    {_, ADom} = lists:splitwith(fun (X) -> (X == $/) or (X == $:) end, NS),
    {Domain, _} = lists:splitwith(fun (X) -> X /= $/ end, ADom),
    Domain.

% @doc gets a binary link and if starts with slash, appends Base url
add_domain(RelativeUrl, BaseUrl) ->
    Link =  binary:bin_to_list(RelativeUrl),
    % andalso evaluates using short circuit
    Schemeless = ((length(Link) > 1) andalso
                  (lists:nth(1, Link) == $/) and
                  (lists:nth(2, Link) == $/)),
    FullyAbsolute = lists:any(fun (X) -> X == $: end, Link),
    AbsolutePath = (length(Link) > 0) andalso (lists:nth(1, Link) == $/),
    if Schemeless ->
            {Scheme, _} = lists:splitwith(fun (X) -> X /= $: end,
                                          BaseUrl),
            Scheme ++ ":" ++ Link;
       FullyAbsolute ->
            Link;  %Is a complete url
       AbsolutePath ->
            {Scheme, _} = lists:splitwith(fun (X) -> X /= $: end,
                                          BaseUrl),
            Scheme ++ "://" ++ get_domain(BaseUrl) ++ Link; %Starts with slash
       true ->
            {_, S} = lists:splitwith(fun (X) -> X /= $/ end,
                                lists:reverse(BaseUrl)),

            lists:reverse(S) ++ Link

    end.

%% @doc transforms relative links into absolute links
relative_to_abs(LinkUrls, URL) ->
    [add_domain(CurrentURL, URL) || CurrentURL <- LinkUrls].
