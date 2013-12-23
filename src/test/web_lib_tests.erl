-module(web_lib_tests).

-include_lib("eunit/include/eunit.hrl").

add_domain_relative_file_in_path_test() ->
    "http://domain.tld/path/file" = web_lib:add_domain(
                                      <<"file">>, "http://domain.tld/path/").

add_domain_relative_another_file_in_path_test() ->
    "http://domain.tld/path/file" = web_lib:add_domain(
                                      <<"file">>,
                                      "http://domain.tld/path/other_file").
