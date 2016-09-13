-module(templaterl_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

simple_token_replacement_test() ->
    Result = templaterl:compile(<<"replace {{{this}}}">>, [{<<"this">>, <<"something">>}]),
    ?assertEqual(<<"replace something">>, Result).

multi_token_replacement_test() ->
    Result = templaterl:compile(<<"replace {{{this}}} & {{{that}}}">>,
                                [{<<"this">>, <<"something">>},
                                 {<<"that">>, <<"another">>}]),
    ?assertEqual(<<"replace something & another">>, Result).

incomplete_token_replacement_test() ->
    Result = templaterl:compile(<<"replace {{{this">>, []),
    ?assertEqual(bad_tag, Result).

incomplete_last_token_replacement_test() ->
    Result = templaterl:compile(<<"replace this{{{">>, []),
    ?assertEqual(bad_tag, Result).