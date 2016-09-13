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

function_token_replacement_test() ->
    Uppercase = fun(_, Value) -> << <<(string:to_upper(X))>> || <<X>> <= Value >> end,
    Result = templaterl:compile(<<"replace {{{uppercase this}}}">>, [
        {<<"this">>, <<"test">>},
        {<<"uppercase">>, Uppercase}
    ]),
    ?assertEqual(<<"replace TEST">>, Result).

multi_function_token_replacement_test() ->
    Uppercase = fun(_, Value) -> << <<(string:to_upper(X))>> || <<X>> <= Value >> end,
    Lowercase = fun(_, Value) -> << <<(string:to_lower(X))>> || <<X>> <= Value >> end,
    Result = templaterl:compile(<<"replace {{{uppercase this}}} and {{{lowercase that}}}">>, [
        {<<"this">>, <<"test">>},
        {<<"that">>, <<"TEsT2">>},
        {<<"uppercase">>, Uppercase},
        {<<"lowercase">>, Lowercase}
    ]),
    ?assertEqual(<<"replace TEST and test2">>, Result).


nested_function_token_replacement_test() ->
    Uppercase = fun(_, Value) -> << <<(string:to_upper(X))>> || <<X>> <= Value >> end,
    Lowercase = fun(_, Value) -> << <<(string:to_lower(X))>> || <<X>> <= Value >> end,
    Result = templaterl:compile(<<"replace {{{uppercase (lowercase (uppercase this))}}}">>, [
        {<<"this">>, <<"test">>},
        {<<"uppercase">>, Uppercase},
        {<<"lowercase">>, Lowercase}
    ]),
    ?assertEqual(<<"replace TEST">>, Result).