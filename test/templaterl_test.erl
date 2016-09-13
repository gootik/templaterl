-module(templaterl_test).

-compile([export_all, inline]).

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
    Uppercase = "uppercase(_Token, Value) -> <<<<(string:to_upper(X))>> || <<X>> <= Value>>.",
    templaterl:register_helpers([{uppercase, Uppercase}]),

    Result = templaterl:compile(<<"replace {{{uppercase this}}}">>, [
        {<<"this">>, <<"test">>}]),
    ?assertEqual(<<"replace TEST">>, Result).

multi_function_token_replacement_test() ->
    Uppercase = "uppercase(_Token, Value) -> <<<<(string:to_upper(X))>> || <<X>> <= Value>>.",
    Lowercase = "lowercase(_Token, Value) -> <<<<(string:to_lower(X))>> || <<X>> <= Value>>.",

    templaterl:register_helpers([{uppercase, Uppercase},
                                 {lowercase, Lowercase}]),

    Result = templaterl:compile(<<"replace {{{uppercase this}}} and {{{lowercase that}}}">>, [
        {<<"this">>, <<"test">>},
        {<<"that">>, <<"TEsT2">>}]),
    ?assertEqual(<<"replace TEST and test2">>, Result).

nested_function_token_replacement_test() ->
    Uppercase = "uppercase(_Token, Value) -> <<<<(string:to_upper(X))>> || <<X>> <= Value>>.",
    Lowercase = "lowercase(_Token, Value) -> <<<<(string:to_lower(X))>> || <<X>> <= Value>>.",

    templaterl:register_helpers([{uppercase, Uppercase},
                                 {lowercase, Lowercase}]),

    Result = templaterl:compile(<<"replace {{{uppercase (lowercase (uppercase this))}}}">>, [{<<"this">>, <<"test">>}]),
    ?assertEqual(<<"replace TEST">>, Result).

readme_test() ->
    Result = templaterl:compile(<<"I have a {{{car_model}}}.">>, [{<<"car_model">>, <<"Nissan GTR">>}]),
    ?assertEqual(<<"I have a Nissan GTR.">>, Result).

readme_test2() ->
    Uppercase = "uppercase(_Token, Value) -> <<<<(string:to_upper(X))>> || <<X>> <= Value>>.",
    templaterl:register_helpers([{uppercase, Uppercase}]),

    Uppercase = fun(_Token, Value) -> <<<<(string:to_upper(X))>> || <<X>> <= Value>> end,
    Result = templaterl:compile(<<"I have a {{{uppercase car_model}}}.">>, [{<<"car_model">>, <<"Nissan GTR">>}]),
    ?assertEqual(<<"I have a NISSAN GTR.">>, Result).

non_binary_values_test() ->
    Result = templaterl:compile(<<"replace {{{number}}} {{{string}}} {{{atom}}} {{{boolean}}} {{{binary}}}">>,
                                [{<<"number">>, 2},
                                 {<<"string">>, "string"},
                                 {<<"atom">>, atom},
                                 {<<"boolean">>, true},
                                 {<<"binary">>, <<"binary">>}]),

    ?assertEqual(<<"replace 2 string atom true binary">>, Result).