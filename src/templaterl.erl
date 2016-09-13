%%--------------------------------------------------------------------
%% @doc Templaterl is a very basic inline templating engine that
%%      supports token replacement as well as custom expressions
%%      on the token values.
%%
%%      For example you can do the following:
%%
%%      Uppercase = fun(Token, Value) -> << <<(string:to_upper(X))>> || <<X>> <= Value >>.
%%      templaterl:compile(<<"This bitstring has a {{{tag}}} that can be uppercase {{{uppercase tag}}} too.">>,
%%                         #{<<"tag">> => <<"token">>},
%%                         #{<<"uppercase">> => Uppercase}).
%%          => <<"This bitstring has a token that can be uppercase TOKEN too.">>
%%
%% @end
%%--------------------------------------------------------------------
-module(templaterl).

-compile(inline).

-export([
    compile/2
]).

-type token_list() :: [{bitstring(), bitstring()}].

%%====================================================================
%% API functions
%%====================================================================
-spec compile(bitstring(), token_list()) -> bitstring().
compile(Bin, Tokens) when is_bitstring(Bin) andalso is_list(Tokens) ->
    parse_and_replace(Bin, Tokens, <<>>).

%%====================================================================
%% Internal functions
%%====================================================================
parse_and_replace(<<>>, _, Acc) ->
    Acc;

parse_and_replace(Bin, Tokens, Acc) ->
    case binary:split(Bin, <<"{{{">>) of
        [Before, After] ->
            parse_and_replace2(After, Tokens, <<Acc/binary, Before/binary>>);
        [Rest] ->
            <<Acc/binary, Rest/binary>>
    end.

parse_and_replace2(<<>>, _, _) ->
    bad_tag;

parse_and_replace2(Bin, Tokens, Acc) ->
    case binary:split(Bin, <<"}}}">>) of
        [Token, Rest] ->
            Value = apply_token_funs(Token, Tokens),
            parse_and_replace(Rest, Tokens, <<Acc/binary, Value/binary>>);
        [_Rest] ->
            bad_tag
    end.

apply_token_funs(TokenBin, Tokens) ->
    CleanToken = binary:replace(TokenBin, [<<"(">>, <<")">>], <<"">>, [global]),
    case binary:split(CleanToken, <<" ">>, [global, trim]) of
        [Token] ->
            {_, Value} = lists:keyfind(Token, 1, Tokens),
            Value;
        FuncList ->
            [Token | Funs] = lists:reverse(FuncList),
            {_, Value} = lists:keyfind(Token, 1, Tokens),
            lists:foldl(
                fun(Current, Prev) ->
                    {_, Fun} = lists:keyfind(Current, 1, Tokens),
                    Fun(Token, Prev)
                end,
                Value,
                Funs)
    end.