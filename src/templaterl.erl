%%--------------------------------------------------------------------
%% @doc Templaterl is a very basic inline templating engine that
%%      supports token replacement as well as custom expressions
%%      on the token values.
%%
%%      For example you can do the following:
%%          Uppercase = fun(_Token, Value) -> << <<(string:to_upper(X))>> || <<X>> <= Value >> end.
%%          templaterl:compile(<<"This bitstring has a {{{tag}}} that can be uppercase {{{uppercase tag}}} too.">>,
%%                             [{<<"tag">>, <<"token">>},
%%                              {<<"uppercase">>, Uppercase}]).
%%              => <<"This bitstring has a token that can be uppercase TOKEN too.">>
%%
%%
%% @end
%%--------------------------------------------------------------------
-module(templaterl).

-compile(inline).

-export([
    compile/2,
    register_helpers/1
]).

-type token_list() :: [token() | token_expression()].
-type token() :: {bitstring(), bitstring()}.
-type token_expression() :: {bitstring(), token_helper_function()}.
-type token_helper_function() :: fun((bitstring(), bitstring()) -> bitstring()).

%%====================================================================
%% API functions
%%====================================================================
-spec compile(bitstring(), token_list()) -> bitstring().
compile(Bin, Tokens) when is_bitstring(Bin) andalso is_list(Tokens) ->
    parse_and_replace(Bin, Tokens, <<>>).

-spec register_helpers(list(token_expression())) -> ok.
register_helpers(HelperList) ->
    try
        Forms = helper_forms(HelperList),
        io:format(user, "~p~n", [Forms]),
        {ok, Module, Bin} = compile:forms(Forms, [debug_info]),
        code:purge(Module),
        Filename = atom_to_list(Module) ++ ".erl",
        {module, Module} = code:load_binary(Module, Filename, Bin),
        ok
    catch Error:Reason ->
        io:format(user, "~p:~p:~p ~n", [Error, Reason, erlang:get_stacktrace()])
    end.

helper_forms(HelperList) ->
    ModuleDefinition = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(templaterl_helpers)]),
    ExportsList = [erl_syntax:arity_qualifier(erl_syntax:atom(HelperName), erl_syntax:integer(2)) || HelperName <- proplists:get_keys(HelperList)],
    Export = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list(ExportsList)]),

    Functions = [erl_syntax:function(erl_syntax:atom(HelperName), helper_function_clauses(HelperName, HelperBody)) || {HelperName, HelperBody} <- HelperList],

    Module = [ModuleDefinition, Export] ++ Functions,
    [erl_syntax:revert(X) || X <- Module].

helper_function_clauses(_HelperName, HelperBody) ->
    io:format(user, "~p~n", [HelperBody]),
    io:format(user, "~p~n", [erlang:fun_info(HelperBody)]),
    {env, [{_,_,_,Forms}]} = erlang:fun_info(HelperBody, env),

    Forms.

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
            Value = convert_to_binary(apply_token_funs(Token, Tokens)),
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
                    apply(templaterl_helpers, binary_to_existing_atom(Current, utf8), [Token, Prev])
                end,
                Value,
                Funs)
    end.

convert_to_binary(Term) when is_binary(Term) -> Term;
convert_to_binary(Term) when is_integer(Term) -> integer_to_binary(Term);
convert_to_binary(Term) when is_float(Term) -> float_to_binary(Term);
convert_to_binary(Term) when is_list(Term) -> list_to_binary(Term);
convert_to_binary(true) -> <<"true">>;
convert_to_binary(false) -> <<"false">>;
convert_to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8).