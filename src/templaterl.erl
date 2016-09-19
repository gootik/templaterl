%%--------------------------------------------------------------------
%% @doc Templaterl is a very basic inline templating engine that
%%      supports token replacement as well as custom expressions
%%      on the token values.
%%
%%      For example you can do the following:
%%          Uppercase = "uppercase(_Token, Value) -> << <<(string:to_upper(X))>> || <<X>> <= Value >>.",
%%          templaterl:register_helpers([Uppercase]),
%%          templaterl:compile(<<"This bitstring has a {{{tag}}} that can be uppercase {{{uppercase tag}}} too.">>,
%%                             [{<<"tag">>, <<"token">>}]).
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

-type token_list() :: [token()].
-type token() :: {bitstring(), bitstring()}.

%%====================================================================
%% API functions
%%====================================================================
-spec compile(bitstring(), token_list()) -> bitstring().
compile(Bin, Tokens) when is_bitstring(Bin) andalso is_list(Tokens) ->
    parse_and_replace(Bin, Tokens, <<>>).

%% @doc If custom helper functions are needed they have to be registered
%%      before calling compile. This function creates a new inline
%%      module with the given expression defintions. This makes the
%%      calls faster.
%%
%%      Helper function definition is in the form of a string. For example,
%%      if you want a helper that concatenates the token name and value
%%      you would write something like:
%%
%%      DummyHelper = "dummy_helper(Token, Value) -> <<Token/binary, $:, Value/binary>>."
%%
%%      Usage:
%%      templaterl:register_helpers([DummyHelper]),
%%      templaterl:compile(<<"test {{dummy_helper my_token}}">>, [{<<"my_token">>, value}]).
%%        => <<"test my_token:value">>
%% @end
-spec register_helpers(list(string())) -> ok.
register_helpers(HelperList) ->
    generate_helper_module(HelperList).

%%====================================================================
%% Internal functions
%%====================================================================
parse_and_replace(<<>>, _, Acc) ->
    erlang:iolist_to_binary(Acc);

parse_and_replace(Bin, Tokens, Acc) ->
    case binary:split(Bin, <<"{{{">>) of
        [Before, After] ->
            parse_and_replace2(After, Tokens, [Acc, Before]);
        [Rest] ->
            parse_and_replace(<<>>, Tokens, [Acc, Rest])
    end.

parse_and_replace2(<<>>, _, _) ->
    bad_tag;

parse_and_replace2(Bin, Tokens, Acc) ->
    case binary:split(Bin, <<"}}}">>) of
        [Token, Rest] ->
            Value = convert_to_binary(apply_token_funs(Token, Tokens)),
            parse_and_replace(Rest, Tokens, [Acc, Value]);
        [_Rest] ->
            bad_tag
    end.

apply_token_funs(TokenBin, Tokens) ->
    case binary:split(TokenBin, <<" ">>, [global, trim]) of
        [Token] ->
            {_, Value} = lists:keyfind(Token, 1, Tokens),
            Value;
        FuncList ->
            [Token | Funs] = lists:map(fun(E) -> binary:replace(E, [<<"(">>, <<")">>], <<"">>, [global]) end, lists:reverse(FuncList)),
            {_, Value} = lists:keyfind(Token, 1, Tokens),
            lists:foldl(
                fun(Current, Prev) ->
                    apply(templaterl_helpers, binary_to_existing_atom(Current, utf8), [Token, Prev])
                end,
                convert_to_binary(Value),
                Funs)
    end.

convert_to_binary(Term) when is_binary(Term) -> Term;
convert_to_binary(Term) when is_integer(Term) -> integer_to_binary(Term);
convert_to_binary(Term) when is_float(Term) -> list_to_binary(io_lib:format("~p", [Term]));
convert_to_binary(Term) when is_list(Term) -> list_to_binary(Term);
convert_to_binary(true) -> <<"true">>;
convert_to_binary(false) -> <<"false">>;
convert_to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8).

generate_helper_module(HelperList) ->
    Forms = helper_forms(HelperList),
    {ok, Module, Bin} = compile:forms(Forms, [debug_info, inline]),
    code:purge(Module),
    Filename = atom_to_list(Module) ++ ".erl",
    {module, Module} = code:load_binary(Module, Filename, Bin),
    ok.

helper_forms(HelperList) ->
    ModuleDefinition = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(templaterl_helpers)]),
    Functions = [helper_function_syntax(HelperBody) || HelperBody <- HelperList],

    ExportsList = [erl_syntax:arity_qualifier(erl_syntax:atom(HelperName), erl_syntax:integer(2)) || {HelperName, _} <- Functions],
    Exports = erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list(ExportsList)]),

    FunctionForms = [HelperFuncForm || {_, HelperFuncForm} <- Functions],

    Module = [ModuleDefinition, Exports] ++ FunctionForms,
    [erl_syntax:revert(X) || X <- Module].

helper_function_syntax(HelperBody) ->
    {ok, Ts, _} = erl_scan:string(HelperBody),
    {ok, Form} = erl_parse:parse_form(Ts),
    Fun = element(3, Form),
    {Fun, Form}.