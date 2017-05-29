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
-compile(inline_list_funcs).
-compile({inline_size, 500}).

-export([
    compile/2,
    register_helpers/1
]).

-type token_list() :: [token()].
-type token() :: {bitstring(), bitstring()}.

-define(TEMPLATERL_HELPER_MODULE, tempalterl_helpers).

-record(internal_state, {
    open_pattern,
    close_pattern,
    token_pattern,
    function_pattern
}).

%%====================================================================
%% API functions
%%====================================================================
-spec compile(bitstring(), token_list()) -> bitstring().
compile(Bin, Tokens) when is_bitstring(Bin) andalso is_list(Tokens) ->
    State = #internal_state{
        open_pattern = binary:compile_pattern(<<"{{{">>),
        close_pattern = binary:compile_pattern(<<"}}}">>),
        token_pattern = binary:compile_pattern(<<" ">>),
        function_pattern = binary:compile_pattern([<<"(">>, <<")">>])
    },

    parse_and_replace(Bin, Tokens, <<>>, State).

%% @doc If custom helper functions are needed they have to be registered
%%      before calling compile. This function creates a new inline
%%      module with the given expression definitions. This makes the
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
parse_and_replace(<<>>, _, Acc, _) ->
    erlang:iolist_to_binary(Acc);

parse_and_replace(Bin, TokenList, Acc, #internal_state{open_pattern = OpenPattern} = State) ->
    case binary:split(Bin, OpenPattern) of
        [Before, After] ->
            parse_and_replace2(After, TokenList, [Acc, Before], State);
        [Rest] ->
            erlang:iolist_to_binary([Acc, Rest])
    end.

parse_and_replace2(<<>>, _, _, _) ->
    bad_tag;

parse_and_replace2(Bin, TokenList, Acc, #internal_state{close_pattern = ClosePattern} = State) ->
    case binary:split(Bin, ClosePattern) of
        [Token, Rest] ->
            Value = convert_to_binary(apply_token_funs(Token, TokenList, State)),
            parse_and_replace(Rest, TokenList, [Acc, Value], State);
        [_Rest] ->
            bad_tag
    end.

apply_token_funs(TokenBin, TokenList, #internal_state{function_pattern = FunctionPattern, token_pattern = TokenPattern} = _State) ->
    case binary:split(TokenBin, TokenPattern, [global, trim]) of
        [Token] ->
            token_value(Token, TokenList);
        FuncList ->
            [Token | Funs] = lists:map(fun(E) -> binary:replace(E, FunctionPattern, <<"">>, [global]) end, lists:reverse(FuncList)),
            Value = token_value(Token, TokenList),
            lists:foldl(
                fun(Current, Prev) ->
                    ExpressionFun = binary_to_existing_atom(Current, utf8),
                    ?TEMPLATERL_HELPER_MODULE:ExpressionFun(Token, Prev)
                end,
                convert_to_binary(Value),
                Funs)
    end.

token_value(Token, TokenList) ->
    case lists:keyfind(Token, 1, TokenList) of
        {_, Value} ->
            Value;
        false ->
            throw({token_not_found, Token})
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
    ModuleDefinition = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(?TEMPLATERL_HELPER_MODULE)]),
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