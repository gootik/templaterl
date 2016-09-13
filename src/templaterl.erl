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

-export([
    compile/2
]).

-type token_map() :: #{bitstring() => bitstring()}.

%%====================================================================
%% API functions
%%====================================================================
-spec compile(bitstring(), token_map()) -> bitstring().
compile(Bin, Tokens) when is_bitstring(Bin) andalso is_map(Tokens) ->
    parse_and_replace(Bin, Tokens, <<>>).

%%====================================================================
%% Internal functions
%%====================================================================
parse_and_replace(<<"{{{", Bin/binary>>, Tokens, Acc) ->
    parse_and_replace2(Bin, Tokens, <<>>, Acc);

parse_and_replace(<<Char:1/binary, Bin/binary>>, Tokens, Acc) ->
    parse_and_replace(Bin, Tokens, <<Acc/binary, Char/binary>>);

parse_and_replace(<<>>, _, Acc) ->
    Acc.

parse_and_replace2(<<"}}}", Bin/binary>>, Tokens, Token, Acc) ->
    Value = maps:get(Token, Tokens),
    parse_and_replace(Bin, Tokens, <<Acc/binary, Value/binary>>);

parse_and_replace2(<<Char:1/binary, Bin/binary>>, Tokens, Token, Acc) ->
    parse_and_replace2(Bin, Tokens, <<Token/binary, Char/binary>>, Acc);
parse_and_replace2(<<>>, _, _, _) ->
    bad_tag.
