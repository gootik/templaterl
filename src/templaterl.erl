%%--------------------------------------------------------------------
%% @doc Templaterl is a very basic inline templating engine that
%%      supports token replacement as well as custom expressions
%%      on the token values.
%%
%%      For example you can do the following:
%%
%%      templaterl:compile(<<"This bitstring has a {{{tag}}} that can be uppercase {{{uppercase tag}}} too.">>, #{<<"tag">> => <<"token">>}).
%%          => This bitstring has a token that can be uppercase TOKEN too.
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
compile(_Bin, _Tokens) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
