%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Case preserving (but case insensitive) HTTP Header dictionary.

-module(mochiweb_headers).
-author('bob@mochimedia.com').
-export([from_list/1, insert/3, enter/3, get_value/2, lookup/2]).
-export([delete_any/2, get_primary_value/2]).
-export([default/3, enter_from_list/2, default_from_list/2]).
-export([to_list/1, make/1, to_normalized_list/1]).
-export([from_binary/1]).

%% @type headers().
%% @type key() = atom() | binary() | string().
%% @type value() = atom() | binary() | string() | integer().

%% @spec make(headers() | [{key(), value()}]) -> headers()
%% @doc Construct a headers() from the given list.
make(L) when is_list(L) ->
    from_list(L);
%% assume a non-list is already mochiweb_headers.
make(T) ->
    T.

%% @spec from_binary(iolist()) -> headers()
%% @doc Transforms a raw HTTP header into a mochiweb headers structure.
%%
%%      The given raw HTTP header can be one of the following:
%%
%%      1) A string or a binary representing a full HTTP header ending with
%%         double CRLF.
%%         Examples:
%%         ```
%%         "Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n"
%%         <<"Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n">>'''
%%
%%      2) A list of binaries or strings where each element represents a raw
%%         HTTP header line ending with a single CRLF.
%%         Examples:
%%         ```
%%         [<<"Content-Length: 47\r\n">>, <<"Content-Type: text/plain\r\n">>]
%%         ["Content-Length: 47\r\n", "Content-Type: text/plain\r\n"]
%%         ["Content-Length: 47\r\n", <<"Content-Type: text/plain\r\n">>]'''
%%
from_binary(RawHttpHeader) when is_binary(RawHttpHeader) ->
    from_binary(RawHttpHeader, []);
from_binary(RawHttpHeaderList) ->
    from_binary(list_to_binary([RawHttpHeaderList, "\r\n"])).

from_binary(RawHttpHeader, Acc) ->
    case erlang:decode_packet(httph, RawHttpHeader, []) of
        {ok, {http_header, _, H, _, V}, Rest} ->
            from_binary(Rest, [{H, V} | Acc]);
        _ ->
            make(Acc)
    end.

%% @spec from_list([{key(), value()}]) -> headers()
%% @doc Construct a headers() from the given list.
from_list(List) ->
    lists:foldl(
        fun({Key, Value}, Headers) -> insert(Key, Value, Headers) end,
        {0, dict:new()},
        List
    ).

%% @spec enter_from_list([{key(), value()}], headers()) -> headers()
%% @doc Insert pairs into the headers, replace any values for existing keys.
enter_from_list(List, T) ->
    lists:foldl(fun ({K, V}, T1) -> enter(K, V, T1) end, T, List).

%% @spec default_from_list([{key(), value()}], headers()) -> headers()
%% @doc Insert pairs into the headers for keys that do not already exist.
default_from_list(List, T) ->
    lists:foldl(fun ({K, V}, T1) -> default(K, V, T1) end, T, List).

%% @spec to_list(headers()) -> [{key(), string()}]
%% @doc Return the contents of the headers. The keys will be the exact key
%%      that was first inserted (e.g. may be an atom or binary, case is
%%      preserved).
to_list(Headers) ->
  Result = fold(
    fun(Key, Value, Acc) -> [{Key, Value} | Acc] end,
    [],
    Headers
  ),
lists:reverse(Result).

%% @spec to_list(headers()) -> [{key(), string()}]
%% @doc Return the contents of the headers. The keys will be the exact key
%%      that was first inserted (e.g. may be an atom or binary, case is
%%      preserved).
to_normalized_list(Headers) ->
    Result = fold(
        fun(Key, Value, Acc) -> [{normalize(Key), Value} | Acc] end,
        [],
        Headers
    ),
    lists:reverse(Result).

%% @spec get_value(key(), headers()) -> string() | undefined
%% @doc Return the value of the given header using a case insensitive search.
%%      undefined will be returned for keys that are not present.
get_value(K, T) ->
    case lookup(K, T) of
        {value, {_, V}} ->
            expand(V);
        none ->
            undefined
    end.

%% @spec get_primary_value(key(), headers()) -> string() | undefined
%% @doc Return the value of the given header up to the first semicolon using
%%      a case insensitive search. undefined will be returned for keys
%%      that are not present.
get_primary_value(K, T) ->
    case get_value(K, T) of
        undefined ->
            undefined;
        V ->
            lists:takewhile(fun (C) -> C =/= $; end, V)
    end.

%% @spec lookup(key(), headers()) -> {value, {key(), string()}} | none
%% @doc Return the case preserved key and value for the given header using
%%      a case insensitive search. none will be returned for keys that are
%%      not present.
lookup(K, {_, Headers}) ->
    case dict:find(normalize(K), Headers) of
        {ok, KVs} ->
            [{_, K0, _} | _] = KVs,
            {value, {K0, join_values(KVs)}};
        error ->
            none
    end.

%% @spec default(key(), value(), headers()) -> headers()
%% @doc Insert the pair into the headers if it does not already exist.
default(K, V, {N, Headers}) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    case dict:is_key(K1, Headers) of
        true  -> {N, Headers};
        false -> {N + 1, dict:store(K1, [{N, K, V1}], Headers)}
    end.

%% @spec enter(key(), value(), headers()) -> headers()
%% @doc Insert the pair into the headers, replacing any pre-existing key.
enter(K, V, {N, Headers}) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    {N + 1, dict:store(K1, [{N, K, V1}], Headers)}.

%% @spec insert(key(), value(), headers()) -> headers()
%% @doc Insert the pair into the headers, merging with any pre-existing key.
%%      A merge is done with Value = V0 ++ ", " ++ V1.
insert(K, V, {N, Headers}) ->
    K1 = normalize(K),
    V1 = any_to_list(V),
    Headers2 = dict:update(
        K1, 
        fun (Old) -> merge(K1, {N, K, V1}, Old) end,
        [{N, K, V1}],
        Headers
    ),
    {N + 1, Headers2}.

%% @spec delete_any(key(), headers()) -> headers()
%% @doc Delete the header corresponding to key if it is present.
delete_any(K, {N, Headers}=H) ->
    K1 = normalize(K),
    case dict:find(K1, Headers) of
        {ok, Values} ->
            Headers2 = dict:erase(K1, Headers),
            {N - length(Values), Headers2};
        error ->
            H
    end.

%% Internal API

expand({array, L}) ->
    mochiweb_util:join(lists:reverse(L), ", ");
expand(V) ->
    V.

merge("set-cookie", V1, V0) ->
    V0 ++ [V1];
merge(_, V1, V0) ->
    [{N, K0, _}| _] = V0,
    [{N, K0, join_values(V0 ++ [V1])}].

join_values(L) ->
    mochiweb_util:join([V || {_, _, V} <- L], ", ").

normalize(K) when is_list(K) ->
    string:to_lower(K);
normalize(K) when is_atom(K) ->
    normalize(atom_to_list(K));
normalize(K) when is_binary(K) ->
    normalize(binary_to_list(K)).

any_to_list(V) when is_list(V) ->
    V;
any_to_list(V) when is_atom(V) ->
    atom_to_list(V);
any_to_list(V) when is_binary(V) ->
    binary_to_list(V);
any_to_list(V) when is_integer(V) ->
    integer_to_list(V).

fold(Fun, Acc, {_, Headers}) ->
    Lines = dict:fold(
        fun(_, Value, Acc1) -> Value ++ Acc1 end,
        [],
        Headers
    ),
    do_fold(lists:sort(Lines), Fun, Acc).

do_fold([{_, Key, Value} | Rest], Fun, Acc) ->
    do_fold(Rest, Fun, Fun(Key, Value, Acc));
do_fold([], _Fun, Acc) -> Acc.


%%
%% Tests.
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_test() ->
    Identity = make([{hdr, foo}]),
    ?assertEqual(
       Identity,
       make(Identity)).

enter_from_list_test() ->
    H = make([{hdr, foo}]),
    ?assertEqual(
       [{hdr, "foo"}, {baz, "wibble"}],
       to_list(enter_from_list([{baz, wibble}], H))),
    ?assertEqual(
       [{hdr, "bar"}],
       to_list(enter_from_list([{hdr, bar}], H))),
    ok.

default_from_list_test() ->
    H = make([{hdr, foo}]),
    ?assertEqual(
       [{hdr, "foo"}, {baz, "wibble"}],
       to_list(default_from_list([{baz, wibble}], H))),
    ?assertEqual(
       [{hdr, "foo"}],
       to_list(default_from_list([{hdr, bar}], H))),
    ok.

get_primary_value_test() ->
    H = make([{hdr, foo}, {baz, <<"wibble;taco">>}]),
    ?assertEqual(
       "foo",
       get_primary_value(hdr, H)),
    ?assertEqual(
       undefined,
       get_primary_value(bar, H)),
    ?assertEqual(
       "wibble",
       get_primary_value(<<"baz">>, H)),
    ok.

set_cookie_test() ->
    H = make([{"set-cookie", foo}, {"set-cookie", bar}, {"set-cookie", baz}]),
    ?assertEqual(
       [{"set-cookie", "foo"}, {"set-cookie", "bar"}, {"set-cookie", "baz"}],
       to_list(H)),
    ok.

headers_test() ->
    H = ?MODULE:make([{hdr, foo}, {"Hdr", "bar"}, {'Hdr', 2}]),
    [{hdr, "foo, bar, 2"}] = ?MODULE:to_list(H),
    H1 = ?MODULE:insert(taco, grande, H),
    [{hdr, "foo, bar, 2"}, {taco, "grande"}] = ?MODULE:to_list(H1),
    H2 = ?MODULE:make([{"Set-Cookie", "foo"}]),
    [{"Set-Cookie", "foo"}] = ?MODULE:to_list(H2),
    H3 = ?MODULE:insert("Set-Cookie", "bar", H2),
    [{"Set-Cookie", "foo"}, {"Set-Cookie", "bar"}] = ?MODULE:to_list(H3),
    "foo, bar" = ?MODULE:get_value("set-cookie", H3),
    {value, {"Set-Cookie", "foo, bar"}} = ?MODULE:lookup("set-cookie", H3),
    undefined = ?MODULE:get_value("shibby", H3),
    none = ?MODULE:lookup("shibby", H3),
    H4 = ?MODULE:insert("content-type",
                        "application/x-www-form-urlencoded; charset=utf8",
                        H3),
    "application/x-www-form-urlencoded" = ?MODULE:get_primary_value(
                                             "content-type", H4),
    H4 = ?MODULE:delete_any("nonexistent-header", H4),
    H3 = ?MODULE:delete_any("content-type", H4),
    H5 = ?MODULE:make([{"ccc","444"}, {"aaa","123"}, {"bbb","321"}]),
    H5_Case = ?MODULE:make([{"CCC","444"}, {"Aaa","123"}, {"Bbb","321"}]),
    H5_List = [{"ccc","444"}, {"aaa","123"}, {"bbb","321"}],
    H5_List = ?MODULE:to_list(H5),
    H5_List = ?MODULE:to_normalized_list(H5_Case),
    HB = <<"Content-Length: 47\r\nContent-Type: text/plain\r\n\r\n">>,
    H_HB = ?MODULE:from_binary(HB),
    H_HB = ?MODULE:from_binary(binary_to_list(HB)),
    "47" = ?MODULE:get_value("Content-Length", H_HB),
    "text/plain" = ?MODULE:get_value("Content-Type", H_HB),
    L_H_HB = ?MODULE:to_list(H_HB),
    2 = length(L_H_HB),
    true = lists:member({'Content-Length', "47"}, L_H_HB),
    true = lists:member({'Content-Type', "text/plain"}, L_H_HB),
    HL = [ <<"Content-Length: 47\r\n">>, <<"Content-Type: text/plain\r\n">> ],
    HL2 = [ "Content-Length: 47\r\n", <<"Content-Type: text/plain\r\n">> ],
    HL3 = [ <<"Content-Length: 47\r\n">>, "Content-Type: text/plain\r\n" ],
    H_HL = ?MODULE:from_binary(HL),
    H_HL = ?MODULE:from_binary(HL2),
    H_HL = ?MODULE:from_binary(HL3),
    "47" = ?MODULE:get_value("Content-Length", H_HL),
    "text/plain" = ?MODULE:get_value("Content-Type", H_HL),
    L_H_HL = ?MODULE:to_list(H_HL),
    2 = length(L_H_HL),
    true = lists:member({'Content-Length', "47"}, L_H_HL),
    true = lists:member({'Content-Type', "text/plain"}, L_H_HL),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<>>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"\r\n">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary(<<"\r\n\r\n">>)),
    [] = ?MODULE:to_list(?MODULE:from_binary("")),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<>>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"">>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"\r\n">>])),
    [] = ?MODULE:to_list(?MODULE:from_binary([<<"\r\n\r\n">>])),
    ok.

-endif.
