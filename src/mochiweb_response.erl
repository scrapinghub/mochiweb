%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Response abstraction.
-compile({parse_transform,pmod_pt}).

-module(mochiweb_response, [Request, Code, Headers]).
-author('bob@mochimedia.com').

-define(QUIP, "Any of you quaids got a smint?").

-export([get_header_value/1, get/1, dump/0]).
-export([send/1, write_chunk/1]).

%% @spec get_header_value(string() | atom() | binary()) -> string() | undefined
%% @doc Get the value of the given response header.
get_header_value(K) ->
    mochiweb_headers:get_value(K, Headers).

%% @spec get(request | code | headers) -> term()
%% @doc Return the internal representation of the given field.
get(request) ->
    Request;
get(code) ->
    Code;
get(headers) ->
    Headers.

%% @spec dump() -> {mochiweb_request, [{atom(), term()}]}
%% @doc Dump the internal representation to a "human readable" set of terms
%%      for debugging/inspection purposes.
dump() ->
    [{request, mochiweb_request:dump(Request)},
     {code, Code},
     {headers, mochiweb_headers:to_list(Headers)}].

%% @spec send(iodata()) -> ok
%% @doc Send data over the socket if the method is not HEAD.
send(Data) ->
    case mochiweb_request:get(method, Request) of
        'HEAD' ->
            ok;
        _ ->
            mochiweb_request:send(Data, Request)
    end.

%% @spec write_chunk(iodata()) -> ok
%% @doc Write a chunk of a HTTP chunked response. If Data is zero length,
%%      then the chunked response will be finished.
write_chunk(Data) ->
    case mochiweb_request:get(version, Request) of
        Version when Version >= {1, 1} ->
            Length = iolist_size(Data),
            send([io_lib:format("~.16b\r\n", [Length]), Data, <<"\r\n">>]);
        _ ->
            send(Data)
    end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
