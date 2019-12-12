-module(mochiweb_request_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

accepts_content_type_test() ->
    Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "multipart/related"}])),
    ?assertEqual(true, mochiweb_request:accepts_content_type("multipart/related", Req1)),
    ?assertEqual(true, mochiweb_request:accepts_content_type(<<"multipart/related">>, Req1)),

    Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual(false, mochiweb_request:accepts_content_type("multipart/related", Req2)),

    Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*"}])),
    ?assertEqual(true, mochiweb_request:accepts_content_type("multipart/related", Req3)),

    Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*; q=0.0"}])),
    ?assertEqual(false, mochiweb_request:accepts_content_type("multipart/related", Req4)),

    Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, multipart/*; q=0"}])),
    ?assertEqual(false, mochiweb_request:accepts_content_type("multipart/related", Req5)),

    Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, */*; q=0.0"}])),
    ?assertEqual(false, mochiweb_request:accepts_content_type("multipart/related", Req6)),

    Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "multipart/*; q=0.0, */*"}])),
    ?assertEqual(false, mochiweb_request:accepts_content_type("multipart/related", Req7)),

    Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "*/*; q=0.0, multipart/*"}])),
    ?assertEqual(true, mochiweb_request:accepts_content_type("multipart/related", Req8)),

    Req9 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "*/*; q=0.0, multipart/related"}])),
    ?assertEqual(true, mochiweb_request:accepts_content_type("multipart/related", Req9)),

    Req10 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1"}])),
    ?assertEqual(true, mochiweb_request:accepts_content_type("text/html;level=1", Req10)),

    Req11 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1, text/html"}])),
    ?assertEqual(true, mochiweb_request:accepts_content_type("text/html", Req11)),

    Req12 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1; q=0.0, text/html"}])),
    ?assertEqual(false, mochiweb_request:accepts_content_type("text/html;level=1", Req12)),

    Req13 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html; level=1; q=0.0, text/html"}])),
    ?assertEqual(false, mochiweb_request:accepts_content_type("text/html; level=1", Req13)),

    Req14 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html;level=1;q=0.1, text/html"}])),
    ?assertEqual(true, mochiweb_request:accepts_content_type("text/html; level=1", Req14)).

accepted_encodings_test() ->
    Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
                                mochiweb_headers:make([])),
    ?assertEqual(["identity"],
                 mochiweb_request:accepted_encodings(["gzip", "identity"], Req1)),

    Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip, deflate"}])),
    ?assertEqual(["gzip", "identity"],
                 mochiweb_request:accepted_encodings(["gzip", "identity"], Req2)),

    Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip;q=0.5, deflate"}])),
    ?assertEqual(["deflate", "gzip", "identity"],
                 mochiweb_request:accepted_encodings(["gzip", "deflate", "identity"], Req3)),

    Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "identity, *;q=0"}])),
    ?assertEqual(["identity"],
                 mochiweb_request:accepted_encodings(["gzip", "deflate", "identity"], Req4)),

    Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip; q=0.1, *;q=0"}])),
    ?assertEqual(["gzip"],
                 mochiweb_request:accepted_encodings(["gzip", "deflate", "identity"], Req5)),

    Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip; q=, *;q=0"}])),
    ?assertEqual(bad_accept_encoding_value,
                 mochiweb_request:accepted_encodings(["gzip", "deflate", "identity"], Req6)),

    Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "gzip;q=2.0, *;q=0"}])),
    ?assertEqual(bad_accept_encoding_value,
                 mochiweb_request:accepted_encodings(["gzip", "identity"], Req7)),

    Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept-Encoding", "deflate, *;q=0.0"}])),
    ?assertEqual([],
                 mochiweb_request:accepted_encodings(["gzip", "identity"], Req8)).

accepted_content_types_test() ->
    Req1 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual(["text/html"],
        mochiweb_request:accepted_content_types(["text/html", "application/json"], Req1)),

    Req2 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html, */*;q=0"}])),
    ?assertEqual(["text/html"],
        mochiweb_request:accepted_content_types(["text/html", "application/json"], Req2)),

    Req3 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*, */*;q=0"}])),
    ?assertEqual(["text/html"],
        mochiweb_request:accepted_content_types(["text/html", "application/json"], Req3)),

    Req4 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.8, */*;q=0.5"}])),
    ?assertEqual(["text/html", "application/json"],
        mochiweb_request:accepted_content_types(["application/json", "text/html"], Req4)),

    Req5 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.8, */*;q=0.5"}])),
    ?assertEqual(["text/html", "application/json"],
        mochiweb_request:accepted_content_types(["text/html", "application/json"], Req5)),

    Req6 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.5, */*;q=0.5"}])),
    ?assertEqual(["application/json", "text/html"],
        mochiweb_request:accepted_content_types(["application/json", "text/html"], Req6)),

    Req7 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make(
            [{"Accept", "text/html;q=0.5, application/json;q=0.5"}])),
    ?assertEqual(["application/json", "text/html"],
        mochiweb_request:accepted_content_types(["application/json", "text/html"], Req7)),

    Req8 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/html"}])),
    ?assertEqual([],
        mochiweb_request:accepted_content_types(["application/json"], Req8)),

    Req9 = mochiweb_request:new(nil, 'GET', "/foo", {1, 1},
        mochiweb_headers:make([{"Accept", "text/*;q=0.9, text/html;q=0.5, */*;q=0.7"}])),
    ?assertEqual(["application/json", "text/html"],
        mochiweb_request:accepted_content_types(["text/html", "application/json"], Req9)).

stream_body_when_expect_header_test() ->
    {Read, Write} = pipe(),
    Req = mochiweb_request:new(Write, 'PUT', "/foo", {1, 1},
        mochiweb_headers:make([{"Expect", "100-continue"}])),
    mochiweb_request:stream_body(1024, fun(_, _) -> ok end, [], Req),
    {ok, Response} = gen_tcp:recv(Read, 0),
    [gen_tcp:close(S) || S <- [Read, Write]],
    ?assertMatch(<<"HTTP/1.1 100 Continue\r\n", _/binary>>, list_to_binary(Response)).

pipe() ->
    {ok, ListenSocket} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(ListenSocket),
    Self = self(),
    spawn_link(fun() ->
        {ok, Socket} = gen_tcp:accept(ListenSocket),
        ok = gen_tcp:close(ListenSocket),
        ok = gen_tcp:controlling_process(Socket, Self),
        Self ! {socket, Socket}
    end),
    {ok, ClientSocket} = gen_tcp:connect("localhost", Port, []),
    receive
        {socket, ServerSocket} ->
            {ServerSocket, ClientSocket}
        after 5000 ->
            timeout
    end.

-endif.
