%    -*- Erlang -*-
%    File:      url.erl  (~jb/work/vpn-4-2/lib/misc/src/url.erl)
%    Author:    Johan Bevemyr
%    Created:   Thu Dec  4 10:12:19 2003
%    Purpose:   Basic HTTP client

-module('url').
-author('jb@skrak.bluetail.com').

%% -compile(export_all).

-export([parse_url/1, parse_url/2, format_url/1, format_url_path/1,
         get/2, get/3, get/4, format_url_hostport/1, to_net_path/2,
         get_with_cookies/2,
         get/1, get_headers/2, post/2, post/3, post/4, post_dict/2,
         post_dict/4, update_cookies/2]).

-include("url.hrl").
-include_lib("kernel/include/inet.hrl").

-ifdef(debug).
-define(dbg(X,Y), error_logger:info_msg("*dbg ~p:~p: " X,
                                        [?MODULE, ?LINE | Y])).
-else.
-define(dbg(X,Y), ok).
-endif.

-define(io2b, iolist_to_binary).
-define(i2l,  integer_to_list).
-define(l2i,  list_to_integer).
-define(b2l,  binary_to_list).
-define(l2b,  list_to_binary).
-define(t2b,  term_to_binary).
-define(b2t,  binary_to_term).
-define(a2l,  atom_to_list).
-define(l2a,  list_to_atom).
-define(l2ea, list_to_existing_atom).
-define(b2a,  binary_to_atom).
-define(b2ea, binary_to_existing_atom).
-define(a2b,  atom_to_binary).
-define(l2t,  list_to_tuple).
-define(t2l,  tuple_to_list).
-define(i2b,  integer_to_binary).
-define(b2i,  binary_to_integer).

-define(i2l(I), integer_to_list(I)).

-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).
-define(liof(Fmt, Args), io:format(user, "~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
-define(liof_bt(Fmt, Args), io:format(user, "~w:~w ~s ~p\n",
                             [?MODULE, ?LINE,
                              io_lib:format(Fmt, Args), ?stack()])).

-define(s2l(X), if is_binary(X) -> ?b2l(X);
                   is_atom(X)   -> ?a2l(X);
                   is_list(X)   -> X
                end).

-define(s2b(X), if is_binary(X) -> X;
                   is_atom(X)   -> ?a2b(X, utf8);
                   is_list(X)   -> ?l2b(X)
                end).

parse_url(Str) ->
    parse_url_scheme(Str, #hurl{}, []).

parse_url(Str, Strict) ->
    case Str of
        "http://" ++ _ ->
            parse_url(Str);
        "https://" ++ _ ->
            parse_url(Str);
        "ftp://" ++ _ ->
            parse_url(Str);
        "file://" ++ _ ->
            parse_url(Str);
        _ when Strict == sloppy ->
            parse_url_host(Str, #hurl{scheme = "http"}, []);
        _  ->
            parse_url(Str)
    end.

parse_url_scheme("://" ++ T, U, Acc) ->
    parse_url_host_or_user(T, U#hurl{scheme = lists:reverse(Acc),
                                     type = net_path}, []);
parse_url_scheme([H | T], U, Acc) ->
    IsValid = is_alnum(H) orelse lists:member(H, "+-."),
    if IsValid ->
            parse_url_scheme(T, U, [H | Acc]);
       true ->
            parse_url_path_no_scheme(lists:reverse(Acc) ++ [H | T], U)
    end;
parse_url_scheme([], U, Acc) ->
    parse_url_path_no_scheme(lists:reverse(Acc), U).


parse_url_host_or_user(Str, U, Acc) ->
    case find_char($@, $/, Str) of
        true ->
            parse_url_user(Str, U, Acc);
        false ->
            parse_url_host(Str, U, Acc)
    end.

parse_url_user([$: | T], U, Acc) ->
    parse_url_passwd(T, U#hurl{user = lists:reverse(Acc)}, []);
parse_url_user([$@ | T], U, Acc) ->
    parse_url_host(T, U#hurl{user = lists:reverse(Acc)}, []);
parse_url_user([H | T], U, Acc) ->
    parse_url_user(T, U, [H | Acc]);
parse_url_user([], U, Acc) ->
    {error, {user, U, lists:reverse(Acc)}}.

parse_url_passwd([$@ | T], U, Acc) ->
    parse_url_host(T, U#hurl{passwd = lists:reverse(Acc)}, []);
parse_url_passwd([H | T], U, Acc) ->
    parse_url_passwd(T, U, [H | Acc]);
parse_url_passwd([], U, Acc) ->
    {error, {passwd, U, lists:reverse(Acc)}}.

parse_url_host([$/ | T], U, Acc) ->
    parse_url_path([$/|T], U#hurl{host = lists:reverse(Acc)}, []);
parse_url_host([$: | T], U, Acc) ->
    parse_url_port(T, U#hurl{host = lists:reverse(Acc)}, []);
parse_url_host([H | T], U, Acc) ->
    parse_url_host(T, U, [H | Acc]);
parse_url_host([], U, Acc) ->
    {ok, U#hurl{host = lists:reverse(Acc)}}.

parse_url_port([$/ | T], U, Acc) ->
    parse_url_path([$/|T],
              U#hurl{port=list_to_integer(lists:reverse(Acc))}, []);
parse_url_port([H | T], U, Acc) ->
    parse_url_port(T, U, [H | Acc]);
parse_url_port([], U, Acc) ->
    {ok, U#hurl{port = list_to_integer(lists:reverse(Acc))}}.


parse_url_path_no_scheme(T=[$/|_], U) ->
    parse_url_path(T, U#hurl{type=abs_path}, []);
parse_url_path_no_scheme(T, U) ->
    parse_url_path(T, U#hurl{type=rel_path}, []).


parse_url_path([$; | T], U, Acc) ->
    parse_url_params(T, U#hurl{path = lists:reverse(Acc)}, []);
parse_url_path([$? | T], U, Acc) ->
    parse_url_qry(T, U#hurl{path = lists:reverse(Acc)}, []);
parse_url_path([$# | T], U, Acc) ->
    parse_url_fragment(T, U#hurl{path = lists:reverse(Acc)}, []);
parse_url_path([H | T], U, Acc) ->
    parse_url_path(T, U, [H | Acc]);
parse_url_path([], U, Acc) ->
    {ok, U#hurl{path = lists:reverse(Acc)}}.

parse_url_params([$? | T], U, Acc) ->
    parse_url_qry(T, U#hurl{params = lists:reverse(Acc)}, []);
parse_url_params([$# | T], U, Acc) ->
    parse_url_fragment(T, U#hurl{params = lists:reverse(Acc)}, []);
parse_url_params([H | T], U, Acc) ->
    parse_url_params(T, U, [H | Acc]);
parse_url_params([], U, Acc) ->
    {ok, U#hurl{params = lists:reverse(Acc)}}.

parse_url_qry([$# | T], U, Acc) ->
    parse_url_fragment(T, U#hurl{qry = lists:reverse(Acc)}, []);
parse_url_qry([H | T], U, Acc) ->
    parse_url_qry(T, U, [H | Acc]);
parse_url_qry([], U, Acc) ->
    {ok, U#hurl{qry = lists:reverse(Acc)}}.

parse_url_fragment([], U, []) ->
    {ok, U};
parse_url_fragment(Str, U, []) ->
    {ok, U#hurl{fragment = Str}}.


format_url(U) ->
    [if U#hurl.scheme /= [] -> [U#hurl.scheme, "://"];
        true -> ""
     end,
     if U#hurl.user /= [] -> [U#hurl.user, ":", U#hurl.passwd, "@"];
        true -> ""
     end,
     U#hurl.host,
     if U#hurl.port /= undefined -> [":", integer_to_list(U#hurl.port)];
        true -> ""
     end|
     format_url_path(U)].

format_url_path(U) ->
    [if U#hurl.path /= [] -> U#hurl.path;
        true -> ""
     end,
     if U#hurl.params /= undefined -> [";", U#hurl.params];
        true -> ""
     end,
     if U#hurl.qry /= undefined -> ["?", U#hurl.qry];
        true -> ""
     end,
     if U#hurl.fragment /= undefined -> ["#", U#hurl.fragment];
        true -> ""
     end].

format_url_hostport(U) ->
    [U#hurl.host|
     if U#hurl.port /= undefined -> [":", integer_to_list(U#hurl.port)];
        true -> ""
     end].


%% true iff C is found in Str before Before is found
find_char(C, _Before, [C | _]) -> true;
find_char(_C, Before, [Before | _]) -> false;
find_char(C, Before, [_ | T]) -> find_char(C, Before, T);
find_char(_, _, []) -> false.


is_alnum(C) when C >= $a, C =< $z -> true;
is_alnum(C) when C >= $A, C =< $Z -> true;
is_alnum(C) when C >= $0, C =< $9 -> true;
is_alnum(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_net_path(Src,Dst) ->
    case Dst#hurl.type of
        net_path ->
            Dst;
        abs_path ->
            Dst#hurl{host=Src#hurl.host,
                     port=Src#hurl.port,
                     scheme=Src#hurl.scheme,
                     type=net_path};
        rel_path ->
            SP = if Src#hurl.path == [] -> "/";
                    true -> Src#hurl.path
                 end,
            SPath = lists:reverse(SP),
            Stripped = lists:reverse(strip_until($/,SPath)),
            Dst#hurl{host=Src#hurl.host,
                     port=Src#hurl.port,
                     scheme=Src#hurl.scheme,
                     type=net_path,
                     path=yaws_api:path_norm(Stripped++Dst#hurl.path)}
    end.

%

strip_until(C, R=[C|_]) -> R;
strip_until(C, [_|R])   -> strip_until(C,R);
strip_until(_, [])      -> [].

%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post_dict(UrlStr, Dict) ->
    Coded = [yaws_api:url_encode(K)++"="++yaws_api:url_encode(V) ||
                {K,V} <- Dict],
    Payload = string:join(Coded, "&"),
    post(UrlStr, Payload, "application/x-www-form-urlencoded").

post_dict(UrlStr, Dict, Cookies, Headers0) ->
    {ok, Url} = parse_url(UrlStr),
    Coded = [yaws_api:url_encode(K)++"="++yaws_api:url_encode(V) ||
                {K,V} <- Dict],
    Payload = string:join(Coded, "&"),
    ContentType = "application/x-www-form-urlencoded",
    CookiesStr = string:join([K++"="++V || {K,V} <- Cookies], "; "),
    Headers = [{"Content-Type", ContentType},
               {"Cookie", CookiesStr}|Headers0],
    post(Url#hurl.host, Url, Headers, #hurl_opts{}, Payload).

post(UrlStr, Payload) ->
    post(UrlStr, Payload, "text/xml; charset=utf-8").

post(UrlStr, Payload, ContentType) ->
    {ok, Url} = parse_url(UrlStr),
    Headers = [{"Content-Type", ContentType}],
    post(Url#hurl.host, Url, Headers, #hurl_opts{}, Payload).

post(UrlStr, Payload, ContentType, Cookies) ->
    {ok, Url} = parse_url(UrlStr),
    CookiesStr = string:join([K++"="++V || {K,V} <- Cookies], "; "),
    Headers = [{"Content-Type", ContentType},
               {"Cookie", CookiesStr}],
    post(Url#hurl.host, Url, Headers, #hurl_opts{}, Payload).

post(Host, Url0=#hurl{}, Headers0, SOpts, Payload) ->
    Length = length(lists:flatten(Payload)),
    Url = if Url0#hurl.path == "" -> Url0#hurl{path="/"};
             true -> Url0
          end,
    Path = format_url_path(Url),
    Opts =
        if Url#hurl.host == [] ->
                [];
           Url#hurl.port == undefined ->
                ["Host: ", Url#hurl.host,"\r\n"];
           Url#hurl.port == 80 , Url#hurl.scheme == "http" ->
                ["Host: ", Url#hurl.host,"\r\n"];
           Url#hurl.port == 443 , Url#hurl.scheme == "https" ->
                ["Host: ", Url#hurl.host,"\r\n"];
           true ->
                ["Host: ", Url#hurl.host,":",
                 integer_to_list(Url#hurl.port),"\r\n"]
        end,

    Headers=[{"Content-Length", ?i2l(Length)}|Headers0],
    FormatedHeaders = [[Key,": ",Value,"\r\n"] || {Key,Value} <- Headers],

    Cmd = ["POST ", Path, " HTTP/1.0\r\n", Opts, FormatedHeaders,
           "\r\n", Payload],

    Port =
        if Url#hurl.port /= undefined -> Url#hurl.port;
           Url#hurl.scheme == "http" -> 80;
           Url#hurl.scheme == "https" -> 443;
           true -> 80
        end,

    SSL =
        if Url#hurl.scheme == "https" -> true;
           true -> false
        end,

    case catch connect(SSL, Host, Port, SOpts) of
        {'EXIT', Reason} ->
            {error, Reason};
        {error, timeout} ->
            ?dbg("Connection timeout",[]),
            {error, "connection timeout"};
        {error, Reason} when SSL==true ->
            ?dbg("Connect error = ~p\n", [Reason]),
            {error, ssl:format_error(Reason)};
        {error, Reason} ->
            ?dbg("Connect error = ~p\n", [Reason]),
            {error, inet:format_error(Reason)};
        {ok, Socket} ->
            send(Socket, Cmd),
            receive_data(Socket, SOpts#hurl_opts.timeout, [])
    end.

get(Host, Url0, SOpts) ->
    get(Host, Url0, [], SOpts).

get(Host, Url0=#hurl{}, Headers, SOpts=#hurl_opts{}) ->
    Url = if Url0#hurl.path == "" -> Url0#hurl{path="/"};
             true -> Url0
          end,
    Path = format_url_path(Url),
    Opts =
        if Url#hurl.host == [] ->
                [];
           Url#hurl.port == undefined ->
                ["Host: ", Url#hurl.host,"\r\n"];
           Url#hurl.port == 80 , Url#hurl.scheme == "http" ->
                ["Host: ", Url#hurl.host,"\r\n"];
           Url#hurl.port == 443 , Url#hurl.scheme == "https" ->
                ["Host: ", Url#hurl.host,"\r\n"];
           true ->
                ["Host: ", Url#hurl.host,":",
                 integer_to_list(Url#hurl.port),"\r\n"]
        end,

    FormattedHeaders = [[Key,": ",Value,"\r\n"] || {Key,Value} <- Headers],

    Cmd = ["GET ", Path, " HTTP/1.0\r\n", Opts, FormattedHeaders, "\r\n"],

    Port =
        if Url#hurl.port /= undefined -> Url#hurl.port;
           Url#hurl.scheme == "http" -> 80;
           Url#hurl.scheme == "https" -> 443;
           true -> 80
        end,

    SSL =
        if Url#hurl.scheme == "https" -> true;
           true -> false
        end,

    case catch connect(SSL, Host, Port, SOpts) of
        {'EXIT', Reason} ->
            {error, Reason};
        {error, timeout} ->
            ?dbg("Connection timeout",[]),
            {error, "connection timeout"};
        {error, Reason} when SSL==true ->
            ?dbg("Connect error = ~p\n", [Reason]),
            {error, ssl:format_error(Reason)};
        {error, Reason} ->
            ?dbg("Connect error = ~p\n", [Reason]),
            {error, inet:format_error(Reason)};
        {ok, Socket} ->
            send(Socket, Cmd),
            receive_data(Socket, SOpts#hurl_opts.timeout, [])
    end.

get(UrlStr) ->
    get(UrlStr, #hurl_opts{}).

get_with_cookies(UrlStr, Cookies) ->
    {ok, Url} = parse_url(UrlStr),
    CookiesStr = string:join([K++"="++V || {K,V} <- Cookies], "; "),
    Headers = [{"Cookie", CookiesStr}],
    get(Url#hurl.host, Url, Headers, #hurl_opts{}).

get(Url=#hurl{}, Opts) ->
    get(Url#hurl.host, Url, Opts);
get(UrlStr, Opts) ->
    {ok, Url} = parse_url(UrlStr),
    get(Url, Opts).

get_headers(UrlStr, Headers) ->
    UrlStrFlat = lists:flatten(UrlStr),
    {ok, Url} = parse_url(UrlStrFlat),
    get(Url#hurl.host, Url, Headers, #hurl_opts{}).

%% get_headers(UrlStr, Headers, _Qry) ->
%%     UrlStrFlat = lists:flatten(UrlStr),
%%     {ok, Url} = parse_url(UrlStrFlat),
%%     get(Url#hurl.host, Url, Headers, #hurl_opts{}).


receive_data(Socket, Timeout, Acc) ->
    receive
        {tcp, Socket, B} ->
            receive_data(Socket, Timeout, [binary_to_list(B)|Acc]);
        {ssl, Socket, B} ->
            receive_data(Socket, Timeout, [binary_to_list(B)|Acc]);
        {ssl_closed, Socket} ->
            close(Socket),
            Data = lists:reverse(Acc),
            case split_head_body(Data, []) of
                {error, _Reason} = E ->
                    E;
                {Head, Body} ->
                    {Status, Rest} = get_status(Head),
                    Headers = parse_headers(Rest),
                    unzip({Status,Headers,Body})
            end;
        {tcp_closed, Socket} ->
            close(Socket),
            Data = lists:reverse(Acc),
            case split_head_body(Data, []) of
                {error, _Reason} = E ->
                    E;
                {Head, Body} ->
                    {Status, Rest} = get_status(Head),
                    Headers = parse_headers(Rest),
                    unzip({Status,Headers,Body})
            end
        after
            Timeout ->
                close(Socket),
                {error, timeout}
    end.

unzip({Status, Headers={_,_,H}, Body}) ->
    case {lists:keyfind("content-encoding", 1, H),
          lists:keyfind(<<"content-encoding">>, 1, H)} of
        {{_, "gzip"},_} ->
            {Status, Headers, zlib:gunzip(Body)};
        {_, {_, <<"gzip">>}} ->
            {Status, Headers, zlib:gunzip(Body)};
        _ ->
            {Status, Headers, Body}
    end.

split_head_body(Msg, Acc) ->
    case get_next_line(Msg) of
        {error, Reason} ->
            {error, Reason};
        {[], Rest} ->
            {lists:reverse(Acc), Rest};
        {Line, Rest} ->
            split_head_body(Rest, [Line|Acc])
    end.

get_next_line(Data) ->
    %% io:format("Data = ~p\n", [Data]),
    get_next_line(Data,[]).

get_next_line([D|Ds], Acc) ->
    case split_reply(D,[]) of
        more ->
            get_next_line(Ds, [D|Acc]);
        {Pre, Rest} when Acc==[] ->
            {Pre, [Rest|Ds]};
        {Pre, Rest} ->
            {lists:flatten(lists:reverse([Pre|Acc])), [Rest|Ds]}
    end;
get_next_line([], _Acc) ->
    {error, no_line}.

split_reply("\r\n"++Rest, Pre) ->
    {lists:reverse(Pre), Rest};
split_reply([H|T], Pre) ->
    split_reply(T, [H|Pre]);
split_reply("", _Pre) ->
    more.

get_status([Line|Rest]) ->
    Code = get_status_code(Line),
    {Code, Rest};
get_status([]) ->
    {error, []}.

get_status_code([$H,$T,$T,$P,$/,$1,$.,_,_,D1,D2,D3|_]) ->
    list_to_integer([D1,D2,D3]);
get_status_code(_) ->
    error.

parse_headers(Lines) ->
    parse_headers(Lines, #http{}).

parse_headers([], Headers) ->
    Headers;
parse_headers([Line|Lines], Headers) ->
    case string:chr(Line, $:) of
        0 ->
            Headers;
        N ->
            Key = lowercase(string:strip(string:sub_string(Line, 1, N-1))),
            Value = string:sub_string(Line, N+2),
            NewH = add_header(Key, Value, Headers),
            parse_headers(Lines, NewH)
    end.

add_header("location", Value, H) ->
    H#http{location = Value};
add_header(Other, Value, H) ->
    H#http{other = [{Other,Value}|H#http.other]}.

lowercase(Str) ->
    [lowercase_ch(S) || S <- Str].

lowercase_ch(C) when C>=$A, C=<$Z -> C + 32;
lowercase_ch(C) -> C.

%% Need to use server specific network configuration...
connect(SSL, Host, Port, SOpts) ->
    %% ?dbg("Host = ~p\n", [Host]),
    %% ?liof("connect ~p\n", [Host]),
    case gethostbyname(Host, SOpts) of
        {ok, IP} ->
            %% ?liof("got ip ~p\n",[IP]),
            Res = do_connect(SSL, IP, Port, SOpts#hurl_opts.timeout,
                             [binary, {packet, raw},
                              {nodelay, true}, {active, true} |
                              SOpts#hurl_opts.sockopts]),
            %% ?liof("Connected ~p\n", [Res]),
            case Res of
                {ok, _} ->
                    ok;
                _ ->
                    erlang:erase({dnscache, Host})
            end,
            Res;
        Error ->
            %% ?liof("got error ~p\n",[Error]),
            Error
    end.

gethostbyname(Host={_,_,_,_}, _SOpts) ->
    {ok, Host};
gethostbyname(Host, _SOpts) ->
    case inet_parse:ipv4_address(Host) of
        {ok, IP} ->
            {ok, IP};
        _ ->
            case erlang:get({dnscache, Host}) of
                undefined ->
                    case inet:gethostbyname(Host) of
                        {ok, HE} ->
                            Addr = hd(HE#hostent.h_addr_list),
                            erlang:put({dnscache, Host}, Addr),
                            {ok, Addr};
                        Error ->
                            Error
                    end;
                Addr ->
                    {ok, Addr}
            end
    end.

do_connect(true, IP, Port, Timeout, Opts) ->
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    ssl:connect(IP, Port, Opts, Timeout);
do_connect(false, IP, Port, Timeout, Opts) ->
    ?dbg("do_connect(~p,~p,~p,~p)\n", [IP, Port, Opts, Timeout]),
    gen_tcp:connect(IP, Port, Opts, Timeout).

send(Port = {sslsocket, _, _}, Cmd) ->
    ssl:send(Port, Cmd);
send(Port, Cmd) ->
    gen_tcp:send(Port, Cmd).

close(Port = {sslsocket, _, _}) ->
    ssl:close(Port);
close(Port) ->
    gen_tcp:close(Port).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_cookies(Cookies, {http, _, Headers}) ->
    %% io:format("Headers=~p\n", [Headers]),
    F = fun({"set-cookie", Str}, Acc) ->
                case parse_set_cookie(Str) of
                    [] ->
                        Acc;
                    Cookie ->
                        add_to_dict(Cookie, Acc)
                end;
           ({<<"set-cookie">>, Str}, Acc) ->
                case parse_set_cookie(?s2l(Str)) of
                    [] ->
                        Acc;
                    Cookie ->
                        add_to_dict(Cookie, Acc)
                end;
           (_, Acc) ->
                Acc
        end,
    lists:foldl(F, Cookies, Headers).

parse_set_cookie(Str) ->
    try
        [KeyValue|_] = string:tokens(Str, ";"),
        [Key,Value] = string:tokens(KeyValue, "="),
        {string:strip(Key, both), string:strip(Value, both)}
    catch
        X:Y ->
            ?liof("parse_set_cookie crashed: ~p:~p\n", [X,Y]),
            []
    end.

add_to_dict(Cookie, []) ->
    [Cookie];
add_to_dict({Key,Value}, [{Key,_Value}|Rest]) ->
    [{Key,Value}|Rest];
add_to_dict(Cookie, [C|Rest]) ->
    [C|add_to_dict(Cookie,Rest)].
