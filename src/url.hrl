%    -*- Erlang -*-
%    File:      url.hrl  (~jb/work/vpn-4-2/lib/misc/include/url.hrl)
%    Author:    Johan Bevemyr
%    Created:   Thu Dec  4 10:13:07 2003
%    Purpose:

-ifndef(URL_HRL).
-define(URL_HRL, true).

-record(hurl, {type,            %% net_path, abs_path or rel_path
               scheme    = [],
               host      = [],
               port,
               path      = [],
               user      = [],
               passwd    = [],
               params,
               qry,
               fragment}).

-record(http, {location,
               other = []}).

-record(hurl_opts, {timeout  = 1000*60*5,
                    sockopts = [],
                    dns_env}).

-endif.
