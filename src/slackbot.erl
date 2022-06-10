%%  -*- erlang-mode -*-
%%  File:    slackbot.erl)
%%  Author:  Johan Bevemyr
%%  Created: Wed Jan  6 14:06:59 2021
%%  Purpose: Post messages to slack channels

-module(slackbot).
-author('jb@bevemyr.com').

%% -compile(export_all).

-behaviour(gen_server).

%% External exports
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%
-define(SERVER, ?MODULE).
-define(i2l(X), integer_to_list(X)).
-define(l2i(X), list_to_integer(X)).
-define(l2b(X), list_to_binary(X)).
-define(l2f(X), list_to_float(X)).
-define(b2l(X), binary_to_list(X)).
-define(a2l(X), atom_to_list(X)).
-define(l2a(X), list_to_atom(X)).
-define(t2l(X), tuple_to_list(X)).
-define(l2t(X), list_to_tuple(X)).
-define(io2b(X), iolist_to_binary(X)).

-define(stack(), try throw(1) catch _:_ -> erlang:get_stacktrace() end).
-define(liof(Fmt, Args), io:format(user, "~w:~w " ++ Fmt,[?MODULE,?LINE|Args])).
-define(liof_bt(Fmt, Args), io:format(user, "~w:~w ~s ~p\n",
                             [?MODULE, ?LINE,
                              io_lib:format(Fmt, Args), ?stack()])).

-define(SLACK_URL, "https://slack.com/api/").
-define(SLACK_TOKEN, "xoxb-XXX").

-define(DAY_SECS, 86400).

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server

-record(state,
        {
         channels = #{},
         schedule = []
        }).

-record(event,
        {
         time, %% {HH,MM,SS}
         day,  %% every | weekday | [1,2,3,4,5,6,7] 1: Monday, 2: Tuesday, ...
         channel, %% binary()
         message  %% binary()
        }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    ?liof("========================================\n", []),
    ?liof("Starting slackbot\n", []),
    rand:seed(exrop),
    _SleepSecs = secs_until_tomorrow_morning(),
    timer:send_after(1000, process),
    Schedule = schedule(),
    Channels = get_channels(),
    {ok, #state{channels = Channels, schedule = Schedule}}.


schedule() ->
    [#event{time = {8,0,0},
            day = weekday,
            channel = <<"tech">>,
            message = <<"Dagens möte.  "
                        "<https://us02web.zoom.us/j/12345?pwd="
                        "XXX|zoom.us/...>"/utf8>>},
     #event{time = {9,58,0},
            day = weekday,
            channel = <<"general">>,
            message = <<"Fika.  "
                        "<https://us02web.zoom.us/j/12345?pwd="
                        "XXX|"
                        "zoom.us/...>">>},
     #event{time = {14,58,0},
            day = weekday,
            channel = <<"general">>,
            message = <<"Fika.  "
                        "<https://us02web.zoom.us/j/12345?pwd="
                        "XXX|zoom.us/...>">>},
%%      #event{time = {random, {8,30,0}, {14,58,0}},
%%             day = {random_weekday, 0.4},
%%             channel = <<"tech">>,
%%             message = <<"Vad det blir för demo idag?/Calle"/utf8>>},
%%     #event{time = {9,44,0},
%%            day = [1,3,5],
%%            channel = <<"general">>,
%%            message = <<"Standup in the main channel">>},
     #event{time = {9,44,0},
            day = [1,3,5],
            channel = <<"tech">>,
            message = <<"Standup in the main channel">>}].

%%----------------------------------------------------------------------
handle_call(stop, _From, S) ->
    {stop, normal, S};

handle_call(_Request, _From, S) ->
    Res = {struct, [{status, "error"}, {reason, "unknown request"}]},
    {reply, Res, S}.

%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
handle_info(process, State) ->
    ?liof("process schedule\n", []),
    SleepSecs = secs_until_tomorrow_morning(),
    Schedule = schedule(),
    Channels = get_channels(),
    process(Schedule),
    timer:send_after(SleepSecs*1000, process),
    {noreply, State#state{channels=Channels, schedule=Schedule}};
handle_info({send_message, Channel, Text}, State) ->
    send_message(Channel, Text, State#state.channels),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%

process(Schedule) ->
    DW = calendar:day_of_the_week(date()),
    WorkDay = is_workday(date()),
    process(Schedule, time(), DW, WorkDay).

process([], _Time, _DW, _WorkDay) ->
    ok;
process([Event|Rest], Time, DW, WorkDay) ->
    #event{time = When0,
           day = Day,
           channel = Channel,
           message = Message} = Event,
    When = get_when(When0),
    IsToday = is_today(Day, DW, WorkDay),
    if IsToday, When > Time ->
            ?liof("Scheduling slack message at ~p: #~p\n", [When, Channel]),
            Secs = secs_to_when(Time, When),
            timer:send_after(Secs*1000, {send_message, Channel, Message}),
            process(Rest, Time, DW, WorkDay);
       true ->
            %% already passed or not today, ignore for today
            ?liof("Passing on ~p: #~s\n", [When, Channel]),
            process(Rest, Time, DW, WorkDay)
    end.

get_when({random, {SH, SM, SS}, {EH, EM, ES}}) ->
    H = random(SH, EH),
    M = random(SM, EM),
    S = random(SS, ES),
    {H, M, S};
get_when({H,M,S}) ->
    {H,M,S}.

random(Start, End) ->
    Range = End-Start+1,
    Start + rand:uniform(Range) - 1.

is_today(every, _DW, _WorkDay) ->
    true;
is_today(weekday, DW, WorkDay) ->
    WorkDay andalso DW < 6;
is_today({random_weekday, Rate}, _DW, WorkDay) ->
    case rand:uniform_real() of
        X when X =< Rate ->
            WorkDay;
        _ ->
            false
    end;
is_today(List, DW, WorkDay) ->
    WorkDay andalso lists:member(DW, List).

is_workday({Year, Month, Day}) ->
    try
        Url = lists:flatten(
                ["https://sholiday.faboul.se/dagar/v2.1/",
                 ?i2l(Year),"/",?i2l(Month),"/",?i2l(Day)]),
        {200, _Headers, Content} = url:get(Url),
        Res = jsx:decode(iolist_to_binary(Content)),
        Dagar = hd(maps:get(<<"dagar">>, Res)),
        case maps:get(<<"arbetsfri dag">>, Dagar) of
            <<"Nej">> ->
                true;
            <<"Ja">> ->
                false
        end
    catch
        X:Y:Stack ->
            ?liof("Crash when fetching workday: ~p:~p\n~p\n",
                  [X,Y,Stack]),
            true
    end.

secs_to_when(Time, When) ->
    Tsecs = time_to_secs(Time),
    Wsecs = time_to_secs(When),
    Wsecs - Tsecs.

time_to_secs({H,M,S}) ->
    H*60*60+M*60+S.

get_channels() ->
    case catch url:get_headers(
                 ?SLACK_URL++"/conversations.list",
                 [{"Authorization","Bearer "++?SLACK_TOKEN}])
    of
        {200, _Headers, Body} ->
            B = jsx:decode(?io2b(Body)),
            Cs = maps:get(<<"channels">>, B, #{}),
            F = fun(C, Acc) ->
                        Id = maps:get(<<"id">>, C),
                        Name = maps:get(<<"name">>, C),
                        Acc#{Name => Id}
                end,
            Channels = lists:foldl(F, #{}, Cs),
            %% ?liof("Channels=~p\n", [Channels]),
            Channels;
        Error ->
            ?liof("Got error ~p\n", [Error]),
            timer:sleep(10000),
            get_channels()
    end.

send_message(Channel, Text, Channels) ->
    ChannelId = maps:get(Channel, Channels, <<"unknown">>),
    Url = ?SLACK_URL++"/chat.postMessage",
    case url:post_dict(
           Url, [{"channel", ?b2l(ChannelId)},
                 {"text", ?b2l(Text)}], [],
           [{"Authorization","Bearer "++?SLACK_TOKEN}])
    of
        {_Status, _Headers, _Body} ->
            %% ?liof("Status=~p\n", [_Status]),
            %% ?liof("Header=~p\n", [_Headers]),
            %% ?liof("Body=~p\n", [_Body]),
            ok;
        Error ->
            ?liof("Error=~p\n", [Error]),
            ok
    end.

%% Time management

inc_date(Date, Inc) ->
    DateSecs = calendar:datetime_to_gregorian_seconds({Date, {0,0,0}}),
    FutureSecs = DateSecs + ?DAY_SECS*Inc,
    {FutureDate,_} = calendar:gregorian_seconds_to_datetime(FutureSecs),
    FutureDate.

secs_until_tomorrow() ->
    Now = gnow(),
    NextDate = inc_date(date(), 1),
    Tomorrow = calendar:datetime_to_gregorian_seconds({NextDate, {0,0,0}}),
    Tomorrow - Now.

secs_until_tomorrow_morning() ->
    secs_until_tomorrow() + 60.

gnow() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%%
