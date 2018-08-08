%%% @author Duncan Sparrell
%%% @copyright (C) 2018, sFractal Consulting LLC
%%%
-module(scan_SUITE).
-author("Duncan Sparrell").
-license("MIT").
-copyright("2018, Duncan Sparrell sFractal Consulting LLC").

%%%-------------------------------------------------------------------
%%% Copyright (c) 2018, Duncan Sparrell, sFractal Consulting
%%% MIT License

%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation files
%%% (the "Software"), to deal in the Software without restriction,
%%% including without limitation the rights to
%%% use, copy, modify, merge, publish, distribute, sublicense, and/or
%%% sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:

%%% The above copyright notice and this permission notice
%%% shall be included in all copies or substantial portions of the Software.

%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
%%% WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE
%%% AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%-------------------------------------------------------------------

%% for test export all functions
-export( [ all/0
         , suite/0
         , init_per_suite/1
         , end_per_suite/1
         , test_scan_memory/1
         ]).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_scan_memory
    ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap, {minutes, 2}}].

%% setup config parameters
init_per_suite(Config) ->
    {ok, _AppList} = application:ensure_all_started(lager),
    %%lager:info("AppList: ~p~n", [AppList]),

    {ok, _AppList2} = application:ensure_all_started(gun),
    %%lager:info("AppList2: ~p~n", [AppList2]),

    %% since ct doesn't read sys.config, set configs here
    application:set_env(haga, port, 8080),
    application:set_env(haga, listener_count, 5),

    %% start application
    {ok, _AppList3} = application:ensure_all_started(haga),
    %%lager:info("AppList3: ~p~n", [AppList3]),

    Config.

end_per_suite(Config) ->
    Config.

test_scan_memory(_Config) ->
    MyPort = application:get_env(haga, port, 8080),

    {ok, ConnPid} = gun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    Body = <<"{\"id\":\"0b4153de-03e1-4008-a071-0b2b23e20723\",\"action\":\"query\",\"target\":\"Hello World\"}">>,

    %% send json command to openc2
    StreamRef = gun:post(ConnPid, "/openc2", Headers, Body),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),
    lager:info("test_query_whatareyou:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    nofin = element(2, Response),
    Status = element(3,Response),
    ExpectedStatus = 200,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    true = lists:member({<<"content-length">>,<<"13">>},RespHeaders),
    true= lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply (which has error msg)
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_query_whatareyou:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"\"Hello World\"">> = RespBody,

    ok.

test_query_profile(_Config) ->
    MyPort = application:get_env(haga, port, 8080),

    {ok, ConnPid} = gun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    Body = <<"{\"id\":\"0b4153de\",\"action\":\"query\",\"target\":{\"openc2\":{\"profile\":\"\"}}}">>,

    %% send json command to openc2
    StreamRef = gun:post(ConnPid, "/openc2", Headers, Body),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),
    lager:info("test_query_profile:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    nofin = element(2, Response),
    Status = element(3,Response),
    ExpectedStatus = 200,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    true = lists:member({<<"content-length">>,<<"63">>},RespHeaders),
    true= lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_query_profile:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"{\"x_haga\":\"https://github.com/sparrell/openc2-cap/haga.cap.md\"}">> =
      RespBody,

    ok.
