%%% @author Duncan Sparrell
%%% @copyright (C) 2018, sFractal Consulting LLC
%%%
-module(a_simple_post_SUITE).
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
         , test_bad_method/1
         , test_post_missing_body/1
         , test_unsupported_media_type/1
         , test_bad_json/1
         , test_bad_action/1
         , test_missing_action/1
         , test_missing_target/1
         , test_post/1
         ]).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_bad_method
    , test_post_missing_body
    , test_unsupported_media_type
    , test_bad_json
    , test_missing_action
    , test_post
    , test_bad_action
    , test_missing_target
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
    application:set_env(haha, port, 8080),
    application:set_env(haha, listener_count, 5),

    %% start application
    {ok, _AppList3} = application:ensure_all_started(haha),
    %%lager:info("AppList3: ~p~n", [AppList3]),

    lager_common_test_backend:bounce(debug),

    Config.

end_per_suite(Config) ->
    Config.

test_post(Config) ->
  %% test json file with query openc2 profile
  JsonSendFileName = "query.helloworld.json",
  %% expect results files
  JsonResponseFileName = "query.helloworld.reply.json",

  %% expect status=OK ie 200
  StatusCode = 200,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),

    ok.

test_bad_method(_Config) ->
    %% test if send get when expecting post

    MyPort = application:get_env(haha, port, 8080),
    lager:info("test_bad_method"),
    {ok, Conn} = gun:open("localhost", MyPort),

    %% send get to openc2 which only allows posts
    StreamRef = gun:get(Conn, "/openc2"),

    %% check reply
    Response = gun:await(Conn,StreamRef),
    lager:info("test_bad_method:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    fin = element(2, Response),

    Status = element(3,Response),
    ExpectedStatus = 405,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    true = lists:member({<<"allow">>,<<"POST">>},RespHeaders),
    true = lists:member({<<"content-length">>,<<"0">>},RespHeaders),
    true = lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    ok.


test_post_missing_body(_Config) ->
    %% test proper reponse to bad input (no body to html request)

    MyPort = application:get_env(haha, port, 8080),
    lager:info("test_post_missing_body"),
    {ok, ConnPid} = gun:open("localhost", MyPort),

    %% send json post with no body
    Headers = [ {<<"content-type">>, <<"application/json">>} ],
    Body = "",
    StreamRef = gun:post(ConnPid, "/openc2", Headers, Body),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),
    lager:info("test_post_missing_body:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    nofin = element(2, Response),

    Status = element(3,Response),
    ExpectedStatus = 400,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    true = lists:member({<<"content-length">>,<<"19">>},RespHeaders),
    true = lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply (which has error msg)
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_post_missing_body:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"\"Missing http body\"">> = RespBody,

    ok.

test_unsupported_media_type(_Config) ->
    %% test proper reponse to bad input
    %%     (html request has media type other than json)

    MyPort = application:get_env(haha, port, 8080),
    lager:info("test_unsupported_media_type"),
    {ok, ConnPid} = gun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"text/plain">>} ],

    Body = "scan",

    %% send json command to openc2
    %%lager:info("about to send json to openc2"),
    StreamRef = gun:post(ConnPid, "/openc2", Headers, Body),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),
    lager:info("test_unsupported_media_type:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    fin = element(2, Response),
    Status = element(3,Response),
    ExpectedStatus = 415,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    true = lists:member({<<"content-length">>,<<"0">>},RespHeaders),
    true = lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    ok.

test_bad_json(_Config) ->
    %% test proper reponse to bad input (unparseable json)

    MyPort = application:get_env(haha, port, 8080),
    %%lager:info("test_bad_json:port= ~p", [MyPort]),
    {ok, ConnPid} = gun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    BadJson = "{]}",

    %% send json command to openc2
    StreamRef = gun:post(ConnPid, "/openc2", Headers, BadJson),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),
    lager:info("test_bad_json:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    nofin = element(2, Response),
    Status = element(3,Response),
    ExpectedStatus = 400,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    %% forcefail = Response,  %% use for debuggingh
    true = lists:member({<<"content-length">>,<<"20">>},RespHeaders),
    true = lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply (which has error msg)
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_bad_json:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"\"Can not parse JSON\"">> = RespBody,

    ok.

test_bad_action(_Config) ->
    %% test proper reponse to bad input (unrecognized action)

    MyPort = application:get_env(haha, port, 8080),
    {ok, ConnPid} = gun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    %% test JSON has bad action
    Body = <<"{\"id\":\"0b4153de-03e1-4008-a071-0b2b23e20723\",\"action\":\"whiskytangofoxtrox\",\"target\":\"Hello World\"}">>,


    %% send json command to openc2
    StreamRef = gun:post(ConnPid, "/openc2", Headers, Body),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),
    lager:info("test_bad_action:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    nofin = element(2, Response),
    Status = element(3,Response),
    ExpectedStatus = 400,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    true = lists:member({<<"content-length">>,<<"16">>},RespHeaders),
    true= lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply (which has error msg)
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_bad_json:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"\"unknown action\"">> = RespBody,

    ok.

test_missing_action(_Config) ->
    %% test proper reponse to bad input (missing action)

    MyPort = application:get_env(haha, port, 8080),
    {ok, ConnPid} = gun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    %% test JSON is missing action
    Body = <<"{\"id\":\"0b4153de-03e1-4008-a071-0b2b23e20723\",\"target\":\"Hello World\"}">>,

    %% send json command to openc2
    StreamRef = gun:post(ConnPid, "/openc2", Headers, Body),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),
    lager:info("test_missing_action:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    nofin = element(2, Response),
    Status = element(3,Response),
    ExpectedStatus = 400,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    true = lists:member({<<"content-length">>,<<"24">>},RespHeaders),
    true= lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply (which has error msg)
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_bad_json:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"\"missing command action\"">> = RespBody,

    ok.


test_missing_target(_Config) ->
    %% test proper reponse to bad input (missing target)

    MyPort = application:get_env(haha, port, 8080),
    {ok, ConnPid} = gun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    %% JSON is missing target
    Body = <<"{\"id\":\"0b4153de-03e1-4008-a071-0b2b23e20723\",\"action\":\"query\"}">>,

    %% send json command to openc2
    StreamRef = gun:post(ConnPid, "/openc2", Headers, Body),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),
    lager:info("test_missing_target:Response= ~p", [Response]),

    %% Check contents of reply
    response = element(1,Response),
    nofin = element(2, Response),
    Status = element(3,Response),
    ExpectedStatus = 400,
    ExpectedStatus = Status,

    RespHeaders = element(4,Response),
    %%true = Response,  %% for debugging
    true = lists:member({<<"content-length">>,<<"24">>},RespHeaders),
    true= lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply (which has error msg)
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_missing_target:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"\"missing command target\"">> = RespBody,

    ok.
