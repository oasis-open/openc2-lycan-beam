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
%         , test_bad_action/1
%         , test_missing_action/1
%         , test_missing_target/1
%         , test_post/1
%         , send_recieve/5
         ]).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% JSON for input tests
%%-include_lib("./include/mitigate01.hrl").
%%-include_lib("./include/nonsense_action.hrl").
%%-include_lib("./include/bad_json.hrl").
%%-include_lib("./include/missing_action.hrl").
%%-include_lib("./include/mitigate_wo_target.hrl").

%% tests to run
all() ->
    [ test_bad_method
    , test_post_missing_body
    , test_unsupported_media_type
    , test_bad_json
%    , test_missing_action
%    , test_post
%    , test_bad_action
%    , test_missing_target
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

    Config.

end_per_suite(Config) ->
    Config.

%test_post(_Config) ->
%    MyPort = application:get_env(haha, port, 8080),
%
%    {ok, Conn} = gun:open("localhost", MyPort),
%    Headers = [ {<<"content-type">>, <<"application/json">>} ],
%
%     SomeJson = ?MITIGATE01,
%
%    %% validate Json
%    true = jsx:is_json(SomeJson),
%
%    Body = SomeJson,
%    Options = #{},
%
%    %% send json command to openc2
%    {ok, Response} = gun:post(Conn, "/openc2", Headers, Body, Options),
%
%    %% verify got 200 for status code
%    #{ status_code := 200 } = Response,
%    #{ headers := RespHeaders} = Response,
%
%    %% test header contents are correct
%    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
%                                                  , 1
%                                                  , RespHeaders
%                                                  ),
%    { <<"date">>, _Date } =  lists:keyfind(<<"date">>
%                                          , 1
%                                          , RespHeaders
%                                          ),
%    %%   note content length is likely to change
%%    { <<"content-length">>, <<"493">>} =  lists:keyfind(<<"content-length">>
%%                                                                 , 1
%%                                                                 , RespHeaders
%%                                                                 ),
%    { <<"content-type">>
%    , <<"application/json">>
%    } = lists:keyfind( <<"content-type">>
%                     , 1
%                     , RespHeaders
%                     ),
%
%    %% test body is what was expected in actual command tests
%
%    ok.

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
    lists:member({<<"allow">>,<<"POST">>},RespHeaders),
    lists:member({<<"content-length">>,<<"0">>},RespHeaders),
    lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

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
    lists:member({<<"allow">>,<<"POST">>},RespHeaders),
    lists:member({<<"content-length">>,<<"0">>},RespHeaders),
    lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply (which has error msg)
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_post_missing_body:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"Missing http body.">> = RespBody,

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
    lists:member({<<"allow">>,<<"POST">>},RespHeaders),
    lists:member({<<"content-length">>,<<"0">>},RespHeaders),
    lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    ok.

test_bad_json(_Config) ->
    %% test proper reponse to bad input (unparseable json)

    MyPort = application:get_env(haha, port, 8080),
    %%lager:info("test_post:port= ~p", [MyPort]),
    {ok, ConnPid} = gun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    BadJson = "{]}",

    %% validate Json is bad
    false = jsx:is_json(BadJson),

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
    lists:member({<<"allow">>,<<"POST">>},RespHeaders),
    lists:member({<<"content-length">>,<<"8">>},RespHeaders),
    lists:member({<<"server">>,<<"Cowboy">>},RespHeaders),

    %% get the body of the reply (which has error msg)
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),

    lager:info("test_bad_json:RespBody= ~p", [RespBody]),

    %% test body is what was expected
    <<"Bad JSON">> = RespBody,

    ok.

%test_bad_action(_Config) ->
%    %% test proper reponse to bad input (unrecognized action)
%
%    MyPort = application:get_env(haha, port, 8080),
%    {ok, Conn} = gun:open("localhost", MyPort),
%    Headers = [ {<<"content-type">>, <<"application/json">>} ],
%
%    %% give an invalid action
%    SomeJson = ?NONSENSE,
%
%    %% validate Json
%    true = jsx:is_json(SomeJson),
%
%    Body = SomeJson,
%
%    Options = #{},
%
%    %% send json command to openc2
%    {ok, Response} = gun:post(Conn, "/openc2", Headers, Body, Options),
%
%    %% verify got 400 (bad request) for status code
%    #{ status_code := 400 } = Response,
%
%    #{ headers := RespHeaders} = Response,
%    #{ body := RespBody } = Response,
%
%    %% test header contents are correct
%    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
%                                                  , 1
%                                                  , RespHeaders
%                                                  ),
%    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
%    %% note content length is for error mesg "Missing action function"
%    { <<"content-length">>, <<"10">>} =  lists:keyfind( <<"content-length">>
%                                                      , 1
%                                                      , RespHeaders
%                                                      ),
%    %% not sure why error response is in html?
%    { <<"content-type">>, <<"text/html">>} =  lists:keyfind( <<"content-type">>
%                                                           , 1
%                                                           , RespHeaders
%                                                           ),
%
%    %% test body is what was expected
%    RespBody = <<"bad action">>,
%
%    ok.
%
%test_missing_action(_Config) ->
%    %% test proper reponse to bad input (missing action)
%
%    MyPort = application:get_env(haha, port, 8080),
%    {ok, Conn} = gun:open("localhost", MyPort),
%    Headers = [ {<<"content-type">>, <<"application/json">>} ],
%
%    %% give an invalid action
%    SomeJson = ?MISSINGACTION,
%
%    %% validate Json
%    true = jsx:is_json(SomeJson),
%
%    Body = SomeJson,
%
%    Options = #{},
%
%    %% send json command to openc2
%    %%lager:info("about to send json to openc2"),
%    {ok, Response} = gun:post(Conn, "/openc2", Headers, Body, Options),
%    lager:info("sent json, got: ~p", [Response] ),
%
%    %% verify got 400 (bad request) for status code
%    #{ status_code := 400 } = Response,
%    %%lager:info("status = ~p", [RespStatus]),
%
%    #{ headers := RespHeaders} = Response,
%    %%lager:info("headers = ~p", [RespHeaders]),
%    #{ body := RespBody } = Response,
%    lager:info("body = ~p", [RespBody]),
%
%    %% test header contents are correct
%    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
%                                                  , 1
%                                                  , RespHeaders
%                                                  ),
%    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
%    %% note content length is for error mesg "Missing action function"
%    { <<"content-length">>, <<"14">>} =  lists:keyfind( <<"content-length">>
%                                                      , 1
%                                                      , RespHeaders
%                                                      ),
%    %% not sure why error response is in html?
%    { <<"content-type">>, <<"text/html">>} =  lists:keyfind( <<"content-type">>
%                                                           , 1
%                                                           , RespHeaders
%                                                           ),
%
%    %% test body is what was expected
%    RespBody = <<"Missing action">>,
%
%    ok.
%
%test_missing_target(_Config) ->
%    %% test proper reponse to bad input (missing target)
%
%    MyPort = application:get_env(haha, port, 8080),
%    {ok, Conn} = gun:open("localhost", MyPort),
%    Headers = [ {<<"content-type">>, <<"application/json">>} ],
%
%    %% give an invalid action
%    SomeJson = ?MITIGATEWOTARGET,
%
%    %% validate Json
%    true = jsx:is_json(SomeJson),
%
%    Body = SomeJson,
%
%    Options = #{},
%
%    %% send json command to openc2
%    %%lager:info("about to send json to openc2"),
%    {ok, Response} = gun:post(Conn, "/openc2", Headers, Body, Options),
%    lager:info("sent json, got: ~p", [Response] ),
%
%    %% verify got 400 (bad request) for status code
%    #{ status_code := 400 } = Response,
%    %%lager:info("status = ~p", [RespStatus]),
%
%    #{ headers := RespHeaders} = Response,
%    %%lager:info("headers = ~p", [RespHeaders]),
%    #{ body := RespBody } = Response,
%    lager:info("body = ~p", [RespBody]),
%
%    %% test header contents are correct
%    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
%                                                  , 1
%                                                  , RespHeaders
%                                                  ),
%    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
%    %% note content length is for error mesg "Missing action function"
%    { <<"content-length">>, <<"19">>} =  lists:keyfind( <<"content-length">>
%                                                      , 1
%                                                      , RespHeaders
%                                                      ),
%    %% not sure why error response is in html?
%    { <<"content-type">>, <<"text/html">>} =  lists:keyfind( <<"content-type">>
%                                                           , 1
%                                                           , RespHeaders
%                                                           ),
%
%    %% test body is what was expected
%    RespBody = <<"No Target Specified">>,
%
%    ok.

%%%%%%%%%%%%%%%%%%%% Utilities

%% utility to save putting this in each test
%send_recieve( Headers          % to send
%            , Options          % to send
%            , Url              % to send
%            , ExpectedStatus  % test get this received
%            , ExpectedBody
%            ) ->
%
%    MyPort = application:get_env(haha, port, 8080),
%    %%lager:info("test_post:port= ~p", [MyPort]),
%    {ok, Conn} = gun:open("localhost", MyPort),
%    {ok, Response} = gun:get(Conn, Url, Headers, Options),
%    %%lager:info("response = ~p", [Response]),
%
%    %% get status code of response
%    #{ status_code := RespStatus } = Response,
%    %%lager:info("status = ~p", [RespStatus]),
%    %% test what received was what was expected
%    ExpectedStatus = RespStatus,
%
%    %% get headers
%    #{ headers := RespHeaders } = Response,
%    %%lager:info("headers = ~p", [RespHeaders]),
%
%    %% verify headers
%    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
%                                                  , 1
%                                                  , RespHeaders
%                                                  ),
%    { <<"date">>, _Date } =  lists:keyfind( <<"date">>
%                                          , 1
%                                          , RespHeaders
%                                          ),
%
%
%    %% check if has body and if it is correct
%    #{ body := RespBody } = Response,
%    ExpectedBody = RespBody,
%
%    %% return
%    ok.
