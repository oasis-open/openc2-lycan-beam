%%% @author Duncan Sparrell
%%% @copyright (C) 2018, sFractal Consulting LLC
%%%
-module(deny_SUITE).
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
         , test_deny_ipcon/1
         , test_deny_2/1
         ]).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_deny_ipcon
    , test_deny_2
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
    application:set_env(slpfhw, port, 8080),
    application:set_env(slpfhw, listener_count, 5),

    %% start application
    {ok, _AppList3} = application:ensure_all_started(slpfhw),
    %%lager:info("AppList3: ~p~n", [AppList3]),

    lager_common_test_backend:bounce(debug),

    Config.

end_per_suite(Config) ->
    Config.

test_deny_ipcon(Config) ->
  %% test json file with scan memory
  JsonSendFileName = "deny.ipcon.json",
  %% expect results files
  JsonResponseFileName = "deny.ipcon.reply.json",

  %% expect status=OK ie 200
  StatusCode = 200,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),

    ok.

test_deny_2(Config) ->
    %% test json file with scan memory
    JsonSendFileName = "deny.2.json",
    %% expect results files
    JsonResponseFileName = "deny.2.reply.json",

    %% expect status=OK ie 200
    StatusCode = 200,
    %% send command and check results
    ok = helper:post_oc2_body( JsonSendFileName
                             , StatusCode
                             , JsonResponseFileName
                             , Config
                             ),

      ok.
