%%% @author Duncan Sparrell
%%% @copyright (C) 2018, sFractal Consulting LLC
%%%
-module(semantics_SUITE).
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
         , test_missing_id/1
         , test_xtra/1
         , test_bad_target_string/1
         , test_bad_target_json/1
         , test_empty_target_map/1
         , test_too_many_targets/1
         , test_unknown_target/1
         , test_unknown_target_specifier/1
         ]).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_missing_id
    , test_xtra
    , test_bad_target_string
    , test_bad_target_json
    , test_empty_target_map
    , test_too_many_targets
    , test_unknown_target
    , test_unknown_target_specifier
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

test_missing_id(Config) ->
  %% test json file
  JsonSendFileName = "missing.id.json",
  %% expect results files
  JsonResponseFileName = "missing.id.reply.json",

  %% expected status
  StatusCode = 400,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),
  ok.

test_xtra(Config) ->
  %% test json file
  JsonSendFileName = "xtra.json",
  %% expect results files
  JsonResponseFileName = "xtra.reply.json",

  %% expected status
  StatusCode = 400,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),
  ok.

test_bad_target_string(Config) ->
  %% test json file
  JsonSendFileName = "bad.targetstring.json",
  %% expect results files
  JsonResponseFileName = "bad.targetstring.reply.json",

  %% expected status
  StatusCode = 400,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),
  ok.

test_bad_target_json(Config) ->
  %% test json file
  JsonSendFileName = "bad.targetjson.json",
  %% expect results files
  JsonResponseFileName = "bad.targetjson.reply.json",

  %% expected status
  StatusCode = 400,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),
  ok.

test_empty_target_map(Config) ->
  %% test json file
  JsonSendFileName = "empty.target.map.json",
  %% expect results files
  JsonResponseFileName = "empty.target.map.reply.json",

  %% expected status
  StatusCode = 400,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),
  ok.

test_too_many_targets(Config) ->
  %% test json file
  JsonSendFileName = "too.many.targets.json",
  %% expect results files
  JsonResponseFileName = "too.many.targets.reply.json",

  %% expected status
  StatusCode = 400,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),
  ok.

test_unknown_target(Config) ->
  %% test json file
  JsonSendFileName = "unknown.target.json",
  %% expect results files
  JsonResponseFileName = "unknown.target.reply.json",

  %% expected status
  StatusCode = 400,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),
  ok.

test_unknown_target_specifier(Config) ->
  %% test json file
  JsonSendFileName = "unknown.target.specifier.json",
  %% expect results files
  JsonResponseFileName = "unknown.target.specifier.reply.json",

  %% expected status
  StatusCode = 400,
  %% send command and check results
  ok = helper:post_oc2_body( JsonSendFileName
                           , StatusCode
                           , JsonResponseFileName
                           , Config
                           ),
  ok.
