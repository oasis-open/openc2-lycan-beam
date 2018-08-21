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

%%%-------------------------------------------------------------------
%% @doc helper routines for test
%% @end
%%%-------------------------------------------------------------------

-module(helper).
-author("Duncan Sparrell").
-copyright("2018, sFractal Consulting, LLC").
-license("MIT").

%% for test export all functions
-export( [ post_oc2_no_body/3
         , post_oc2_body/4
         ] ).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%% Utilities

%% utilities to save putting this in each test

%% post openc2 command expecting no body
post_oc2_no_body(JsonFileName, ExpectedStatus, Config) ->
    %% read json to send
    JsonTxt = read_json_file(JsonFileName, Config),

    %% open connection to send post
    ConnPid = make_conn(),

    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    %% send json command to openc2
    StreamRef = gun:post(ConnPid, "/openc2", Headers, JsonTxt),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),

    %% Check contents of reply
    response = element(1,Response),
    %% since this is no_body, expect fin
    fin = element(2, Response),
    Status = element(3,Response),
    ExpectedStatus = Status,
    %% check headers
    RespHeaders = element(4,Response),
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
                                                  , 1
                                                  , RespHeaders
                                                  ),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),

    ok.

%% post openc2 command and get expected result which contains body
post_oc2_body(JsonFileName, ExpectedStatus, ResultsFileName, Config) ->
    %% read json to send
    JsonTxt = read_json_file(JsonFileName, Config),
    %lager:debug("helper:JsonTxt= ~p", [JsonTxt]),
    %% validate Json
    true = is_json(JsonTxt),

    %% read and validate expected results
    ExpectedResultsTxt = read_json_file(ResultsFileName, Config),
    %lager:debug("helper:ExpectedResultsTxt= ~p", [ExpectedResultsTxt]),
    %% validate Json
    true = is_json(ExpectedResultsTxt),

    %% convert json to erlang terms
    ExpectedResults = jiffy:decode(ExpectedResultsTxt, [return_maps]),
    %lager:info("helper:ExpectedResults= ~p", [ExpectedResults]),

    %% open connection to send post
    ConnPid = make_conn(),

    %% setup headers to send
    Headers = [ {<<"content-type">>, <<"application/json">>} ],

    %% send json command to openc2
    StreamRef = gun:post(ConnPid, "/openc2", Headers, JsonTxt),

    %% check reply
    Response = gun:await(ConnPid,StreamRef),

    %% Check contents of reply
    response = element(1,Response),
    %% expect nofin since expecting a body
    nofin = element(2, Response),
    Status = element(3,Response),
    %% test status is what was expected
    ExpectedStatus = Status,
    %% check headers
    ResponseHeaders = element(4,Response),
    true= lists:member({<<"server">>,<<"Cowboy">>},ResponseHeaders),
    %% don't bother with date being correct, just check exists
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, ResponseHeaders),

    %% get the body of the reply
    {ok, ResponseBody} = gun:await_body(ConnPid, StreamRef),
    %lager:info("helper:RespBody= ~p", [ResponseBody]),

    %% test body is json
    true = is_json(ResponseBody),

    %% test body is what was expected by comparing decoded erlang
    %%    (so not failed based on diff order or spacing)
    ResponseErl = jiffy:decode( ResponseBody, [return_maps] ),
    ExpectedResults = ResponseErl,

    ok.

%% check if text is json by decoding
is_json(Txt) ->
  try jiffy:decode( Txt, [return_maps] ) of
    _ -> true
  catch
    _:_ -> false
  end.

%% include path in filename
full_data_file_name(Filename, Config) ->
    filename:join( ?config( data_dir, Config ), Filename ).

%% read a file containing valid json and return json object
read_json_file(Filename, Config) ->
    FullFilename = full_data_file_name(Filename, Config),
    read_json_file(FullFilename).

%% read a file containing valid json and return json object
read_json_file(Filename) ->
    Json = read_file(Filename),
    _JsonErl = jiffy:decode(Json),
    Json.

read_file(Filename) ->
    %% read text from a file
    {ok, Txt} = file:read_file(Filename),
    Txt.

%% make a connection
make_conn() ->
    MyPort = application:get_env(ocas, port, 8080),
    {ok, Conn} = gun:open("localhost", MyPort),
    Conn.
