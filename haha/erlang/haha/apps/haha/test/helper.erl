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
    nofin = element(2, Response),
    %fin = element(2, Response),
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

    %% read and validate expected results
    ExpectedResultsTxt = read_json_file(ResultsFileName, Config),

    %% convert json to erlang terms
    ExpectedResults = jiffy:decode(ExpectedResultsTxt, [return_maps]),

    %% open connection to send post
    Conn = make_conn(),

    %% send post
    {ok, Response} = gun:post( Conn
                                 , "/openc2"  % Url
                                 , [ { <<"content-type">>  % ReqHeaders
                                     , <<"application/json">>
                                     }
                                   ]
                                 , JsonTxt  % ReqBody
                                 , #{}      % Options
                                 ),
    %% compare expected status to resonse status
    ResponseStatus = maps:get(status_code, Response),
    ExpectedStatus = ResponseStatus,

    check_status(ExpectedResults, Response),

    %% check headers
    check_headers(Response),

    %% check has body and is json
    #{ body := RespBody } = Response,

    %% decode json into erlang map
    JsonMap = jiffy:decode( RespBody, [return_maps] ),

    %% check all components are in response json
    check_keys(JsonMap, ExpectedResults),

    %% check correct return values in response
    check_json_values(JsonMap, ExpectedResults),

    ok.
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

%% check status of a response
check_status(ExpectedResults, Response) ->
    ExpectedStatus = maps:get(<<"ExpectedStatus">>, ExpectedResults),
    ResponseStatus = maps:get(status_code, Response),
    ExpectedStatus = ResponseStatus,
    ok.

%% check response headers
check_headers(Response) ->
    %% get the headers out of the response

    #{ headers := RespHeaders } = Response,

    %% verify headers
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
                                                  , 1
                                                  , RespHeaders
                                                  ),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
    ok.

check_keys(JsonMap, ExpectedResults) ->
    %% get expected keys
    ExpectedKeys = maps:get(<<"ExpectedJsonKeys">>, ExpectedResults),

    %% get response keys
    ResponseKeys = maps:keys(JsonMap),
    lager:info("ResponseKeys: ~p", [ResponseKeys]),

    %% check expected are in response
    check_key(ResponseKeys, ExpectedKeys),
    JsonMap.

check_key(_ResultKeys, [] ) ->
    %% done since list empty
    ok;

check_key(ResultKeys, [Key | RestOfKeys] ) ->
    %% check key is in Results
    lager:info("check_key: ~p", [Key]),
    true = lists:member(Key, ResultKeys),

    %% recurse thru rest of list
    check_key(ResultKeys, RestOfKeys).

%% check response contains key/values expected
check_json_values(JsonMap, ExpectedResults) ->
    %% get expected key/value map
    ExpectedJsonPairMap = maps:get(<<"ExpectedJsonPairs">>, ExpectedResults),
    ExpectedJsonPairs = maps:to_list(ExpectedJsonPairMap),
    lager:info("ResponseMap: ~p", [JsonMap]),

    %% recurse thru {key,value} in ExpectedJsonPairs looking for match
    check_json_pair( ExpectedJsonPairs, JsonMap).

check_json_pair( [], _JsonMap) ->
    % done
    ok;

check_json_pair( [ {Key, Value} | RestOfExepectedPairs ], JsonMap) ->
    %% check in key/value in json map
    lager:info("key/value: ~p/~p", [Key, Value]),
    Value = maps:get(Key, JsonMap),

    %% recurse on to next pair
    check_json_pair( RestOfExepectedPairs, JsonMap).
