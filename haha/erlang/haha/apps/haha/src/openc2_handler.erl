%%% @author Duncan Sparrell
%%% @copyright (C) 2017, sFractal Consulting LLC
%%%
-module(openc2_handler).
-author("Duncan Sparrell").
-license("MIT").

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

-export([init/2
        , allowed_methods/2
        , content_types_accepted/2
        , handle_json/2
        ]).

-ignore_xref({init, 3}).
-ignore_xref({allowed_methods, 2}).
-ignore_xref({content_types_accepted, 2}).
-ignore_xref({handle_json, 2}).

init( Req, Opts) ->
  lager:info("got to openc2_handler init"),
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    lager:info("got to allowed methods"),
    lager:info("allowed methods state ~p", [State]),
    State2 = #{},
    {[<<"POST">>], Req, State2}.

content_types_accepted(Req, State) ->
    lager:info("got to content_types"),
    lager:info("content_types_accepted state ~p", [State]),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req, State) ->
    lager:info("got to handle_json"),
    lager:info("handle_json state ~p", [State]),
    %% check for case of no body
    HasBody = cowboy_req:has_body(Req),
    body_check(HasBody, Req, State ).

%% handle case of whether body present or not
body_check(false, Req, State) ->
    lager:info("body_check state false ~p", [State]),
    %% no body so bad request
    State2 = maps:put(has_http_body, false, State),
    Req2 = cowboy_req:reply( 400
                           , #{<<"content-type">> => <<"text/html">>}
                           , <<"Missing http body.">>
                           , Req
                           ),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State2};

body_check(true, Req, State) ->
  lager:info("body_check state true ~p", [State]),
    %% body present so move to next test
    State2 = maps:put(has_http_body, true, State),

    %% get the body of the request, limiting to 1k charactions and 3 seconds
    { ok, Body, Req1} = cowboy_req:read_body(Req
                                            , #{ length => 1000
                                               , period => 3000
                                               }
                                            ),
    State3 = maps:put(http_body, Body, State2),

    %% check if body is json as it should be
    IsJson = jsx:is_json(Body),

    is_body_json(IsJson, Req1, State3).

is_body_json(false, Req, State) ->
    %% decoding json failed so bad request
    State2 = maps:put(good_json, false, State),
    Req2 = cowboy_req:reply( 400
                           , #{<<"content-type">> => <<"text/html">>}
                           , <<"Bad JSON">>
                           , Req
                           ),

    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State2};

is_body_json(true, Req, State) ->
    %% json decodes ok so move on to next test
    State2 = maps:put(good_json, true, State),

    %% Get the body (previously put in State)
    %%     convert to JSON
    Body = maps:get(http_body, State2),
    JsonMap = jsx:decode(Body, [return_maps]),
    lager:info("handle_json Json: InputMap ~p", [JsonMap] ),
    State3 = maps:put(json_map, JsonMap, State2),

    %% Get list of top level fields
    TopFields = maps:keys(JsonMap),
    lager:info("top level fields ~p", [TopFields] ),

    %% Check for id, action and target
    IdKeyExists = lists:member(<<"id">>, TopFields),
    ActionKeyExists = lists:member(<<"action">>, TopFields),
    TargetKeyExists = lists:member(<<"target">>, TopFields),

    %% pop id, action, target off list and anything esle shouldn't be there
    Leftovers = lists:delete(<<"target">>,
                            lists:delete(<<"action">>,
                                        lists:delete(<<"id">>,
                                                     TopFields))),
    %% check if any Leftovers
    LeftoverExists = lists:any(fun(E) -> is_binary(E) end, Leftovers),

    %% check overall structure - has id, action, target and nothing else
    structure_check( IdKeyExists
                   , ActionKeyExists
                   , TargetKeyExists
                   , LeftoverExists
                   , Req
                   , State3
                   ).

%% structure_check finds badly formed (for this actuator) commands
structure_check( false %% IdKeyExists should be there so return error
               , _ActionKeyExists
               , _TargetKeyExists
               , _LeftoverExists
               , Req
               , State
               ) ->
    lager:info("Missing id"),
    Req2 = cowboy_req:reply( 400
                           , #{<<"content-type">> => <<"text/html">>}
                           , <<"Missing ID">>
                           , Req
                           ),

    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State};

structure_check( _IdKeyExists
               , false %% ActionKeyExists should be there so return error
               , _TargetKeyExists
               , _LeftoverExists
               , Req
               , State
                 ) ->
      lager:info("Missing action"),
      Req2 = cowboy_req:reply( 400
                             , #{<<"content-type">> => <<"text/html">>}
                             , <<"Missing action">>
                             , Req
                             ),
      %% return (don't move on since request was bad)
      %%   is this correct return tuple?
      {ok, Req2, State};

structure_check( _IdKeyExists
               , _ActionKeyExists
               , false %% TargetKeyExists should be there so return error
               , _LeftoverExists
               , Req
               , State
               ) ->
      lager:info("Missing target"),
      Req2 = cowboy_req:reply( 400
                             , #{<<"content-type">> => <<"text/html">>}
                             , <<"Missing target">>
                             , Req
                             ),

      %% return (don't move on since request was bad)
      %%   is this correct return tuple?
      {ok, Req2, State};

structure_check( _IdKeyExists
               , _ActionKeyExists
               , _TargetKeyExists
               , true %% LeftovertExists should not be there so return error
               , Req
               , State
               ) ->
      lager:info("Additional fields"),
      Req2 = cowboy_req:reply( 400
                             , #{<<"content-type">> => <<"text/html">>}
                             , <<"Extra Fields??">>
                             , Req
                             ),

      %% return (don't move on since request was bad)
      %%   is this correct return tuple?
      {ok, Req2, State};

%% passed structure checks
structure_check( true  % IdKeyExists
               , true  % ActionKeyExists
               , true  % TargetKeyExists
               , false % LeftoverExists
               , Req
               , State
               ) ->
    lager:info("passed structure checks"),
    %% check for correct action (query)
    JsonMap = maps:get(json_map, State),
    ActionBin = maps:get( <<"action">>, JsonMap ),
    lager:info("ActionBin ~p", [ActionBin]),
    TargetBin = maps:get( <<"target">>, JsonMap ),
    lager:info("ActionBin ~p", [ActionBin]),
    query_check(ActionBin, TargetBin, Req, State).

query_check(<<"query">>, <<"Hello">>, Req, State) ->
    %% query action, Hello target
    %%figure out good reply;
    OuputJson = <<"World">>,
    Req2 = cowboy_req:reply( 200
                           , #{<<"content-type">> => <<"application/json">>}
                           , OuputJson
                           , Req
                           ),

    {ok, Req2, State};

query_check(<<"query">>, <<"what">>, Req, State) ->
    %% figure out
    OuputJson = <<"World">>,
    Req2 = cowboy_req:reply( 200
                           , #{<<"content-type">> => <<"application/json">>}
                           , OuputJson
                           , Req
                           ),
    {ok, Req2, State};

query_check(<<"query">>, _Target, Req, State) ->
    %% Bad target
    Req2 = cowboy_req:reply( 400
                           , #{<<"content-type">> => <<"text/html">>}
                           , <<"Bad Target, only profile, hello supported">>
                           , Req
                           ),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State};

query_check(_Action, _Target, Req, State) ->
    %% bad action
    Req2 = cowboy_req:reply( 400
                           , #{<<"content-type">> => <<"text/html">>}
                           , <<"Bad Action, only query supported">>
                           , Req
                           ),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State}.
