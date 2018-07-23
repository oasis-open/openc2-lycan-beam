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

handle_json(Req0, State0) ->
    lager:info("got to handle_json"),
    lager:info("handle_json state ~p", [State0]),
    %%%%%%%%
    Results = process_json(Req0, State0),
    {HttpStatus, HttpErl, Req2, State2} = Results,
    lager:info("handle_json HttpStatus ~p", [HttpStatus]),
    lager:info("handle_json HttpErl ~p", [HttpErl]),
    lager:info("handle_json req2 ~p", [Req2]),
    lager:info("handle_json state2 ~p", [State2]),
    ReturnBody = jiffy:encode(HttpErl),
    lager:info("handle_json ReturnBody ~p", [ReturnBody]),
    Req3 = cowboy_req:reply(HttpStatus
                           , #{<<"content-type">> => <<"application/json">>}
                           , ReturnBody
                           , Req2
                           ),
    {true, Req3, State2}.

process_json(Req, State0) ->
    %% execute the json after validating
    try check_body(Req, State0) of
        %% success so return results
        Results ->
                lager:info("process_json success"),
                Results
    catch
        _:_ -> {400, <<"error processing json">>, Req, State0}
    end.

check_body(Req0, State0) ->
    %% validate then run commands
    lager:info("run_command starting"),
    lager:info("run_command Req0 ~p", [Req0]),
    lager:info("run_command State0 ~p", [State0]),
    %% check http has body
    HasBody = cowboy_req:has_body(Req0),
    lager:info("run_command HasBody ~p", [HasBody]),
    %% return error message if no body, otherwise continue on
    case HasBody of
        false ->
            lager:info("run_command: no http body"),
            {400, <<"Missing http body">>, Req0, State0};
        true ->
            lager:info("run_command: has http body"),
            is_json(Req0, State0)
    end.

  is_json(Req0, State0) ->
    lager:info("TODO need to get config for times"),
    lager:info("TODO fix is_json and jsx/jiffy incl in tests"),
    { ok, Body, Req1} = cowboy_req:read_body(Req0
                                            , #{ length => 1000
                                               , period => 3000
                                               }
                                            ),

    lager:info("run_command Body: ~p", [Body] ),
    State1 = maps:put(http_body, Body, State0),
    lager:info("run_command State1: ~p", [State1] ),
    %% decode json, if fails return error, else continue on
    try jiffy:decode(Body, [return_maps]) of
        JsonMap ->
              lager:info("got to good jsonmap"),
              has_id(JsonMap, Req1, State1)
    catch
        _:_ -> {400, <<"Can not parse JSON">>, Req1, State1}
    end.

  has_id(JsonMap, Req1, State1) ->
    lager:info("has_id JsonMap: ~p", [JsonMap] ),
    State2 = maps:put(json_map, JsonMap, State1),

    %% Get list of top level fields
    %%     and validate id/action/target are present and no others
    TopFields = maps:keys(JsonMap),
    lager:info("top level fields ~p", [TopFields] ),
    %% check if id present, error if not, continue on if present
    case lists:member(<<"id">>, TopFields) of
        false ->
          lager:info("has_id: command missing id"),
          {400, <<"missing command id">>, Req1, State1};
        true ->
          lager:info("has_id: command missing id"),
          has_action(TopFields, JsonMap, Req1, State2)
    end.

  has_action(TopFields, JsonMap, Req1, State1) ->
    %% check if action present, error if not, continue on if present
    case lists:member(<<"action">>, TopFields) of
        false ->
          lager:info("has_action: command missing action"),
          {400, <<"missing command action">>, Req1, State1};
        true ->
          lager:info("has_action: has action"),
          has_target(TopFields, JsonMap, Req1, State1)
    end.

  has_target(TopFields, JsonMap, Req1, State1) ->
    case lists:member(<<"target">>, TopFields) of
        false ->
          lager:info("has_target: command missing target"),
          {400, <<"missing command target">>, Req1, State1};
        true ->
          lager:info("has_target passed"),
          has_extra(TopFields, JsonMap, Req1, State1)
    end.

has_extra(TopFields, JsonMap, Req1, State1) ->
    %% check if extra top level fields
    case length(TopFields) of
        3 ->
          lager:info("run_command: command has correct # fields"),
          %% correct number so continue on
          check_action( JsonMap, Req1, State1);
        _ ->
          lager:info("run_command: command has extra fields"),
          {400, <<"command has unknown extra fields">>, Req1, State1}
    end.


%% passed structure checks
check_action(JsonMap, Req1, State1) ->
    lager:info("passed structure checks"),
    %% check for correct action (query)
    ActionBin = maps:get( <<"action">>, JsonMap ),
    lager:info("ActionBin ~p", [ActionBin]),
    case ActionBin of
        %% if query, continue otherwise error
        <<"query">> ->
            check_target(JsonMap, Req1, State1);
        _ ->
            {400, <<"unknown action">>, Req1, State1}
    end.

check_target(JsonMap, Req, State) ->
    TargetBin = maps:get( <<"target">>, JsonMap ),
    lager:info("TargetBin ~p", [TargetBin]),
    %% parse the type of target so can select on it
    %%  TargetInfo = { TargetIsBinary, TargetIsMap, TopTarget, SpecifierList}
    TargetInfo = target_typing(TargetBin),
    lager:info("sc: TargetInfo = ~p", [TargetInfo]),
    query_check(TargetInfo, Req, State).

target_typing(TargetBin) ->
    TargetIsBinary = is_binary(TargetBin),
    TargetIsMap = is_map(TargetBin),
    {TopTarget, SpecifierList} = string_map( TargetIsBinary
                                           , TargetIsMap
                                           , TargetBin
                                           ),
    {TargetIsBinary, TargetIsMap, TopTarget, SpecifierList}.

string_map(true, _TargetIsMap, TargetBin) ->
    TopTarget = TargetBin,
    SpecifierList = [],
    {TopTarget, SpecifierList};
string_map(false, true, TargetBin) ->
    lager:info("stringmap TargetBin as map"),
    TargetList = maps:keys(TargetBin),
    %% error if not one and only one target in map
    case length(TargetList) of
        0 ->
          {error, ["only one target allowed"]};
        1 ->
          %% the one item is the target
          TopTarget = lists:nth(1,TargetList),
          SpecifierMap = maps:get(TopTarget,TargetBin),
          SpecifierList = maps:keys(SpecifierMap),
          {TopTarget, SpecifierList};
        _ ->
          {error, ["only one target allowed"]}
    end;
string_map(false, false, _) ->
    %% bad target value since neither string nor map
    {error, ["bad target json"]}.

%% query_check
%%  query_check(ActionBin, TargetInfo, Req, State)
%%    TargetInfo = {TargetIsBinary, TargetIsMap, TopTarget, SpecifierList}
%%
%% case where ActionBin=query, TargetIsBinary=true, target= whatareyou
query_check({true, false, <<"whatareyou">>, []}, Req, State) ->
    %% query action, whatareyou target
    %%figure out good reply;
    HelloWorld = <<"Hello World">>,
    OuputJson = jiffy:encode(HelloWorld),
    Req2 = cowboy_req:reply( 200
                           , #{<<"content-type">> => <<"application/json">>}
                           , OuputJson
                           , Req
                           ),
    lager:info("query_check whatareyou"),
    {true, Req2, State};

%% case where ActionBin=query, TargetIsBinary=true, target= openc2,
%%     with no target specifiers (do return all)
query_check({true, false, <<"openc2">>, []}, Req, State) ->
    %% figure out
    HelloWorld = <<"Need to figure out openc2=all still">>,
    OuputJson = jiffy:encode(HelloWorld),
    Req2 = cowboy_req:reply( 200
                           , #{<<"content-type">> => <<"application/json">>}
                           , OuputJson
                           , Req
                           ),
    lager:info("query_check openc2 binary"),
    {true, Req2, State};

%% case where ActionBin=query, TargetIsMap=true, target= openc2,
%%     with target specifiers
query_check({false, true, <<"openc2">>, SpecList}, Req, State) ->
  %% figure out
  lager:info("query_check SpecList ~p", [SpecList]),
  {HtmlCode, OutputJson} = case process_spec_list(SpecList, #{}) of
      {error, _ErrorMsg} ->
          {400, jiffy:encode(<<"Problem with Target Specifiers">>)};
      {ok, Output} ->
          lager:info("query_check output ~p", [Output]),
          {200, jiffy:encode(Output)}
      end,

  Req2 = cowboy_req:reply( HtmlCode
                         , #{<<"content-type">> => <<"application/json">>}
                         , OutputJson
                         , Req
                         ),
  lager:info("query_check openc2 map"),
  {true, Req2, State};

query_check(_Target, Req, State) ->
    %% Bad target
    ErrorMsg = <<"Bad Target">>,
    Req2 = cowboy_req:reply( 400
                           , #{<<"content-type">> => <<"text/html">>}
                           , ErrorMsg
                           , Req
                           ),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State}.

%% process_spec_list(SpecList, Output) creates output depending on specifiers
process_spec_list([], Output) ->
    %% spec list now empty so done
    {ok, Output};
process_spec_list([H | T], Output) ->
      %% recurse thru specs creating output
      case output_spec(H, Output) of
          %% output_spec returns {Status, NewOutputList} or {error, error msg}
          {error, ErrorMsg} ->
              lager:info("got to process_spec error ~p", [ErrorMsg]),
              {error, ErrorMsg};
          {ok, NewOutputList} ->
              lager:info("got to process_spec NewOutputList ~p", [NewOutputList]),
              process_spec_list(T, NewOutputList)
          end.

%% output_spec creates output for a particular specifiers
output_spec(<<"profile">>, Output) ->
    %% return new output map with profile information
    Url = <<"get url from config and put here">>,
    { ok, maps:put(<<"x_haha">>, Url, Output) };

output_spec(<<"schema">>, Output) ->
  %% return new output map with schema information
  Schema = <<"figure out how to put schema here">>,
  { ok, maps:put(<<"schema">>, Schema, Output) };

output_spec(_, _) ->
  %% unknown specifier
  {error, "unknown specifier"}.
