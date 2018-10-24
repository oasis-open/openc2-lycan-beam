%%% @author Duncan Sparrell
%%% @copyright (C) 2018, sFractal Consulting LLC
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
  %lager:info("got to openc2_handler init"),
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    %lager:info("got to allowed methods"),
    lager:info("allowed methods state ~p", [State]),
    %% covert from State being empty list to being an empty map
    State2 = #{},
    {[<<"POST">>], Req, State2}.

content_types_accepted(Req, State) ->
    %lager:info("got to content_types"),
    %lager:info("content_types_accepted state ~p", [State]),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req0, State0) ->
    %lager:info("got to handle_json"),
    %lager:info("handle_json state ~p", [State0]),
    Results = process_json(Req0, State0),
    {HttpStatus, HttpErl, Req2, State2} = Results,
    %lager:info("handle_json HttpStatus ~p", [HttpStatus]),
    %lager:info("handle_json HttpErl ~p", [HttpErl]),
    %lager:info("handle_json req2 ~p", [Req2]),
    lager:info("handle_json state2 ~p", [State2]),
    ReturnBody = jiffy:encode(HttpErl),
    %lager:info("handle_json ReturnBody ~p", [ReturnBody]),
    cowboy_req:reply(HttpStatus
                           , #{<<"content-type">> => <<"application/json">>}
                           , ReturnBody
                           , Req2
                           ).

process_json(Req, State0) ->
    %% execute the json after validating
    try check_body(Req, State0) of
        %% success so return results
        Results ->
                %lager:info("process_json success"),
                Results
    catch
        _:_ -> {400, <<"error processing json command">>, Req, State0}
    end.

check_body(Req0, State0) ->
    %% validate then run commands
    %lager:info("run_command starting"),
    %lager:info("run_command Req0 ~p", [Req0]),
    %lager:info("run_command State0 ~p", [State0]),
    %% check http has body
    HasBody = cowboy_req:has_body(Req0),
    %lager:info("run_command HasBody ~p", [HasBody]),
    %% return error message if no body, otherwise continue on
    case HasBody of
        false ->
            lager:info("run_command: no http body"),
            {400, <<"Missing http body">>, Req0, State0};
        true ->
            %lager:info("run_command: has http body"),
            is_json(Req0, State0)
    end.

  is_json(Req0, State0) ->
    lager:info("TODO need to get config for times"),
    { ok, Body, Req1} = cowboy_req:read_body(Req0
                                            , #{ length => 1000
                                               , period => 3000
                                               }
                                            ),

    lager:info("run_command Body: ~p", [Body] ),
    State1 = maps:put(http_body, Body, State0),
    %lager:info("run_command State1: ~p", [State1] ),
    %% decode json, if fails return error, else continue on
    try jiffy:decode(Body, [return_maps]) of
        JsonMap ->
              %lager:info("got to good jsonmap"),
              has_id(JsonMap, Req1, State1)
    catch
        _:_ -> {400, <<"Can not parse JSON">>, Req1, State1}
    end.

  has_id(JsonMap, Req1, State1) ->
    %lager:info("has_id JsonMap: ~p", [JsonMap] ),
    State2 = maps:put(json_map, JsonMap, State1),

    %% Get list of top level fields
    %%     and validate id/action/target are present and no others
    TopFields = maps:keys(JsonMap),
    %lager:info("top level fields ~p", [TopFields] ),
    %% check if id present, error if not, continue on if present
    case lists:member(<<"id">>, TopFields) of
        false ->
          lager:info("has_id: command missing id"),
          {400, <<"missing command id">>, Req1, State1};
        true ->
          %lager:info("has_id: command has id"),
          has_action(TopFields, JsonMap, Req1, State2)
    end.

  has_action(TopFields, JsonMap, Req1, State1) ->
    %% check if action present, error if not, continue on if present
    case lists:member(<<"action">>, TopFields) of
        false ->
          lager:info("has_action: command missing action"),
          {400, <<"missing command action">>, Req1, State1};
        true ->
          %lager:info("has_action: has action"),
          has_target(TopFields, JsonMap, Req1, State1)
    end.

  has_target(TopFields, JsonMap, Req1, State1) ->
    case lists:member(<<"target">>, TopFields) of
        false ->
          lager:info("has_target: command missing target"),
          {400, <<"missing command target">>, Req1, State1};
        true ->
          %lager:info("has_target passed"),
          has_extra(TopFields, JsonMap, Req1, State1)
    end.

has_extra(TopFields, JsonMap, Req1, State1) ->
    %% check if extra top level fields
    case length(TopFields) of
        3 ->
          %lager:info("run_command: command has correct # fields"),
          %% correct number so continue on
          check_action( JsonMap, Req1, State1);
        _ ->
          %% wrong number so return error
          lager:info("run_command: command has extra fields"),
          {400, <<"command has unknown extra fields">>, Req1, State1}
    end.


%% passed structure checks
check_action(JsonMap, Req1, State1) ->
    %lager:info("passed structure checks"),
    %% check for correct action (query)
    ActionBin = maps:get( <<"action">>, JsonMap ),
    %lager:info("ActionBin ~p", [ActionBin]),
    case ActionBin of
        %% if query, continue otherwise error
        <<"query">> ->
            check_target(JsonMap, Req1, State1);
        _ ->
            {400, <<"unknown action">>, Req1, State1}
    end.

check_target(JsonMap, Req, State) ->
    TargetBin = maps:get( <<"target">>, JsonMap ),
    %lager:info("TargetBin ~p", [TargetBin]),
    %% check if simple case of Hello World
    TargetIsBinary = is_binary(TargetBin),
    binary_target(TargetIsBinary, TargetBin, Req, State).

binary_target(true, <<"Hello World">>, Req, State) ->
  %% TargetIsBinary = true, TargetBin = <<"Hello World">>
  %% valid so return hello world
  %lager:info("binary_target Hello World"),
  {200, <<"Hello World">>, Req, State};

binary_target(true, _TargetBin, Req, State) ->
  %% TargetIsBinary = true, TargetBin != <<"Hello World">> therefore error
  lager:info("binary_target but not Hello World"),
  {400, <<"Bad Target">>, Req, State};

binary_target(false, TargetBin, Req, State) ->
  %% TargetIsBinary = false, so must be map
  %lager:info("binary_target map"),
  map_target(is_map(TargetBin), TargetBin, Req, State).

map_target(false, _TargetMap, Req, State) ->
  %% Target is neither binary text nor map so error out
  lager:info("map_target false"),
  {400, <<"Bad Target">>, Req, State};

map_target(true, TargetMap, Req, State) ->
  %% Target is map so check only one target
  %lager:info("map_target true"),
  num_target(length(maps:keys(TargetMap)), TargetMap, Req, State).

num_target(0, _TargetMap, Req, State) ->
  %% target is empty map therefore error
  lager:info("num_target target is empty map"),
  {400, <<"Missing Target Info">>, Req, State};

num_target(1, TargetMap, Req, State) ->
  %% one target type so check it is correct one
  %lager:info("num_target has 1 target"),
  TargetType = lists:nth(1,maps:keys(TargetMap)),
  check_target_type(TargetType, TargetMap, Req, State);

num_target(_, _TargetMap, Req, State) ->
  %% too many targets therefore error
  lager:info("num_target has too many targets"),
  {400, <<"only one target type allowed">>, Req, State}.

check_target_type(<<"openc2">>, TargetMap, Req, State) ->
  %% TargetType = openc2
  %lager:info("check_target_type openc2 target"),
  SpecifierList = maps:get(<<"openc2">>,TargetMap),
  %lager:info("check_target_type SpecifierList ~p", [SpecifierList]),
  process_spec(SpecifierList, Req, State);

check_target_type(_TargetType, _TargetMap, Req, State) ->
  %% TargetType != openc2
  lager:info("check_target_type unknown target"),
  {400, <<"unknown target">>, Req, State}.

process_spec(SpecifierList, Req, State) ->
    %lager:info("process_spec SpecList ~p", [SpecifierList]),

    case process_spec_list(SpecifierList, #{}) of
        {error, ErrorMsg} ->
            lager:info("process_spec error ~p", [ErrorMsg]),
            {400, <<"Problem with Target Specifiers">>, Req, State};
        {ok, Output} ->
            %lager:info("process_spec output ~p", [Output]),
            {200, Output, Req, State}
        end.

%% process_spec_list(SpecList, Output) creates output depending on specifiers
process_spec_list([], Output) ->
    %% spec list now empty so done
    %lager:info("process_spec complete Output ~p", [Output]),
    {ok, Output};
process_spec_list([H | T], Output) ->
      %% recurse thru specs creating output
      case output_spec(H, Output) of
          %% output_spec returns {Status, NewOutputList} or {error, error msg}
          {error, ErrorMsg} ->
              lager:info("got to process_spec error ~p", [ErrorMsg]),
              {error, ErrorMsg};
          {ok, NewOutputList} ->
              %lager:info("got to process_spec NewOutputList ~p", [NewOutputList]),
              process_spec_list(T, NewOutputList)
          end.

%% output_spec creates output for a particular specifiers
output_spec(<<"profile">>, Output) ->
    %% return new output map with profile information
    Url = <<"https://github.com/sparrell/openc2-cap/haha.cap.md">>,
    { ok, maps:put(<<"x_haha">>, Url, Output) };

output_spec(<<"schema">>, Output) ->
  %% return new output map with schema information
  %lager:info("output_spec about to get filename"),
  case code:priv_dir(haha) of
        {error, bad_name} ->
            % This occurs when not running as a release; e.g., erl -pa ebin
            % Of course, this will not work for all cases, but should account
            % for most
            PrivDir = "priv";
        PrivDir ->
            % In this case, we are running in a release and the VM knows
            % where the application (and thus the priv directory) resides
            % on the file system
            ok
  end,
  %lager:info("output_spec about to read file"),
  case file:read_file(filename:join([PrivDir, "haha.jadn"])) of
    {ok, Schema} ->
        %lager:info("output_spec read file ok ~p", [Schema]),
        ok;
    AnyThingElse ->
        lager:info("output_spec trouble reading file ~p", [AnyThingElse]),
        Schema = "oops"
  end,
  %lager:info("output_spec schema: ~p", [Schema]),
  %% convert json text to erlang terms to put in map
  SchemaMap = jiffy:decode(Schema),
  %lager:info("output_spec schemamap: ~p", [SchemaMap]),
  %lager:info("output_spec preOutput: ~p", [Output]),
  { ok, maps:put(<<"schema">>, SchemaMap, Output) };

output_spec(<<"version">>, Output) ->
  %% return new output map with schema information
  Version = <<"1.0">>,
  { ok, maps:put(<<"version">>, Version, Output) };

output_spec(_, _) ->
  %% unknown specifier
  {error, "unknown specifier"}.
