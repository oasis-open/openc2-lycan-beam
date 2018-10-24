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
  %%lager:info("got to openc2_handler init"),
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, _State) ->
    %%lager:info("got to allowed methods"),
    %%lager:info("allowed methods state ~p", [State]),
    State2 = #{},
    {[<<"POST">>], Req, State2}.

content_types_accepted(Req, State) ->
    %%lager:info("got to content_types"),
    %%lager:info("content_types_accepted state ~p", [State]),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req0, State0) ->
    %lager:info("got to handle_json"),
    %%lager:info("handle_json state ~p", [State0]),
    %%%%%%%%
    Results = process_json(Req0, State0),
    {HttpStatus, HttpErl, Req2, _State2} = Results,
    %%lager:info("handle_json HttpStatus ~p", [HttpStatus]),
    %%lager:info("handle_json HttpErl ~p", [HttpErl]),
    %%lager:info("handle_json req2 ~p", [Req2]),
    %%lager:info("handle_json state2 ~p", [State2]),
    ReturnBody = jiffy:encode(HttpErl),
    %%lager:info("handle_json ReturnBody ~p", [ReturnBody]),
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
                %%lager:info("process_json success"),
                Results
    catch
        _:_ -> {400, <<"error processing json command">>, Req, State0}
    end.

check_body(Req0, State0) ->
    %% validate then run commands
    %%lager:info("run_command starting"),
    %%lager:info("run_command Req0 ~p", [Req0]),
    %%lager:info("run_command State0 ~p", [State0]),
    %% check http has body
    HasBody = cowboy_req:has_body(Req0),
    %%lager:info("run_command HasBody ~p", [HasBody]),
    %% return error message if no body, otherwise continue on
    case HasBody of
        false ->
            lager:info("run_command: no http body"),
            {400, <<"Missing http body">>, Req0, State0};
        true ->
            %%lager:info("run_command: has http body"),
            is_json(Req0, State0)
    end.

  is_json(Req0, State0) ->
    lager:info("TODO need to get config for times"),
    { ok, Body, Req1} = cowboy_req:read_body(Req0
                                            , #{ length => 1000
                                               , period => 3000
                                               }
                                            ),

    %%lager:info("run_command Body: ~p", [Body] ),
    State1 = maps:put(http_body, Body, State0),
    %%lager:info("run_command State1: ~p", [State1] ),
    %% decode json, if fails return error, else continue on
    try jiffy:decode(Body, [return_maps]) of
        JsonMap ->
              %%lager:info("got to good jsonmap"),
              has_action(JsonMap, Req1, State1)
    catch
        _:_ -> {400, <<"Can not parse JSON">>, Req1, State1}
    end.

  has_action(JsonMap, Req1, State1) ->
    %%lager:info("has_action JsonMap: ~p", [JsonMap] ),
    State2 = maps:put(json_map, JsonMap, State1),

    %% Get list of top level fields
    %%     and validate id/action/target are present and no others
    TopFields = maps:keys(JsonMap),
    lager:info("top level fields ~p", [TopFields] ),
    %% check if action present, error if not, continue on if present
    case lists:member(<<"action">>, TopFields) of
        false ->
          lager:info("has_action: command missing action"),
          {400, <<"missing command action">>, Req1, State2};
        true ->
          %%lager:info("has_action: has action"),
          has_target(TopFields, JsonMap, Req1, State2)
    end.

  has_target(TopFields, JsonMap, Req1, State1) ->
    case lists:member(<<"target">>, TopFields) of
        false ->
          lager:info("has_target: command missing target"),
          {400, <<"missing command target">>, Req1, State1};
        true ->
          %%lager:info("has_target passed"),
          has_extra(TopFields, JsonMap, Req1, State1)
    end.

has_extra(TopFields, JsonMap, Req1, State1) ->
    %% check if extra top level fields
    case length(TopFields) of
        2 ->
          %%lager:info("run_command: command has correct # fields"),
          %% correct number so continue on
          check_action( JsonMap, Req1, State1);
        _ ->
          %% wrong number so return error
          lager:info("run_command: command has extra fields"),
          {400, <<"command has unknown extra fields">>, Req1, State1}
    end.


%% passed structure checks
check_action(JsonMap, Req1, State0) ->
    %%lager:info("passed structure checks"),
    %% check for correct action (query)
    ActionBin = maps:get( <<"action">>, JsonMap ),
    lager:info("ActionBin ~p", [ActionBin]),
    {TargetType, TargetSpecifiers} = target_decode(JsonMap),
    lager:info("check_target:TargetType, TargetSpecifiers ~p, ~p",
                [TargetType, TargetSpecifiers]),
    action_target( ActionBin
                 , TargetType
                 , TargetSpecifiers
                 , JsonMap
                 , Req1
                 , State0
                 ).

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
    Url = <<"https://github.com/sparrell/openc2-cap/slpfhw.cap.md">>,
    { ok, maps:put(<<"x_slpfhw">>, Url, Output) };

output_spec(<<"schema">>, Output) ->
  %% return new output map with schema information
  %lager:info("output_spec about to get filename"),
  case code:priv_dir(slpfhw) of
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
  case file:read_file(filename:join([PrivDir, "slpfhw.jadn"])) of
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

target_decode(JsonMap) ->
  %% all slpf targets consist of a key : [list] information
  TargetMap = maps:get(<<"target">>, JsonMap),
  lager:info("target_decode:TargetMap ~p", [TargetMap]),
  %% validate only one target type, throw exception if !=1
  TargetTypes = maps:keys(TargetMap),
  NumTartgetTypes = length(TargetTypes),
  lager:info("target_decode:NumTartgetTypes ~p", [NumTartgetTypes]),
  1 = NumTartgetTypes,
  %% if didn't throw exception, 1 target type so decode it
  [TargetType | _T] = TargetTypes,
  lager:info("target_decode:TargetType ~p", [TargetType]),
  TargetSpecifiers = maps:get(TargetType, TargetMap),
  lager:info("target_decode:TargetSpecifiers ~p", [TargetSpecifiers]),
  {TargetType, TargetSpecifiers}.

action_target( <<"query">> %ActionBin
             , <<"openc2">> %TargetType
             , TargetSpecifiers
             , _JsonMap
             , Req0
             , State0
             ) ->
    lager:info("action=query, target=openc2"),
    process_spec(TargetSpecifiers, Req0, State0);
action_target( <<"query">> %ActionBin
             , <<"hello">> %TargetType
             , [<<"world">>]
             , _JsonMap
             , Req0
             , State0
             ) ->
      lager:info("action=query, target=hello, specs=world"),
      %% valid so return hello world
      {200, <<"Hello World">>, Req0, State0};
action_target( <<"allow">> %ActionBin
              , <<"ip_addr">> %TargetType
              , _TargetSpecifiers
              , _JsonMap
              , _Req0
              , _State0
              ) ->
          lager:info("action=allow, target=ip_addr"),
          ok;

action_target( <<"deny">> %ActionBin
              , <<"ip_addr">> %TargetType
              , _TargetSpecifiers
              , _JsonMap
              , _Req0
              , _State0
              ) ->
          lager:info("action=deny, target=ip_addr"),
          ok;

action_target( _ActionBin
             , _TargetType
             , _TargetSpecifiers
             , _JsonMap
             , Req0
             , State0
             ) ->
    lager:info("bad action/target pair"),
    {400, <<"bad action/target pair">>, Req0, State0}.
