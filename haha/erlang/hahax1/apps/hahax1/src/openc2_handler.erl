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

process_json(Req0, State0) ->
    %% validate then run commands
    lager:info("run_command starting"),
    lager:info("run_command Req0 ~p", [Req0]),
    lager:info("run_command State0 ~p", [State0]),
    %% check http has body
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
    JsonMap = jiffy:decode(Body, [return_maps]),

    lager:info("has_id JsonMap: ~p", [JsonMap] ),
    State2 = maps:put(json_map, JsonMap, State1),

    %% error out if doesn't have id
    _Id = maps:get( <<"id">>, JsonMap ),

    %% error out if doesn't have action or if != query
    <<"query">> = maps:get( <<"action">>, JsonMap ),

    %% error out if doesn't have target
    _Target = maps:get( <<"target">>, JsonMap ),

    %% error out if extra fields
    3 = length(maps:keys(JsonMap)),

    TargetBin = maps:get( <<"target">>, JsonMap ),
    lager:info("TargetBin ~p", [TargetBin]),

    %% if binary then must be Hello World, otherwise must be openc2
    case TargetBin of
        <<"Hello World">> ->
            %% do hw stuff
            lager:info("got to hello world in new"),
            {200, <<"Hello World">>, Req1, State2};
        #{<<"openc2">> := TargSpecifierMap} ->
            lager:info("got to openc2 in new ~p", [TargSpecifierMap]),
            SpecifierList = maps:keys(TargSpecifierMap),
            process_spec(SpecifierList, Req1, State2)
    end.

process_spec(SpecifierList, Req, State) ->
    lager:info("process_spec SpecList ~p", [SpecifierList]),
    Output = process_spec_list(SpecifierList, #{}),
    {200, Output, Req, State}.

%% process_spec_list(SpecList, Output) creates output depending on specifiers
process_spec_list([], Output) ->
    %% spec list now empty so done
    lager:info("process_spec complete Output ~p", [Output]),
    Output;
process_spec_list([H | T], Output) ->
      %% recurse thru specs creating output
      NewOutputList = output_spec(H, Output),
      process_spec_list(T, NewOutputList).

%% output_spec creates output for a particular specifiers
output_spec(<<"profile">>, Output) ->
    %% return new output map with profile information
    Url = <<"https://github.com/sparrell/openc2-cap/haha.cap.md">>,
    maps:put(<<"x_hahax1">>, Url, Output);

output_spec(<<"schema">>, Output) ->
  %% return new output map with schema information
  lager:info("output_spec about to get filename"),
  PrivDir = code:priv_dir(hahax1),
  lager:info("output_spec about to read file"),
  {ok, Schema} = file:read_file(filename:join([PrivDir, "hahax1.jadn"])),
  lager:info("output_spec schema: ~p", [Schema]),
  %% convert json text to erlang terms to put in map
  SchemaMap = jiffy:decode(Schema),
  lager:info("output_spec schemamap: ~p", [SchemaMap]),
  lager:info("output_spec preOutput: ~p", [Output]),
  NewOutput = maps:put(<<"schema">>, SchemaMap, Output),
  lager:info("output_spec NewOutput: ~p", [NewOutput]),
  maps:put(<<"schema">>, SchemaMap, Output);

output_spec(<<"version">>, Output) ->
  %% return new output map with schema information
  Version = <<"1.0">>,
  maps:put(<<"version">>, Version, Output).
