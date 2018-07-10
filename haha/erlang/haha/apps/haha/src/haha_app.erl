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
%% @doc haha public API
%% @end
%%%-------------------------------------------------------------------

-module(haha_app).
-author("Duncan Sparrell").
-copyright("2018, Duncan Sparrell, sFractal Consulting").
%%-license(MIT).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).
-ignore_xref({start, 0}).

%%====================================================================
%% API
%%====================================================================

-spec start(_, _) -> {'ok', pid() }.
start(_StartType, _StartArgs) ->

    %% log pid of this
    lager:info("haha_app pid: ~p", [self()]),

    %% log what apps running
    Apps = application:which_applications(),
    lager:info("apps: ~p", [Apps]),

    %% start supervisor
    lager:info("starting supervisor"),
    haha_sup:start_link(),

    %% start webserver
    lager:info("starting webserver"),
    WebServerReturn = start_webserver(),

    %% log some info
    lager:info("webserver return: ~p", [WebServerReturn]),
    AppEnv = application:get_all_env(),
    lager:info("env: ~p", [AppEnv]),

    %% start env server
    EnvSvrReturn = oc_env:first_start(),
    lager:info("envserver return: ~p", [EnvSvrReturn]),

    %% return
    {ok, self()}.

%%--------------------------------------------------------------------
-spec start() -> {'error', {atom(), _}} | {'ok', [atom()]}.
start() ->
  application:ensure_all_started(ocas).


%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec start_webserver() -> pid().
start_webserver() ->
  %% which port to listen to
  {ok, Port} = application:get_env(port),
  lager:info("starting cowboy on port: ~p", [Port]),

  %% how many parallel listeners
  {ok, ListenerCount} = application:get_env(listener_count),
  lager:info("starting ~p listeners", [ListenerCount]),

  %% setup routes
  Routes =
    [
      {
        '_'  %virtual hostname (any host name)
      , [
          {"/status", status_handler, []}
        , {"/ok", status_ok_handler, []}  % returns ok if service working
        , {"/openc2", openc2_handler, []}    % handles the meat of openc2
        , {"/init", init_handler, []}    % handles starting/restarting the svr
        ]
      }
    ],
  Dispatch = cowboy_router:compile(Routes),

  %% start cowboy
  {ok, CowboyReturn} = cowboy:start_http( http
                             , ListenerCount
                             , [{port, Port}]
                             , [{env, [{dispatch, Dispatch}]}]
                             ),
  lager:info("Cowboy started returned: ~p", [CowboyReturn] ),

  %% return
  self().
