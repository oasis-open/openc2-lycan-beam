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
%% @doc haha top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(haha_sup).
-author("Duncan Sparrell").
-copyright("2018, Duncan Sparrell, sFractal Consulting").
%%-license(MIT).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% macro defining "SERVER" to contain the name of this module
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init([]) -> {'ok', {{'one_for_all', 0, 1}, []}}.
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
