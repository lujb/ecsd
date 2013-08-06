
-module(ecsd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Procs = [{ecsd_server, {ecsd_server, start_link, []},
        permanent, 5000, worker, [ecsd_server]}],
    {ok, {{one_for_one, 5, 10}, Procs} }.

