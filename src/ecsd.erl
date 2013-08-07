-module(ecsd).

-export([start/0]).
-export([stop/0]).
-export([load/1]).
-export([lookup/1]).

-compile([nowarn_unused_function]).


-define(ECSD_SERVER, ecsd_server).


%% API

%% @doc Start ECSD_SERVER.
start() ->
    case whereis(?ECSD_SERVER) of
        undefined ->
            ecsd_server:start(),
            init();
        _ ->
            ok
    end,
    %start_gui().
    ok.

%% @doc Stop ECSD_SERVER.
stop() ->
    case whereis(?ECSD_SERVER) of 
        undefined -> ok;
        _ -> ecsd_server:stop()
    end.

%% @doc Load ecsd file.
load(FileName) ->
    start(),
    ecsd_server:insert(FileName).

%% @doc Fetch ecsd by NS.
lookup(NS) ->
    start(),
    ecsd_server:lookup(NS).

%% Internal.

init() ->
    [ecsd_server:insert(X) || X <- preload_ecsd()],
    ok.

preload_ecsd() ->
    ["sample/ewp.ecsd"].

start_gui() ->
    start_gui(which_gui()).

start_gui(wx) ->
    ecsd_gui:start();
start_gui(gs) ->
    todo.

which_gui() ->
    try
        wx:new(),
        wx:destroy(),
        wx
    catch _:_ ->
        gs
    end.
