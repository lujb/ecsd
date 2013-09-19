%% Author: lu.jingbo@rytong.com
%% Created: 2013-8-5
%% Description: TODO: Add description to rytong_util
-module(rytong_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([consult/1]).

%%
%% API Functions
%%

consult(Str) when is_list(Str) ->
    scan(Str).


%% Internal Functions.
scan(Str) ->
    case erl_scan:string(Str)of
        {ok, Tok, _} ->
            parse(Tok);
        {error, {N, Mod, Desc}, _L} ->
            {error, {line(N), lists:concat(["error: ", Mod, ", desc: ", format_desc(erl_scan, Desc)])}}
    end.

parse(Tok) ->
    parse(Tok, [], []).
parse([], [], Rst) ->
    {ok, lists:reverse(Rst)};
parse([], Acc, Rst) ->
    do_parse([], Acc, Rst);
parse([T = {dot, _}|Rest], Acc, Rst)->
    do_parse(Rest, [T|Acc], Rst);
parse([T|Rest], Acc, Rst)->
    parse(Rest, [T|Acc], Rst).

do_parse(Tok, Acc, Rst) ->
    case erl_parse:parse_term(lists:reverse(Acc)) of
        {error, {N, Mod, Desc}} ->
            {error, {line(N), lists:concat(["error: ", Mod, ", desc: ", format_desc(erl_parse, Desc)])}};
        {ok, Term}->
            parse(Tok, [], [Term | Rst])
    end.


format_desc(M, Desc) ->
    lists:flatten(M:format_error(Desc)).

line(N) when is_integer(N), N>0 -> N;
line(_N) -> 1.

