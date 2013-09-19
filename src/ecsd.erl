-module(ecsd).

-include("include/ecsd.hrl").

-export([start/0]).
-export([stop/0]).
-export([load/1]).
-export([lookup/1]).


-export([parse_conf/1]).
-export([parse_conf/2]).
-export([parse_conf_from_file/1]).
-export([parse_conf_from_file/2]).

-export([unparse_conf/1]).

-compile([nowarn_unused_function]).
-compile([export_all, debug_info]).


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

%% @doc load conf
parse_conf(Bin) when is_binary(Bin) ->
    parse_conf(binary_to_list(Bin));
parse_conf(Str) when is_list(Str), is_integer(hd(Str)) ->
    case  re:run(Str, "^[\\s%]+using\\s+(ecsd://[^\\s]+)", [{capture, all_but_first, list}]) of
        {match, [NS]} ->
            parse_conf(NS, Str);
        _ ->
            {error, no_ecsd_header}
    end.
parse_conf(NS, Bin) when is_list(NS), is_binary(Bin) ->
    parse_conf(NS, binary_to_list(Bin));
parse_conf(NS, Str) when is_list(NS), is_list(Str) ->
    io:format("finding schema for namespace ~p...~n", [NS]), 
    case lookup(NS) of
        [] ->
            {error, no_schema_definition};
        [Schema] -> 
            case rytong_util:consult(Str) of
                {ok, Term} ->
                    parse(Schema, Term);
                _ ->
                    {error, bad_conf_content}
            end
    end.

parse_conf_from_file(Filename) when is_list(Filename) ->
    {ok, Bin} = file:read_file(Filename), 
    parse_conf(Bin).
parse_conf_from_file(NS, Filename) when is_list(NS), is_list(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    parse_conf(NS, Bin).

unparse_conf(#element{children = Elements}) ->
    [unparse(Ele) || Ele <- Elements].



%% Internal.

init() ->
    [ecsd_server:insert(X) || X <- preload_ecsd()],
    ok.

preload_ecsd() ->
    ["schema/adapter.ecsd"].

unparse(#element{annotation = Anot, children = Elements}) ->
    ok.

parse(#schema{annotation = Anot, elements = EleRules}, Term) ->
    % try
        #element{
            annotation = Anot,
            children = try_all(Term, EleRules, [])
        }.
    % catch
    %     _Type:Why ->
    %         io:format(" --failed to parse:~p~n", [Why]) 
    % end.

parse_element(Element, #element{annotation = Anot, children = Rules}) ->
    % io:format("==parsing element:~n~p~n", [Element]), 
    io:format("++using rule:~n~p~n", [Anot]), 
    Type = Anot#annotation.type,
    Tag = Anot#annotation.tag,
    ValueType = Anot#annotation.value,
    io:format("  {type,tag,value}:~p~n", [{Type,Tag,ValueType}]), 
    check_type(Element, Type),
    check_tag(Element, Tag),
    check_value(Type, Tag, ValueType, Element, Rules),
    Value = case Tag of
        undefined -> 
            % case Type of
            %     string -> Element;
            %     _ -> undefined
            % end;
            Element;
        _ ->
            erlang:element(2, Element) 
    end,
    io:format("\t--value:~p~n\t--rules:~p~n", [Value, Rules]), 
    case ValueType of
        undefined ->
            case Type of
                tuple -> 
                    #element{
                        annotation = Anot,
                        children = try_once(tuple_to_list(Value), Rules)
                    };
                list ->
                    #element{
                        annotation = Anot,
                        children = try_all(Value, Rules)
                    }
            end;
        tuple ->
            #element{
                annotation = Anot,
                children = try_once(tuple_to_list(Value), Rules)
            };
        list ->
            #element{
                annotation = Anot,
                children = try_all(Value, Rules, [])
            };
        oneof ->
            #element{
                annotation = Anot,
                children = try_all(Value, Rules, [])
            };
        _ ->
            #element{
                annotation = Anot,
                value = Value
            }
    end.

try_all([], _Rules, Acc) ->
    Acc;
try_all([Ele|Rest], Rules, Acc) ->
    io:format("=====[try-all]~n~p~n", [Ele]), 
    try_all(Rest, Rules, [try_all(Ele, Rules)|Acc]).
try_all(Element, [Rule|Rest]) ->
    try
        parse_element(Element, Rule)
    catch
        Class:Reason ->
            case Rest of
                [] -> erlang:raise(Class, Reason, erlang:get_stacktrace());
                _ -> try_all(Element, Rest)
            end
    end.

try_once(Elements, Rules) ->
    lists:map(fun({Ele, Rule}) ->
        io:format("=====[try-once]~n~p~n", [Ele]), 
        parse_element(Ele, Rule)
    end, lists:zip(Elements, Rules)).

check_type(Element, tuple) ->
    ret(is_tuple(Element), {type, tuple}); 
check_type(Element, list) ->
    ret((is_list(Element) andalso is_integer(hd(Element))==false), {type, list}); 
check_type(Element, string) ->
    ret(is_string(Element), {type, string}); 
check_type(_Element, constant) ->
    ret(true, {type, constant});
check_type(_Element, undefined) ->
    ret(true, {type, undefined});
check_type(_, _) ->
    ret(false, {type, not_supported}).

check_tag(_, undefined) -> ret(true, {tag, no_tag_needed});
check_tag({Tag, _}, Tag) -> ret(true, {tag, Tag});
check_tag(_, _) -> ret(false, {tag, not_match}).

check_value(list, undefined, _ValueType, _Ele, _Rules) ->
    throw(to_do_1); 
check_value(constant, _, Value, Ele, _Rules) ->
    ret(Value==Ele, {value, type_constant_any});
check_value(tuple, undefined, undefined, Ele, Rules) ->
    ret(erlang:size(Ele)==length(Rules), {value, type_tuple_undefined});
check_value(_Type, undefined, string_or_atom, _Ele, _Rules) ->
    ret(true, {value, type_undefined_string_or_atom});
check_value(Type, undefined, _ValueType, _Ele, Rules) ->
    ret((lists:member(Type, [string, atom]) andalso Rules==[]), {value, undefined});
check_value(tuple, _, tuple, {_, Value}, Rules) ->
    ret((is_tuple(Value) andalso erlang:size(Value)==length(Rules)), {value, type_tuple_tuple});
check_value(tuple, _, list, {_, Value}, Rules) ->
    ret((is_list(Value) andalso length(Rules)>0),{value, type_tuple_list});
check_value(tuple, _, string, {_, Value}, Rules) ->
    ret((is_string(Value) andalso Rules==[]),{value, type_tuple_string});
check_value(tuple, _, #oneof{type=constant, children=_C}, {_, _Value}, Rules) ->
    ret(Rules==[], {value, type_oneof_constant});
check_value(tuple, _, #oneof{type=list, children=_C}, {_, Value}, Rules) ->
    to_do,
    ret((is_string(Value)andalso(Rules==[])), {value, type_tuple_oneof_list});
check_value(tuple, _, integer, {_, Value}, Rules) ->
    ret((is_integer(Value) andalso Rules==[]), {value, type_tuple_integer});
check_value(tuple, _, boolean, {_, Value}, Rules) ->
    ret((is_boolean(Value) andalso Rules==[]), {value, type_tuple_boolean});
check_value(_Type, _Tag, _ValueType, _Ele, _Rules) ->
    ret(false, {value, not_supported}).



ret(false, {Test, Rule}) ->
    ErrMsg = lists:concat([" ** ", Test, "(", Rule, ") test failed"]), 
    io:format("~p~n", [ErrMsg]), 
    throw(check_failed);
ret(true, _) ->
    ok.

is_string(Str) when is_list(Str), is_integer(hd(Str)) -> true;
is_string(_) -> false.

is_string_or_atom(S) ->
    is_string(S) orelse is_atom(S).


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

