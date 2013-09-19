-module(ecsd_server).
-behaviour(gen_server).

-include("include/ecsd.hrl").

%% API
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-export([insert/1]).
-export([lookup/1]).

-export([index/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-compile([export_all]).



-record(state, {index}).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).
-define(D(Format, Data), io:format(Format, Data)).


%% API
%% @private

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
    gen_server:call(?SERVER, stop).

insert(FileName) when is_list(FileName), is_integer(hd(FileName))->
    gen_server:call(?SERVER, {insert, FileName}). 

lookup(NS) ->
    gen_server:call(?SERVER, {lookup, NS}).

index() ->
    gen_server:call(?SERVER, index). 


%% gen_server.

%% @private
init([]) ->
    ?TABLE = ets:new(?TABLE, [set, {keypos, 2}, protected, named_table]),
    {ok, #state{index=[]}}.

%% @private
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({insert, FileName}, _From, #state{index = I} = State) ->
    Schema = load_schema(FileName),
    NS = Schema#schema.namespace,
    Name = Schema#schema.annotation#annotation.name,
    Resp = 
        case ets:insert_new(?TABLE, Schema) of
            true -> ok;
            _ -> insert_error
        end,
    {reply, Resp, State#state{index = [{NS, Name}|I]}};
handle_call({lookup, NS}, _From, State) ->
    Resp = ets:lookup(?TABLE, NS),
    {reply, Resp, State};
handle_call(index, _From, State) ->
    {reply, State#state.index, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal.

load_schema(FileName) when is_list(FileName) ->
    ?D("loading schema file: ~p...~n", [FileName]),
    case file:consult(FileName) of 
        {ok, Content} ->
            Schema = do_load_schema(Content),
            Schema#schema{location = FileName};
        {error, Reason} ->
            io:format("failed to load schema, for: ~p~n", [Reason]) 
    end.

do_load_schema(L) when is_list(L) ->
    #schema{
        namespace = parse_ns(proplists:get_value(namespace, L)),
        annotation = parse_anot(proplists:get_value(annotation, L)), 
        elements = parse_ele(proplists:lookup_all(element, L)) 
    }.

parse_ns(NS) when is_list(NS), is_integer(hd(NS)) -> NS.

parse_anot(Anots) when is_list(Anots) ->
    #annotation{
        name = proplists:get_value(name, Anots), 
        docstring = proplists:get_value(docstring, Anots), 
        type = proplists:get_value(type, Anots), 
        tag = proplists:get_value(tag, Anots), 
        use = proplists:get_value(use, Anots), 
        repeat = proplists:get_value(repeat, Anots), 
        onlywhen = proplists:get_value(onlywhen, Anots), 
        value = parse_value(proplists:get_value(value, Anots))
    }.  

parse_ele(Elements) when is_list(Elements) ->
    parse_ele(Elements, []).

parse_ele([], Acc) ->
    lists:reverse(Acc); 
parse_ele([{element, Data} | Rest], Acc) ->
    Ele = #element{
        annotation = parse_anot(proplists:get_value(annotation, Data)),
        children = parse_ele(proplists:lookup_all(element, Data))},
    parse_ele(Rest, [Ele | Acc]);
parse_ele([Constant | Rest], Acc) ->
    Ele = #element{
        annotation = #annotation{
            type = constant,
            value = Constant
        }
    },
    parse_ele(Rest, [Ele | Acc]).

parse_value({oneof, Elements}) when is_list(Elements) ->
    #oneof{children = parse_ele(Elements)};
parse_value({oneof, {EleName, list}}) when is_list(EleName), is_integer(hd(EleName)) ->
    #oneof{
        type = list,
        children = EleName
    };
parse_value(ValueType) ->
    ValueType.

