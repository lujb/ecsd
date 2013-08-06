-module(ecsd_server).
-behaviour(gen_server).


%% API
-export([start_link/0]).
-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-compile([export_all]).

% {namespace, "spec://rytong.com/conf/ewp"}.
% {anotation, [{id, "ewp_conf"}, {name, "Ewp Config"},{docstring, "something"}]}.

% {typedef, {string_or_atom, [string, atom]}}

% {element, [{anotation, [{name, "mobile_resolution"},
%                         {docstring, "xxx"},
%                         {type, tuple},
%                         {use, requied}
%                        ]
%            },
%            {element, [{anotation, [{type, const}, {value, channel]} }]},
%            {element, [{anotation, [{name, "name"},
%                                    {docstring, "sth"},
%                                    {type, string_or_atom}
%                                   ]
%            }]}
%           ]
% }.


% {element, [{anotation, [{name, "mobile_resolution"},
%                         {docstring, "xxx"},
%                         {type, list}
%                        ]
%            },
%            {elements, [{element, [{anotation, [{name, "name"},
%                                          {docstring, "sth"},
%                                          {type, list}]
%                             }
%                             {element, [{attr, []}]}
%                            ]
%                     }
%                    ]
%            }
%           ]
% }.


% {element, [{anotation, [{name, "mobile_resolution"},
%                         {docstring, "xxx"},
%                         {type, string}
%                        ]
%            }
%           ]
% }.
              
% value. | "value". | 12.    atom/string/number
% {v1, v2, v3}.                   tuple
% [v1, v2, v3...]                 list
% [{value, v1, v2}..]        any

-record(schema, {
    namespace,
    anotation,
    elements
    }).
-record(anotation, {
    name,
    docstring,
    type,
    use,
    value
    }).
-record(element, {
    anotation,
    elements
    }).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).
-define(D(Format, Data), io:format(Format, Data)).


%% API
%% @private

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
-spec stop() -> stopped.
stop() ->
    gen_server:call(?SERVER, stop).


%% gen_server.

%% @private
init([]) ->
    ?TABLE = ets:new(?TABLE, [set, protected, named_table]),
    {ok, []}.

%% @private
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
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
            do_load_schema(Content);
        {error, Reason} ->
            io:format("failed to load schema, for: ~p~n", [Reason]) 
    end.

do_load_schema(L) when is_list(L) ->
    #schema{
        namespace = parse_ns(proplists:get_value(namespace, L)),
        anotation = parse_anot(proplists:get_value(anotation, L)), 
        elements = parse_ele(proplists:lookup_all(element, L)) 
    }.

parse_ns(NS) when is_list(NS), is_integer(hd(NS)) -> NS.

parse_anot(Anots) when is_list(Anots) ->
    #anotation{
        name = proplists:get_value(name, Anots), 
        docstring = proplists:get_value(docstring, Anots), 
        type = proplists:get_value(type, Anots), 
        use = proplists:get_value(use, Anots), 
        value = proplists:get_value(value, Anots) 
    }.  

% {element, [{anotation, [{name, "mobile_resolution"},
%                         {docstring, "xxx"},
%                         {type, tuple},
%                         {use, requied}
%                        ]
%            },
%            {element, [{anotation, [{type, const}, {value, channel]} }]},
%            {element, [{anotation, [{name, "name"},
%                                    {docstring, "sth"},
%                                    {type, string_or_atom}
%                                   ]
%            }]}
%           ]
% }.
parse_ele(Elements) when is_list(Elements) ->
    parse_ele(Elements, []).

parse_ele([], Acc) ->
    lists:reverse(Acc); 
parse_ele([{element, Data} | Rest], Acc) ->
    Ele = #element{
        anotation = parse_anot(proplists:get_value(anotation, Data)),
        elements = parse_ele(proplists:lookup_all(element, Data))},
    parse_ele(Rest, [Ele | Acc]).

