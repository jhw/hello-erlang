%%%-------------------------------------------------------------------
%% @doc hello_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(hello_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/echo", echo_handler, []},
            {"/add", add_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    hello_erlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
