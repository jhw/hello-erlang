-module(echo_handler).
-export([init/2]).

init(Req0, State) ->
    #{message := Message} = cowboy_req:match_qs([{message, [], <<>>}], Req0),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Message,
        Req0),
    {ok, Req, State}.
