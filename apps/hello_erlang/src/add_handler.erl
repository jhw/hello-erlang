-module(add_handler).
-export([init/2]).

%% Simple addition handler that crashes on invalid input
%% This is intentional - we want to test error logging to Slack
%%
%% Valid: GET /add?x=5&y=10 -> returns 15
%% Invalid: GET /add?x=foo&y=bar -> crashes and triggers error alert

init(Req0, State) ->
    #{x := X, y := Y} = cowboy_req:match_qs([{x, [], <<>>}, {y, [], <<>>}], Req0),

    %% NO try/catch - let it crash if inputs are invalid!
    %% This follows the Erlang "let it crash" philosophy
    XInt = binary_to_integer(X),
    YInt = binary_to_integer(Y),
    Result = XInt + YInt,

    ResultBin = integer_to_binary(Result),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        ResultBin,
        Req0),
    {ok, Req, State}.
