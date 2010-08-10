-module(botnet_scheduler).
-export([run/2, loop/2]).

run({M,F,A}, {X, times_each, second}) when is_integer(X) ->
    run({M,F,A}, 1000 div X);
run({M,F,A}, {X, times_each, minute}) when is_integer(X) ->
    run({M,F,A}, (1000 div X) * 60);
run({M,F,A}, {X, times_each, hour}) when is_integer(X) ->
    run({M,F,A}, (1000 div X) * 60 * 60);
run({M,F,A}, {X, times_each, day}) when is_integer(X) ->
    run({M,F,A}, (1000 div X) * 60 * 60 * 24);
run({M,F,A}, MillisecondFreq) when is_integer(MillisecondFreq) ->
    spawn_link(?MODULE, loop, [{M,F,A}, MillisecondFreq]).

loop({M,F,A}, MillisecondFreq) ->
    Before = now(),
    case botnet:lock_node() of
        undefined -> io:format("no available nodes!~n");
        Node ->
            rpc:cast(Node, M, F, A),
            botnet:unlock_node(Node)
    end,
    After = now(),
    Wait =
        case MillisecondFreq - (timer:now_diff(After, Before) div 1000) of
            X when X > 0 -> X;
            _ -> 0
        end,
    receive
        {stop, Caller} -> Caller ! {ok, self()}
    after Wait ->
        loop({M,F,A}, MillisecondFreq)
    end.