-module(rai).
-export([run_udp/1, handle_udp/2]).


run_udp(PortNo) ->
    serv:start(
      {handler,
       fun() ->
               {ok, Sock} = gen_udp:open(PortNo, [binary, {active,true}]),
               #{ sock => Sock}
       end,
       fun ?MODULE:handle_udp/2}).
handle_udp({udp,Sock,_Host,_Port,Data}, State = #{ sock := Sock }) ->
    %% FIXME: Currently this only takes Pd messages for a single instance.
    tools:re_dispatch(
      Data,
      [{"\\s*(.*?)\\s+(.*?)\\s*;\n",
        fun([Var,Val]) -> 
                {FVal,[]} = string:to_float(Val),
                log:info("~p~n", [{Var,FVal}]),
                State
        end},
       {"",
        fun(_) -> exit({bad_data, data}) end}]);

handle_udp(Msg, State) ->
    obj:handle(Msg, State).
