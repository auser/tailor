-module(sar_collect).

-export([systat/0, systat/1, start/0, start/1]).

start() ->
	start(59).
	
start(Seconds) ->
        spawn(?MODULE, systat, [Seconds]).

% update the file every second        
systat(0) -> 
        loop(1);

systat(Seconds) ->
        loop(Seconds).

%update the file every 59 seconds
systat() ->
        loop(59).

loop(Seconds) when Seconds < 60 ->
 Cmd = lists:flatten([ "sar -o /tmp/last.tmp ", integer_to_list(Seconds),  " 1", " -A" ]),
	io:format("Calling ~p~n", [Cmd]),
 execute(".", Cmd),
 file:rename("/tmp/last.tmp", "/tmp/last"),
 timer:sleep(60 - Seconds),
 receive
  stop ->
   exit(normal);

  {interval, NewSeconds} ->
   loop(NewSeconds);

  _A ->
   loop(Seconds)

 after 0 ->
  loop(Seconds) 

 end;

%default update 20 seconds (arbitrary chosen)
loop(_Seconds) ->
 loop(20).

execute(Host, Cmd) ->
 Port = open_port({spawn, Cmd}, [ {cd, Host}, exit_status, binary ] ),
 wait(Port, []).

wait(Port, Content) ->
 receive 
  {Port, {data, _BinData}} ->
   wait(Port, Content);

  {Port, {exit_status, _Status}} ->
   ok;

  {Port, eof} ->
   port_close(Port),
   Content;

  {Port, exit} ->
   error_logger:info_msg("Received : ~p~n", [Port]),
   Content
 end.