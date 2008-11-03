-module (wsar).

-export([systat/0, stats/0, stats/1, option/1]).
-export([extract/1]).
-define (CMD, "sar ").
-define(OPTIONS, "-A -u -p -n DEV 1 1"). %-u -r -v -c -q -n DEV
-define(DATA, "-f /tmp/last").

systat() ->
	Cmd = ?CMD ++ ?DATA ++ " -- " ++ ?OPTIONS,
	execute(".", Cmd).

stats() ->
	Cmd = ?CMD ++ ?DATA ++ " -- " ++ ?OPTIONS,
	io:format("Called: ~p~n", [Cmd]),
	{ok, _, Bin} = execute(".", Cmd),
	extract(Bin).

stats(List) when is_list(List) ->
 Args = lists:foldl(fun(X, Acc) -> case option(X) of
     error ->
      Acc;
     T ->
      [ $ , T | Acc ]
    end end, [], List),
 Cmd = ?CMD ++ ?DATA ++ " -- " ++ lists:reverse(Args),
 {ok, _, Bin} = execute(".", lists:flatten(Cmd)),
 extract(Bin);

stats(Elem) ->
 stats([Elem]).

option(cpu) ->
 "-u";
option(disk) ->
 "-d";
option(sock) ->
 "-n SOCK";
option(eth0) ->
 "-n DEV";
option(eth1) ->
 "-n DEV";
option(eth2) ->
 "-n DEV";
option(proc) ->
 "-c";
option(run) ->
 "-q";
option(mem) ->
 "-r";
option(inode) ->
 "-v";
option(switch) ->
 "-w";
option(swaping) ->
 "-W";
option(_) ->
 error.

execute(_Host, Cmd) ->
 Port = open_port({spawn, Cmd}, [ exit_status, binary ] ),
 wait(Port, []).

wait(Port, Content) ->
 receive 
  {Port, {data, BinData}} ->
   %error_logger:info_msg("dump:~n~p~n", [BinData]),
   NewContent = [ BinData | Content ],
   wait(Port, NewContent);

  {Port, {exit_status, Status}} ->
   %error_logger:info_msg("exit_code: ~p~n", [Status]),
   {ok, Status, Content};

  {Port, eof} ->
   %error_logger:info_msg("Port closed"),
   port_close(Port),
   {ok, eof, Content};

  {Port, exit} ->
   error_logger:info_msg("Received : ~p~n", [Port]),
   Content
 end.

extract(Bin) ->
 sar_values:extract(iolist_to_binary(Bin)).