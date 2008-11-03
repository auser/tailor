-module (tailor_tail).
-export([start/1, start/2, start/3, stop/1, snapshot/1, display/1, init/3]).

start(File) ->
	start(File, fun display/1, "/var/log").

start(File, Callback) ->
	Dir = "/var/log",
	start(File, Callback, Dir).

start(File, Callback, Dir) ->
	spawn_link(?MODULE, init, [File, Callback, Dir]).

snapshot(Pid) ->
	Pid ! {snap, self() },
	receive
		{Port, Callback} ->
			{Port, erlang:fun_info(Callback)};
		_Any ->
			_Any
	end.

stop(Pid) ->
	Pid ! stop.

init(File, Callback, Dir) ->
	Cmd = "/usr/bin/tail -f "++ Dir ++ "/" ++ File,
	Port = open_port({spawn, Cmd}, [ {cd, Dir}, stderr_to_stdout, {line, 256}, exit_status, binary]), 
	tail_loop(Port, Callback).

tail_loop(Port, Callback) ->
	receive
		{Port, {data, {eol, Bin}}} ->
			Callback(Bin),
			tail_loop(Port, Callback);

		{Port, {data, {noeol, Bin}}} ->
			Callback(Bin),
			tail_loop(Port, Callback);

		{Port, {data, Bin}} ->
			Callback(Bin),
			tail_loop(Port, Callback);

		{Port, {exit_status, Status}} ->
			{ok, Status};
	
		{Port, eof} ->
			port_close(Port),
			{ok, eof};

		{snap, Who} ->
			Who ! { Port, Callback},
			tail_loop(Port, Callback);

		stop ->
			port_close(Port),
			{ok, stop};
		
		_Any ->
			tail_loop(Port, Callback) 
	end.

display(Bin) ->
	Content = iolist_to_binary(Bin),
	io:format("[INFO] ~s~n", [Content]).
