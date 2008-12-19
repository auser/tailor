% start with
% erlog:start(["/var/log/system.log"]).
-module (erlog).
-export([start/1, start/2, stop/0]).
-export ([loop/2,loop1/5]).
-include_lib("kernel/include/file.hrl").
-define (SECONDS, 5).

start(File) -> start(File, self()).

start(File, From) -> spawn(fun() -> loop(File, From) end ).
stop() -> loop1(0, 0, 0, stop, 0).

loop(File, From) ->
	Mtime = get_new_mtime(File),
	Pos = get_file_pos_for(File),	
	loop1(From, File, Mtime, 0, Pos).

loop1(From, File, LastModTime, LastModTime, FilePos) ->
	case LastModTime of
		stop -> ok;
		_ ->
			timer:sleep(?SECONDS * 1000),
			NewMTime = get_new_mtime(File),
			loop1(From, File, LastModTime, NewMTime, FilePos)
	end;

loop1(From, File, LastModTime, _NewModTime, FilePos) ->
	case read_latest_chunk(File, FilePos) of
		{Data, NewFilePos} ->
			From ! {data, Data},
			NewMTime = get_new_mtime(File),
			loop1(From, File, NewMTime, NewMTime, NewFilePos);
		_ ->
			loop1(From, File, LastModTime, LastModTime, FilePos)
	end.
read_latest_chunk(File, Pos) ->
	Iodevice = case file:open(File, [read]) of
		{ok, Io} -> Io;
		_ ->
			ok
	end,
	file:position(Iodevice, Pos),
	case file:pread(Iodevice, Pos, Pos) of
		{ok, Data} ->
			NewPos = get_file_pos_for(File),
			{Data, NewPos};
		_Msg ->
			error
	end.

get_file_pos_for(File) ->
	case file:read_file_info(File) of
		{ok, #file_info{size=Size}} ->
			Size;
		_ ->
			"Error"
	end.
	
get_new_mtime(File) ->
	case file:read_file_info(File) of
		{ok, #file_info{mtime=Mtime}} ->
			Mtime;
		_ ->
			time:now()
	end.
