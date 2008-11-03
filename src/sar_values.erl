-module(sar_values).

-export([extract/1, extractor/1, sort/1]).
-export([parse/1, parse_value/2]).

extract(Bin) ->
 extract(Bin, []).

extract(Bin, Stats) ->
 case parse(Bin) of
  {Class, Type, Rest} ->
   %io:format("~p.~p", [Class, Type]),
   case  parse_value(Rest, <<>>) of
    {more, Value, More} ->
     NewStats = [ {Class, Type, Value} | Stats ],
     extract(More, NewStats);

    {eof, Value} ->
     NewStats = [ {Class, Type, Value} | Stats ],
     NewStats 
   end;

  eof -> 
   Stats 
 end.

parse(<<"%user", Rest/binary >>) -> {cpu, user, Rest};
parse(<<"%nice", Rest/binary>>) -> {cpu, nice, Rest};
parse(<<"%system", Rest/binary>>) -> {cpu, system, Rest};
parse(<<"%iowait", Rest/binary>>) -> {cpu, iowait, Rest};
parse(<<"%steal", Rest/binary>>) ->  {cpu, steal, Rest};
parse(<<"%idle", Rest/binary>>) -> {cpu, idle, Rest};

parse(<<"kbmemfree", Rest/binary>>) -> {mem, kbmemfree, Rest};
parse(<<"kbmemused", Rest/binary>>) -> {mem, kbmemused, Rest};
parse(<<"%memused", Rest/binary>>) -> {mem, memused, Rest};
parse(<<"kbbuffers", Rest/binary>>) -> {mem, kbbuffers, Rest};
parse(<<"kbcached", Rest/binary>>) -> {mem, kbcached, Rest};

parse(<<"kbswpfree", Rest/binary>>) -> {swap, free, Rest};
parse(<<"kbswpused", Rest/binary>>) -> {swap, used, Rest};
parse(<<"%swpused", Rest/binary>>) -> {swap, usage, Rest};
parse(<<"kbswpcad", Rest/binary>>) -> {swap, swpcad, Rest};

parse(<<"dentunusd", Rest/binary>>) -> {inode, dentryunused, Rest};
parse(<<"file-sz", Rest/binary>>) -> {inode, fileopened, Rest};
parse(<<"inode-sz", Rest/binary>>) -> {inode, inodes, Rest};
parse(<<"super-sz", Rest/binary>>) -> {inode, super, Rest};
parse(<<"%super-sz", Rest/binary>>) -> {inode, superusage, Rest};
parse(<<"dquot-sz", Rest/binary>>) -> {inode, dquotsz, Rest};
parse(<<"%dquot-sz", Rest/binary>>) -> {inode, dquotszusage, Rest};
parse(<<"rtsig-sz", Rest/binary>>) -> {rtsig, count , Rest};
parse(<<"%rtsig-sz", Rest/binary>>) -> {rtsig, usage, Rest};

parse(<<"totsck", Rest/binary>>) -> {sock, total, Rest};
parse(<<"tcpsck", Rest/binary>>) -> {sock, tcp, Rest};
parse(<<"udpsck", Rest/binary>>) -> {sock, udp, Rest};
parse(<<"rawsck", Rest/binary>>) -> {sock, raw, Rest};
parse(<<"ip-frag", Rest/binary>>) -> {sock, ipfrag, Rest};

parse(<<"runq-sz", Rest/binary>>) -> {procs, running, Rest};
parse(<<"plist-sz", Rest/binary>>) -> {procs, total, Rest};

parse(<<"ldavg-15", Rest/binary>>) -> {load, min15, Rest};
parse(<<"ldavg-1", Rest/binary>>) -> {load, min1, Rest};
parse(<<"ldavg-5", Rest/binary>>) -> {load, min5, Rest};

parse(<<"pswpin/s", Rest/binary>>) -> {swaping, pswpin, Rest};
parse(<<"pswpout/s", Rest/binary>>) -> {swaping, pswpout, Rest};

parse(<<"l0", Rest/binary>>)   -> parsebis(Rest, l0);
parse(<<"eth0", Rest/binary>>) -> parsebis(Rest, eth0);
parse(<<"eth1", Rest/binary>>) -> parsebis(Rest, eth1);
parse(<<"eth2", Rest/binary>>) -> parsebis(Rest, eth2);

parse(<<>>) -> eof;
 
parse(Bin) ->
 {_, Next} = split_binary(Bin, 1), 
 parse(Next).

parsebis(<<"rxpck/s", Rest/binary>>,  Category) -> {Category, rxpck, Rest};
parsebis(<<"txpck/s", Rest/binary>>,  Category) -> {Category, txpck, Rest};
parsebis(<<"rxbyt/s", Rest/binary>>,  Category) -> {Category, rxbyt, Rest};
parsebis(<<"txbyt/s", Rest/binary>>,  Category) -> {Category, txbyt, Rest};
parsebis(<<"rxcmp/s", Rest/binary>>,  Category) -> {Category, rxcmp, Rest};
parsebis(<<"txcmp/s", Rest/binary>>,  Category) -> {Category, txcmp, Rest};
parsebis(<<"rxmcst/s", Rest/binary>>, Category) -> {Category, rxmcst, Rest};
parsebis(Bin, Category) -> 
 {_, Next} = split_binary(Bin, 1), 
 parsebis(Next, Category).

parse_value(<<$\t, Rest/binary>>, _Value) ->
 parse_value(Rest, _Value);
parse_value(<<$ , Rest/binary>>, _Value) ->
 parse_value(Rest, _Value);

parse_value(<<$\n, _Rest/binary>>, Value) ->
 {more, Value, _Rest};

parse_value(<<>>, Value) ->
 {eof, Value};

parse_value(Bin, Value) ->
 {H, Next} = split_binary(Bin, 1),
 parse_value(Next, iolist_to_binary([Value, H])).

extractor(Motif) ->
 fun(L) when is_list(L) ->
  [ {Y, Z} || {X, Y, Z} <- L, X == Motif]
 end.

sort(List) ->
 lists:sort( fun({X, _V}, {Y, _W}) when X < Y -> 
   true; 
   (_A, _B) -> false 
  end, List).