-module(geodata2_lib).

-export([load/1, lookup/2]).

load(Path) ->
  case read(Path) of
    {ok, Data} ->
      {ok, Meta} = geodata2_format:meta(Data),
      {ok, {Meta, Data}};
    Else -> Else
  end.

lookup(OpaqueData, IP) when is_binary(IP) ->
  Parts = binary:split(IP, <<".">>, [ global ]),
  Numeric = [ list_to_integer(binary_to_list(Part)) || Part <- Parts ],
  lookup(OpaqueData, Numeric);

lookup({Meta, Data}, [W1, W2, W3, W4]) ->
  {ok, Bits, Version} = geodata2_ip:make_ip({W1, W2, W3, W4}),

  case geodata2_format:lookup(Meta, Data, Bits, Version) of
    {ok, Res} -> {ok, mapify(Res)};
    Else -> Else
  end.


%% internal

read(Path) ->
  case file:read_file(Path) of
    {ok, Data} ->
      case filename:extension(Path) of
        ".gz" -> {ok, zlib:gunzip(Data)};
        <<".gz">> -> {ok, zlib:gunzip(Data)};
        _ -> {ok, Data}
      end;
      Else -> Else
  end.

mapify(El) when is_list(El) ->
  Converted = [ {K, mapify(V)} || {K,V} <- El ],
  maps:from_list(Converted);

mapify(El) -> El.
