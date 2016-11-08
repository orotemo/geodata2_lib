-module(geodata2_lib).

-include("geodata2.hrl").
-define(GREG_EPOCH, 62167219200).

-export([load/1, lookup/2, db_age/1]).

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

db_age({Meta, _Data}) ->
   erlang:system_time(seconds) -
   calendar:datetime_to_gregorian_seconds(Meta#mmdbmeta.timestamp) +
   ?GREG_EPOCH.

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
