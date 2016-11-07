-module(geodata2_utils).

-export([map_search/2]).

map_search(_, undefined) -> undefined;
map_search([], M) -> M;
map_search([E | Rest], M) when is_map(M) ->
  Val = maps:get(E, M, undefined),
  map_search(Rest, Val).
