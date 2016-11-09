-module(geodata2_utils).

-export([ map_search/2,
          country_name/1,
          country_name/2,
          city_name/1,
          city_name/2,
          country_code/1]).

map_search(_, undefined) -> undefined;
map_search([], M) -> M;
map_search([E | Rest], M) when is_map(M) ->
  Val = maps:get(E, M, undefined),
  map_search(Rest, Val).

country_name(LookupResult) -> country_name(LookupResult, <<"en">>).
country_name(LookupResult, Language) ->
  map_search([<<"country">>, <<"names">>, Language], LookupResult).

city_name(LookupResult) -> city_name(LookupResult, <<"en">>).
city_name(LookupResult, Language) ->
  map_search([<<"city">>, <<"names">>, Language], LookupResult).

country_code(LookupResult) ->
  map_search([<<"country">>, <<"iso_code">>], LookupResult).
