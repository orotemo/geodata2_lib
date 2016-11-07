This lib is a partial copy of [this github project](https://github.com/brigadier/geodata2).

This project only includes the necessary files to perform the following:

```erlang
% Data is an opaque tuple. one may load a gzipped file, or an opened one.
% they are differentiated by the extension.
{ok, Data} = geodata2_lib:load(<<"GeoLite2-City.mmdb.gz">>),

% using the opaque tuple, one can call lookup. this version is intended for
% IPv4.
{ok, Result} = geodata2_lib:lookup(Data, <<"123.34.12.32">>),

% now we can ask for specific results using deep map search:
geodata2_utils:map_search([<<"city">>, <<"names">>, <<"en">>], Result).

% it will return the result or undefined atom.
```
