-module('Elixir.UCL').
-export([to_json/1,
         'to_json!'/1]).

to_json(Bin) -> ucl:to_json(Bin).

'to_json!'(Bin) ->
 {ok, Json} = ucl:to_json(Bin),
 Json.
