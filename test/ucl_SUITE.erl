-module(ucl_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-compile(export_all).

all() -> [start].

start(_Config) ->
    Simple = << "simple: true" >>,
    ?assertEqual({ok,<<"{\n    \"simple\": true\n}">>},
                 ucl:to_json(Simple)),
    %% blob taken from parent libucl project
    {ok, BlobUcl} = file:read_file("../../../../test/fixtures/blob.ucl"),
    BlobJson = file:read_file("../../../../test/fixtures/blob.json"),
    ?assertEqual(BlobJson, ucl:to_json(BlobUcl)),
    ok.
