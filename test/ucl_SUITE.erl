-module(ucl_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-compile(export_all).

all() -> [start].

start(_Config) ->

    Empty = ucl:to_json(<< "" >>),
    ?assertEqual({ok,<<"{\n\n}">>}, Empty),

    Simple = ucl:to_json(<< "simple: true" >>),
    ?assertEqual({ok,<<"{\n    \"simple\": true\n}">>}, Simple),

    BadArg = ucl:to_json(error),
    ?assertEqual({error, badarg}, BadArg),

    Invalid = ucl:to_json(<< "invalid" >>),
    ?assertEqual({error, ucl_invalid}, Invalid),

    %% blob taken from parent libucl project
    {ok, BlobUcl} = file:read_file("../../../../test/fixtures/blob.ucl"),
    BlobJson = file:read_file("../../../../test/fixtures/blob.json"),
    ?assertEqual(BlobJson, ucl:to_json(BlobUcl)),
    ok.
