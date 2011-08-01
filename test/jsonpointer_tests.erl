-module(jsonpointer_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Json = {[
        {<<"a">>, 1},
        {<<"b">>, {[
            {<<"c">>, 2}
        ]}},
        {<<"d">>, {[
            {<<"e">>, [
                {[{<<"a">>, 3}]},
                {[{<<"b">>, 4}]},
                {[{<<"c">>, 5}]}
            ]}
        ]}},
        {<<"e">>, [3, 4]}
    ]},

    Cases = [
        {"/", Json},
        {"/a", 1},
        {"/b/c", 2},
        {"/d/e/0/a", 3},
        {"/d/e/1/b", 4},
        {"/d/e/2/c", 5}
    ],

    DoAsserts = fun({Path, Val}) ->
        ?assertEqual(jsonpointer:get(Path, Json), Val),
        ?assertEqual(jsonpointer:get(list_to_binary(Path), Json), Val)
    end,

    lists:foreach(DoAsserts, Cases),

    ?assertThrow({error, missing_path}, jsonpointer:get("/f", Json)),
    ?assertThrow({error, invalid_array_index}, jsonpointer:get("/e/3", Json)),
    ok.

complex_keys_test() ->
    Json = {[{<<"a/b">>, 1}]},
    ?assertEqual(1, jsonpointer:get(<<"/a%2fb">>, Json)),
    ok.
