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
                {<<"a">>, 3},
                {<<"b">>, 4},
                {<<"c">>, 5}
            ]}
        ]}}
    ]},

    ?assertEqual(Json, jsonpointer:get(Json, <<"/">>)),
    ?assertEqual(1, jsonpointer:get(Json, <<"/a">>)),
    ?assertEqual(2, jsonpointer:get(Json, <<"/b/c">>)),
    ?assertEqual(3, jsonpointer:get(Json, <<"/d/e/0/a">>)),
    ?assertEqual(4, jsonpointer:get(Json, <<"/d/e/1/b">>)),
    ?assertEqual(5, jsonpointer:get(Json, <<"/d/e/2/c">>)),

    ?assertEqual(Json, jsonpointer:get(Json, "/")),
    ?assertEqual(1, jsonpointer:get(Json, "/a")),
    ?assertEqual(2, jsonpointer:get(Json, "/b/c")),
    ?assertEqual(3, jsonpointer:get(Json, "/d/e/0/a")),
    ?assertEqual(4, jsonpointer:get(Json, "/d/e/1/b")),
    ?assertEqual(5, jsonpointer:get(Json, "/d/e/2/c")),

    ?assertThrow({error, "Pointer not found"}, jsonpointer:get(Json, "/e")),
    ?assertThrow({error, "List index > list length"},
        jsonpointer:get(Json, "/d/e/4/c")),
    ok.

complex_keys_test() ->
    Json = {[
        {<<"a/b">>, 1}
    ]},
    ?assertEqual(1, jsonpointer:get(Json, <<"/a%2fb">>)),
    ok.
