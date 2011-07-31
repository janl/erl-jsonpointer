# JSON Pointer for Erlang

This is an implementation of [JSON Pointer](http://tools.ietf.org/html/draft-pbryan-zyp-json-pointer-00).

## Usage

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

    One =   jsonpointer:get(Json, <<"/a">>)),
    Two =   jsonpointer:get(Json, <<"/b/c">>)),
    Three = jsonpointer:get(Json, <<"/d/e/0/a">>)),
    Four =  jsonpointer:get(Json, <<"/d/e/1/b">>)),
    Five =  jsonpointer:get(Json, <<"/d/e/2/c">>)),

## Building

    $ rebar compile

## Testing

    $ rebar eunit

## License

MIT License.
