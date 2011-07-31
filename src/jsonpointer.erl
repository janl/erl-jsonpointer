-module(jsonpointer).

-export([get/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

get(Json, "/") -> Json;
get(Json, <<"/">>) -> Json;
get(Json, PointerString) ->
    Pointer = parse_pointer(PointerString),
    traverse(Json, Pointer).

parse_pointer(PointerString) ->
    [_Empty | EscapedPointer] = re:split(PointerString, "/"),
    lists:map(fun(Elm) ->
        case re:replace(Elm, "%2f", "/") of
        Elm -> Elm;
        Replaced -> list_to_binary(Replaced)
        end
    end, EscapedPointer).

traverse({Key, Value}, [PointerHead | PointerRest]) when PointerHead =:= Key->
    traverse(Value, PointerRest);
traverse(Json, []) ->
    Json;
traverse(Json, [PointerHead | PointerRest]) when is_list(Json) ->
    ListIndex = list_to_integer(binary_to_list(PointerHead)) + 1,
    if ListIndex > length(Json) ->
        throw({error, "List index > list length"});
    true ->
        SubJson = lists:nth(ListIndex, Json),
        traverse(SubJson, PointerRest)
    end;
traverse({Json}, [PointerHead | PointerRest]) ->
    case proplists:get_value(PointerHead, Json) of
    undefined ->
        throw({error, "Pointer not found"});
    SubJson ->
        traverse(SubJson, PointerRest)
    end.
