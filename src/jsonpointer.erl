-module(jsonpointer).

-export([get/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

get(Json, PointerString) ->
    Pointer = parse_pointer(PointerString),
    traverse(Json, Pointer).

parse_pointer(PointerString) ->
    [_Empty | Pointer] = re:split(PointerString, "/"),
    Pointer.

traverse(Json, []) ->
    Json;
traverse({Key, Value}, [PointerHead | PointerRest]) when PointerHead =:= Key->
    traverse(Value, PointerRest);
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
