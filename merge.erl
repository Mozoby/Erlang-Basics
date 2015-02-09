-module(merge).
-export([sort/1]).

sort(List) ->
    sort(List, length(List)).


combine(ascending, [Head1 | Tail1], [Head2|Tail2]) when Head1 < Head2 ->
    [Head1 | combine(ascending, Tail1, [Head2 | Tail2])];

combine(ascending, [Head1 | Tail1], [Head2|Tail2]) when Head1 >= Head2 ->
    [Head2 | combine(ascending, [Head1 | Tail1], Tail2)];

combine(ascending, [Head1 | Tail1], []) ->
    [Head1 | Tail1];

combine(ascending, [], [Head2 | Tail2]) ->
    [Head2 | Tail2].

sort(List, 0) ->
    [];
sort(List, 1) ->
    List;
sort(List, ListLength) ->
    L1 = ListLength div 2,
    L2 = ListLength - L1,
    ToSort = lists:split(L1, List),
    S1 = sort(element(1,ToSort), L1),
    S2 = sort(element(2,ToSort), L2),
    combine(ascending, S1, S2).


