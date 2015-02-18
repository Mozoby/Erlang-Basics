-module(setv1).
-export([run/0]).

run() ->
 live_analysis(get_test_data()).

get_test_data() ->
	[	
		{[], [a,b], [2]}, 
		{[a,b], [c,d], [3,5]},
		{[b,d], [], [4,5]},
		{[a,b,e], [d], [3]},
		{[a,b,c], [e], [2,6]},
		{[b,d], [a], []}
	].

% Each Block B is a tuple of form:
% 	{ use, def, Successor }
%		use: Set of symbols
%		def: Set of Symbols
% 		Successor: List of integers (refers to index in B)
live_analysis(Blocks) ->
	InSets  = [empty_set() || X <- Blocks],
	OutSets = [empty_set() || X <- Blocks],

	perform_loop(Blocks, InSets, OutSets, true).

perform_loop(_, _, _, Change) when Change == false ->
	ok;

perform_loop(Blocks, InSets, OutSets, Change) when Change == true ->
	%Print Initial
	io:format("--Iteration Start--~nOut: ~p~nIn: ~p~n", [InSets, OutSets]),
	{NewChange, NewIn, NewOut} = iterate_blocks(Blocks, Blocks, 1, InSets, [], [], false),
	%print stuff here?
	io:format("--Iteration End--~nOut: ~p~nIn: ~p~n----------------~n", [NewIn, NewOut]),
	perform_loop(Blocks, NewIn, NewOut, NewChange).


iterate_blocks(Blocks, [B | BTail], BIndex, InSets, AccOutSets, InSetsAcc, Change) ->
	Old = lists:nth(BIndex, InSets),

	NewOut = get_out_set(B, Blocks, element(3, B), InSets, []),
	NewIn = union_set(element(1, B), set_subtract(NewOut, element(2, B))),

	DidChange = set_equal(NewIn, lists:nth(BIndex, InSets)),
	if
		Change == true ->
			NewChange = true;
		DidChange == true ->
			NewChange = true;
		true ->
			NewChange = false
	end,

	iterate_blocks(Blocks, BTail, BIndex + 1, InSets, [AccOutSets | NewOut], [InSetsAcc | NewIn], NewChange);

iterate_blocks(Blocks, [], BIndex, InSets, AccOutSets, InSetsAcc, Change) ->
	{Change, InSetsAcc, AccOutSets}.

get_out_set(B, Blocks, [Successor | More],InSets, OutSetAcc) ->
	get_out_set(B, Blocks, More, InSets, 
		union_set(OutSetAcc, lists:nth(Successor, InSets)));

get_out_set(B, Blocks, [], InSets, OutSetAcc) ->
	OutSetAcc.

empty_set() ->
 [].

in_set([SetHead | _], Element) when SetHead == Element ->
	true;

in_set([SetHead | SetTail], Element) when SetHead /= Element ->
	in_set(SetTail, Element);

in_set([], Element) ->
	false.

set_equal(Set1, Set2) when length(Set1) /= length(Set2) ->
	false;

set_equal(Set1, Set2) ->
	equal_helper(Set1, Set2).

equal_helper([Head1 | Tail1], Set2)  ->
	IsInSet = in_set(Set2, Head1),

	if  
		IsInSet == true ->
			equal_helper(Tail1, Set2);
		true ->
			false
	end;

equal_helper([], Set2)  ->
	true.

union_set(Set1, Set2) ->
	union_set(Set1, Set2, []).

union_set([Head1 | Tail1], Set2, Acc)  ->
	IsInSet = in_set(Set2, Head1),

	if  
		IsInSet == true ->
			union_set(Tail1, Set2, Acc);
		true ->
			union_set(Tail1, Set2, [Head1 | Acc] )
	end;

union_set([], [Head2| Tail2], Acc)  ->
	union_set([], Tail2, [Head2 | Acc]);

union_set([], [], Acc) -> 
	Acc.

%Set1 - Set2
set_subtract(Set1, Set2) ->
	set_subtract(Set1, Set2, []).
	
set_subtract([Head1| Tail1], Set2, Acc) ->
	IsInSet = in_set(Set2, Head1),
	if
		IsInSet ->
			set_subtract(Tail1, Set2, Acc);
		true ->
			set_subtract(Tail1, Set2, [Head1 | Acc])
	end;

set_subtract([], Set2, Acc) ->
	Acc.



	
	
