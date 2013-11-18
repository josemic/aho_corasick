%%% File    : ahocorasik.erl
%%% Author  : Fyodor Ustinov <ufm@ufm.su>
%%% Descrip.: Multiple search based on Aho-Corasick string matching algorithm
%%%
%%% License : GPL
%%%
%%% Usage:
%%%     ahocorasik:match(["pat1", "pat2"], "pat1 pat2 pat1")
%%%     or
%%%	    Tree = ahocorasik:tree(["pat1", "pat2"]) 
%%%	    ahocorasik:match_tree(Tree, "pat1 pat2 pat1")
%%%
%%%	    functions 'match' and 'match_tree' returns a list of matching patterns 
%%%	    and their position in the text:
%%%	    [{"pat1",[1,11]},{"pat2",[6]}]
%%%

-module(ahocorasik).
%%-define(TEST,true).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([match/2, match_tree/2, tree/1]).

-define(ROOT, {[], [], []}).

-type apex() :: {list(), list(), list()}.

-record(s,{
	  apex = ?ROOT         :: apex(),
	  tree = []            :: list(apex()),
	  counter = dict:new() :: dict(),
	  pos = 1              :: integer()
	 }
).

tree(Pat) ->
    build_fail(build_tree(Pat, []), 1)
.

build_tree([], Tree)    -> [?ROOT|lists:reverse(Tree)];
build_tree([H|T], Tree) ->
    build_tree(T, parse_word(H, Tree))
.

parse_word(Word, Tree) ->
    parse_word(Word, Tree, [])
.

parse_word([], Tree, Apex) when length(Apex) =:= 0 -> Tree;
parse_word([], Tree, Apex) ->
    Out = lists:reverse(Apex),
    add_out(Apex, Out, Tree)
;

parse_word([H|T], Tree, Apex) ->
    CApex = [H|Apex],
    case lists:keymember(CApex, 1, Tree) of
	true ->
	    parse_word(T, Tree, CApex);
	false ->
	    parse_word(T, add_apex(CApex, Tree), CApex)
    end
.

add_apex(CApex, Tree) ->
    [{CApex, [], []}| Tree]
.

add_out(Apex, Out, Tree) ->
    {Apex, COut, Fail} = lists:keyfind(Apex, 1, Tree),
    case lists:member(Out, COut) of
	false ->
	    lists:keyreplace(Apex, 1, Tree, {Apex, lists:usort([Out | COut]), Fail});
	true ->
	    Tree
    end
.

build_fail(Tree, Level) ->
    Apexes = [X || {X, _, _} <- Tree, length(X) =:= Level],
    case Apexes of
	[] ->
	    Tree;
	_ ->
	    build_fail(fail_apexes(Apexes, Tree), Level + 1)
    end
.

fail_apexes([], Tree)            -> Tree;
fail_apexes([Apex|Apexes], Tree) ->
    fail_apexes(Apexes, fail_one(Apex, lists:reverse(Apex), Tree))
.

fail_one([_Apex|[]], _, Tree)    -> Tree;
fail_one(_, [], Tree)            -> Tree;
fail_one(Apex, [_|SApex], Tree)  ->
    NApex = lists:reverse(SApex),
    case lists:keymember(NApex, 1, Tree) of
	true ->
	    add_fail(Apex, NApex, Tree);
	false ->
	    fail_one(Apex, SApex, Tree)
    end
.

add_fail(Apex, NApex, Tree) ->
    {Apex, Out, _} = lists:keyfind(Apex, 1, Tree),
    {NApex, COut, _} = lists:keyfind(NApex, 1, Tree),
    lists:keyreplace(Apex, 1, Tree, {Apex, lists:umerge(COut, Out), NApex})
.
	

match_tree(Tree, Text) -> do_match(#s{tree = Tree}, Text).
match(Pat, Text)       -> do_match(#s{tree = tree(Pat)}, Text).

do_match(S, [])    -> dict:to_list(S#s.counter);
do_match(S, [H|T]) -> do_match((do_apex(S, H))#s{pos = S#s.pos + 1}, T);

do_match(S, <<>>)    -> dict:to_list(S#s.counter);
do_match(S, <<H:8, T/binary>>) -> do_match((do_apex(S, H))#s{pos = S#s.pos + 1}, T).

do_apex(S, H) ->
    Apex = [H | element(1, S#s.apex)],
    case lists:keyfind(Apex, 1, S#s.tree) of
	{Apex, Out, _Fail} = NApex ->
	    (inc_out(S, Out))#s{apex = NApex};
	false ->
	    case length(Apex) of
		1 ->
		    S#s{apex = ?ROOT};
		_ ->
		    do_apex(S#s{apex = lists:keyfind(element(3, S#s.apex), 1, S#s.tree)}, H)
	    end
    end
.

inc_out(S, [])    -> S;
inc_out(S, [H|T]) ->
    inc_out(S#s{counter = dict:append(H, S#s.pos - length(H) + 1, S#s.counter)}, T)
.

%%%%%%%%%%%%%%%%%%%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
tree_test() ->
    ?assertEqual(tree(["1"]), [{[],[],[]},{"1",["1"],[]}]),
    T = tree(["1","2"]),
    ?assertEqual(T, [{[],[],[]},{"1",["1"],[]},{"2",["2"],[]}]),
    R = match(["1","2"],"123456789012345678901234567890"),
    ?assertEqual(R, [{"2",[2,12,22]},{"1",[1,11,21]}]),
    ?assertEqual(match_tree(T,"123456789012345678901234567890"), R),
    ?assertEqual(match(["1","12","2","23","901"],"123456789012345678901234567890"), [
        {"23",[2,12,22]},
        {"2",[2,12,22]},
        {"901",[9,19]},
        {"1",[1,11,21]},
        {"12",[1,11,21]}]
    ),
    R2 = match(["1","2"],<<"123456789012345678901234567890">>),
    ?assertEqual(R2, [{"2",[2,12,22]},{"1",[1,11,21]}]),
    ?assertEqual(match_tree(T,<<"123456789012345678901234567890">>), R2),
    ?assertEqual(match(["1","12","2","23","901"],<<"123456789012345678901234567890">>), [
        {"23",[2,12,22]},
        {"2",[2,12,22]},
        {"901",[9,19]},
        {"1",[1,11,21]},
        {"12",[1,11,21]}]
    )
.
-endif.
