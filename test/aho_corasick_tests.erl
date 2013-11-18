%%% File    : aho_corasick.erl
%%% Author  : Fyodor Ustinov <ufm@ufm.su>
%%% Descrip.: Multiple search based on Aho-Corasick string matching algorithm
%%%
%%% License : GPL
%%% modified by josemic
-module(aho_corasick_tests).

-include_lib("eunit/include/eunit.hrl").

aho_corasick_test_() ->
    [
     tree_1(),
     tree_2(),
     match_list_1(),
     match_list_2(),
     match_list_3(),
     match_binary_1(),
     match_binary_2(),
     match_binary_3()
    ].

tree_1() ->
    ?_assertEqual(aho_corasick:tree(["1"]), [{[],[],[]},{"1",["1"],[]}]).

tree_2() ->
    T = aho_corasick:tree(["1","2"]),
    ?_assertEqual(T, [{[],[],[]},{"1",["1"],[]},{"2",["2"],[]}]).

match_list_1() ->
    R = aho_corasick:match(["1","2"],"123456789012345678901234567890"),
    ?_assertEqual(R, [{"2",[2,12,22]},{"1",[1,11,21]}]).

match_list_2() ->
    T = aho_corasick:tree(["1","2"]),
    R = aho_corasick:match(["1","2"],"123456789012345678901234567890"),
    ?_assertEqual(aho_corasick:match_tree(T,"123456789012345678901234567890"), R).

match_list_3() ->
    ?_assertEqual(aho_corasick:match(["1","12","2","23","901"],
				     "123456789012345678901234567890"), 
		  [
		   {"23",[2,12,22]},
		   {"2",[2,12,22]},
		   {"901",[9,19]},
		   {"1",[1,11,21]},
		   {"12",[1,11,21]}]
		 ).

match_binary_1() ->
    R = aho_corasick:match(["1","2"],<<"123456789012345678901234567890">>),
    ?_assertEqual(R, [{"2",[2,12,22]},{"1",[1,11,21]}]).

match_binary_2() ->
    T = aho_corasick:tree(["1","2"]),
    R = aho_corasick:match(["1","2"],
			   <<"123456789012345678901234567890">>),
    ?_assertEqual(aho_corasick:match_tree(T,"123456789012345678901234567890"), R).

match_binary_3() ->
    ?_assertEqual(aho_corasick:match(["1","12","2","23","901"],
				     <<"123456789012345678901234567890">>),
		  [
		   {"23",[2,12,22]},
		   {"2",[2,12,22]},
		   {"901",[9,19]},
		   {"1",[1,11,21]},
		   {"12",[1,11,21]}]
		 ).
