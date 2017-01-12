#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa 'ebin/'
%%
% Strictly Locally Reducible Languages 
% basic examples
% MPradella, MMXVI


-import(locred,
        [ bordered_factors_of/2
          ,reduction_star/2
          ,check_system/1
          ,show_automaton/1
          ,transitions/1
          ,automaton_states/1
          ,format_factors/1
          ,ex_to_sys/2
        ]).

%%%% Examples

anbn() ->
    %%% Automaton example (a^n b^n)
    
    Anbn = bordered_factors_of("a[a.b]b",3),
    format_factors(Anbn),
    San = {system, Anbn, 3},
    check_system(San),
    reduction_star("aaaabbbb", San),
    show_automaton(transitions(automaton_states(San))).

notOP() ->
    %% a^n b^n U c^n (ba)^n
    P1 = bordered_factors_of("a[a[a.b]b]b",5),
    P2 = sets:union(P1, bordered_factors_of("a.b",5)),
    P3 = sets:union(P2, bordered_factors_of("c[c[c.b.a]b.a]b.a",5)),
    P4 = sets:union(P3, bordered_factors_of("c.b.a",5)),
    Sys = {system, P4, 5},
    check_system(Sys),
    reduction_star("ccccbabababa", Sys),
    reduction_star("aaabbb", Sys),
    show_automaton(transitions(automaton_states(Sys))).

l1() ->
    P1 = bordered_factors_of("a[a[a.b]b]b",3),
    P2 = sets:union(P1, bordered_factors_of("a.b",3)),
    P3 = sets:union(P2, bordered_factors_of("a",3)),
    Sys = {system, P3, 3},
    check_system(Sys),
    format_factors(P3),
    show_automaton(transitions(automaton_states(Sys))).


l2() ->
    P1 = bordered_factors_of("a[a[a.c.b.b]c.b.b]c.b.b",3),
    P2 = sets:union(P1, bordered_factors_of("a.c.b.b",3)),
    Sys = {system, P2, 3},
    check_system(Sys),
    format_factors(P2),
    reduction_star("aaacbcbbbcbb", Sys),
    show_automaton(transitions(automaton_states(Sys))).

l3() ->
    P1 = bordered_factors_of("a[a.c.b.b]c.b.b",3),
    P2 = sets:union(P1, bordered_factors_of("a[a.c]c",3)),
    P3 = sets:union(P2, bordered_factors_of("a.c.b.b",3)),
    P4 = sets:union(P3, bordered_factors_of("a.c",3)),
    P5 = sets:union(P4, bordered_factors_of("a[a.c]c.b.b",3)),
    Sys = {system, P5, 3},
    check_system(Sys),
    format_factors(P5),
    reduction_star("aaacbcbbbcbb", Sys),
    show_automaton(transitions(automaton_states(Sys))).

l4() -> % forse questo; L = a^n (c b+)^n
    P1 = bordered_factors_of("a[a.c.b[b]c.b]c.b[b",3),
    P2 = sets:union(P1, bordered_factors_of("a[a.c.b]c.b",3)),
    P21 = sets:union(P2, bordered_factors_of("a[a.c.b[b]c.b",3)),
    P3 = sets:union(P21, bordered_factors_of("a.c.b",3)),
    P4 = sets:union(P3, bordered_factors_of("a.c.b",3)),
    P5 = sets:union(P4, bordered_factors_of("a[a.c.b]c.b[b",3)),
    Sys = {system, P5, 3},
    check_system(Sys),
    format_factors(P5),
    reduction_star("aaacbbbcbcbb", Sys),
    show_automaton(transitions(automaton_states(Sys))).

l4bis() -> % L = a^n (c b+)^n
    P1 = bordered_factors_of("a[a.c.b.b]c.b]c.b.b",3),
    P2 = sets:union(P1, bordered_factors_of("a[a.c.b]c.b",3)),
    P21 = sets:union(P2, bordered_factors_of("a[a.c.b.b]c.b",3)),
    P3 = sets:union(P21, bordered_factors_of("a.c.b",3)),
    P4 = sets:union(P3, bordered_factors_of("a.c.b",3)),
    P5 = sets:union(P4, bordered_factors_of("a[a.c.b]c.b.b",3)),
    Sys = {system, P5, 3},
    check_system(Sys),
    format_factors(P5),
    reduction_star("aaacbbbbcbcbbb", Sys),
    reduction_star("aacbb", Sys),
    show_automaton(transitions(automaton_states(Sys))).

l5() ->
    P1 = bordered_factors_of("a[a.b.c]b.c",5),
    P2 = sets:union(P1, bordered_factors_of("a.b.c",5)),
    P3 = sets:union(P2, bordered_factors_of("b[b.c.a]c.a",5)),
    P4 = sets:union(P3, bordered_factors_of("b.c.a",5)),
    P5 = sets:union(P4, bordered_factors_of("c[c.a.b]a.b",5)),
    P6 = sets:union(P5, bordered_factors_of("c.a.b",5)),
    Sys = {system, P6, 5},
    check_system(Sys),
    format_factors(P6),
    reduction_star("bcabcabca", Sys),
    show_automaton(transitions(automaton_states(Sys))).


hierarchy() ->
    P1 = bordered_factors_of("a.a.a.b]a.a.a.b",5),
    P2 = sets:union(P1, bordered_factors_of("a.a.a.b",5)),
    Sys = {system, P2, 5},
    check_system(Sys),
    format_factors(P2),
    reduction_star("aaabaaabaaab", Sys),
    reduction_star("aaabaaaaaaabaaab", Sys), % this should fail for k > 5
    show_automaton(transitions(automaton_states(Sys))).

hierarchy2() ->
    P1 = bordered_factors_of("a.a.b]a.a.b",5),
    P2 = sets:union(P1, bordered_factors_of("a.a.b",5)),
    Sys = {system, P2, 5},
    check_system(Sys),
    format_factors(P2),
    reduction_star("aabaabaab", Sys),
    reduction_star("aaabaabaaab", Sys), % this should fail 
    show_automaton(transitions(automaton_states(Sys))).

% new interface!
dyck() ->
    San = ex_to_sys([
                     "a[a.A]A",
                     "a.A",
                     "a.A[a.A",
                     "b[b.B]B",
                     "b.B",
                     "b.B[b.B",
                     "a.A[b.B",
                     "b.B[a.A",
                     "b[a.A]B",
                     "a[b.B]A"
                    ], 3),
    reduction_star("aabBAAaAbbBBbaaaAAAB", San),
    reduction_star("aaAAaA", San),
    show_automaton(transitions(automaton_states(San))).



% used as a script:
% escript locred.erl
main(_V) -> 
    dyck().

