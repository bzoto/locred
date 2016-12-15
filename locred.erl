% Strictly Locally Reducible Languages
% MPradella, MMXVI

-module(locred).

-author('Matteo Pradella').
-date(20161107).

-export([factors_of/2
         ,bordered_factors_of/2
         ,check_factors/2
         ,check_string/1
         ,reduction/2
         ,reduction_star/2
         ,add_borders/2
         ,check_system/1
         ,automaton_states/1
         ,transitions/1
         ,show_automaton/1
        ]).

-export([format_factors/1]).

-export([main/1]).

%% Basic Utils

%% this is used to check if a string contains the correct precedences
check_string(String) ->
    {Res, _} = lists:foldl(fun check_string_help/2, {true, odd}, String),
    Res.

check_string_help(Char,  {true, even}) -> {not(lists:member(Char, "[.]")), odd};
check_string_help(Char,  {true, odd})  -> {lists:member(Char, "[.]"), even};
check_string_help(_Char, {false, _V})  -> {false, _V}.


borders(1) -> "#";
borders(Size) -> lists:append("#.",borders(Size-2)).

add_borders(String, Size) -> lists:append([borders(Size),
                                           "[", String, "]",
                                           borders(Size)]).
format_factors(Set) ->
    io:format("{~n"),
    sets:fold(fun(X,_) ->
                      io:format("~s~n",[X])
              end,"",Set),
    io:format("}~n").


%% Main stuff

% factors_of returns all the factors of size Size of String,
% which must contain precedences
factors_of(String, Size) -> factors_help(String, 1, Size, sets:new()).
bordered_factors_of(String, Size) -> factors_of(add_borders(String, Size), Size).

factors_help(String, From, Size, Set) ->
    C = string:substr(String, From, Size),
    if
        length(C) < Size -> Set;
        true -> factors_help(String, From+2, Size, sets:add_element(C, Set))
    end.



% A System has form {system, Set, Size}
check_factors(String, System) ->
    {system, Factors, Size} = System,
    SFact = factors_of(String, Size),
    sets:is_subset(SFact, Factors).

% Check if a System is conflictual
conflictual(Str1, Str2) ->
    F = fun(X) -> not(lists:member(X, "[.]")) end,
    Str1 =/= Str2 andalso lists:filter(F,Str1) =:= lists:filter(F, Str2).

check_system(System) ->
    {system, Factors, _} = System,
    Confl = sets:fold(fun(X,B) ->
                              sets:fold(fun(Y,B1) ->
                                                case conflictual(X,Y) of
                                                    true  -> [{conflict, X, Y} | B1];
                                                    false -> B1
                                                end
                                        end, B, Factors)
                      end, [], Factors),
    %% Note: Factors will contain both {conflict, A, B} and {conflict, B, A}
    io:format("Conflicts: ~p~n", [Confl]),
    Confl.

%%% simple, less efficient variant based on lists
%check_system(System) ->
%    {system, Factors, _} = System,
%    L = sets:to_list(Factors),
%    Confl = [ {conflict, E1, E2} || E1 <- L, E2 <- L, conflictual(E1, E2) ],
%    io:format("Conflicts: ~p~n", [Confl]),
%    Confl.

% Look-back/Look-ahead automaton construction
automaton_states(System) ->
    {system, Factors, Size} = System,
    States = sets:fold(fun(X,B) ->
                               sets:fold(fun(Y,B1) ->
                                                 % this is to avoid spurious beginnings
                                                 Flag = hd(Y) =/= hd("#") andalso lists:last(X) =/= hd("#"), 
                                                 V = lists:append(X,tl(Y)),
                                                 case check_factors(V, System) andalso Flag of
                                                     true ->  R1 = [tl(V)];
                                                     false -> R1 = []
                                                 end,
                                                 V1 = lists:append(lists:droplast(X),Y),
                                                 case check_factors(V1, System) andalso Flag of
                                                     true ->  R2 = [lists:droplast(V1) | R1];
                                                     false -> R2 = R1
                                                 end,
                                                 sets:union(sets:from_list(R2),B1)
                                         end, B, Factors)
                       end, sets:new(), Factors),
    %% This manages "ad hoc" starting and ending states
    Start = sets:filter(fun(X) -> hd(X) =:= hd("#") end, States),
    Start1 = sets:fold(fun(X,B) -> 
                               sets:add_element(string:concat(".",string:substr(X,1,Size)),B)
                       end, sets:new(), Start),
    End = sets:filter(fun(X) -> lists:last(X) =:= hd("#") end, States),
    End1 = sets:fold(fun(X,B) -> 
                             sets:add_element(string:concat(tl(X),"."),B)
                     end, sets:new(), End),
    sets:union(States,sets:union(Start1,End1)).


is_transition(State1, State2) ->
    tl(State1) =:= lists:droplast(State2).

transitions(States) ->
    sets:fold(fun(X,B) ->
                      sets:fold(fun(Y,B1) ->
                                        case is_transition(X,Y) of
                                            true  -> O = sets:add_element({X,Y}, B1);
                                            false -> O = B1
                                        end,
                                        O
                                end, B, States)
              end, sets:new(), States).

show_automaton(Transitions) ->
    {ok, F} = file:open("automa.dot", write),
    io:fwrite(F,"digraph finite_state_machine {~n",[]),
    io:fwrite(F,"rankdir = LR~n",[]),
    sets:fold(fun(X,B) -> 
                      {From, To} = X,
                      L = string:len(From) div 2,
                      Lab = string:substr(From,L+1,1),
                      io:fwrite(F,"  ~p -> ~p  [ label = ~p] ~n", [From, To, Lab])
              end, [], Transitions),
   io:fwrite(F,"}~n",[]),
   file:close(F),
   os:cmd("dot automa.dot -Tpdf > automa.pdf"). %% uses Graphviz




% Reduction and Reduction*
reduction(String, System) ->
    {ok, Handle} = re:compile("\\[[^\\[\\]]+\\]"),
    case re:split(String, Handle, [{parts, 2},{return, list}]) of
        [L,R] ->
            HP = [lists:append([L,"[",R]),
                  lists:append([L,".",R]),
                  lists:append([L,"]",R])],
            case lists:map(fun(X) -> check_factors(X, System) end, HP) of
                [true, false, false] -> [V, _, _] = HP, {ok, V};
                [false, true, false] -> [_, V, _] = HP, {ok, V};
                [false, false, true] -> [_, _, V] = HP, {ok, V};
                _ -> {no, HP}
            end;
        _ -> % No more reductions available
            {stop, String}
    end.

reduction_star_h(String, System) ->
    case reduction(String, System) of
        {ok, NewString} ->
            io:format("Step: ~s ~n", [NewString]),
            reduction_star_h(NewString, System);
        {stop, V} ->
            io:format("---> Stop at ~s <---~n", [V]), V;
        {no, V} ->
            io:format("Problem with ~s ~nHypotheses:~n ~s ~n ~s ~n ~s ~n", [String|V]),
            {no, String, V}
    end.

% Entry point for reduction*: add borders and checks if the staring factors are ok
reduction_star(String, System) ->
    {system, _, Size} = System,
    Str = add_borders(String, Size),
    case check_factors(Str, System) of
        true  -> reduction_star_h(Str, System);
        false -> io:format("reduction: bad factors~n"), {no, Str}
    end.

% used as a script:
% escript locred.erl
main(_V) ->
    S0 = bordered_factors_of("e]-.-.e[*.e[*.-.e",5),
    S1 = sets:union(S0, bordered_factors_of("e]-.e[*.-.e",5)),
    S2 = sets:union(S1, bordered_factors_of("e]-.e]-.e",5)),
    S3 = sets:union(S2, bordered_factors_of("-.-.e",5)),
    S4 = sets:union(S3, bordered_factors_of("-.e",5)),
    S5 = sets:union(S4, bordered_factors_of("*.e",5)),
    S6 = sets:union(S5, bordered_factors_of("*.-.e",5)),
    Sys = {system, S6, 5},
    check_system(Sys),

    format_factors(S6),
    reduction_star("e]-.e]-.-.e]-.e", Sys),
    io:format("~n"),
    reduction_star("e]*.e]*.-.e]*.e", Sys),
    
    %%% Automaton
    
    Anbn = bordered_factors_of("a[a.b]b",3),
    format_factors(Anbn),
    San = {system, Anbn, 3},
    check_system(San),
    States = automaton_states(San),
    Trans = transitions(States),
    show_automaton(Trans).
    %show_automaton(transitions(automaton_states(Sys))).


