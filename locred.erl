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
         ,sigma/1
         ,ex_to_sys/2
        ]).

-export([format_factors/1]).


%% Basic Utils

-spec check_string(string()) -> boolean().

%% this is used to check if a string contains the correct precedences
check_string(String) ->
    {Res, _} = lists:foldl(fun check_string_help/2, {true, odd}, String),
    Res.

check_string_help(Char,  {true, even}) -> {not(lists:member(Char, "[.]")), odd};
check_string_help(Char,  {true, odd})  -> {lists:member(Char, "[.]"), even};
check_string_help(_Char, {false, _V})  -> {false, _V}.

-spec borders(integer()) -> string().

borders(1) -> "#";
borders(Size) -> "#." ++ borders(Size-2).

-spec add_borders(string(), integer()) -> string().

add_borders(String, Size) -> borders(Size) ++ "[" ++ String ++ "]" ++ borders(Size).

format_factors(Set) ->
    io:format("{~n"),
    sets:fold(fun(X,_) ->
                      io:format("~s~n",[X])
              end,"",Set),
    io:format("}~n").


%% Main stuff

% factors_of returns all the factors of size Size of String,
% which must contain precedences

-spec factors_of(string(), integer()) -> sets:set().

factors_of(String, Size) -> factors_help(String, 1, Size, sets:new()).
bordered_factors_of(String, Size) -> 
    case Size rem 2 of
        1 -> factors_of(add_borders(String, Size), Size);
        0 -> error("factors: size must be odd")
    end.

factors_help(String, From, Size, Set) ->
    C = string:substr(String, From, Size),
    if
        length(C) < Size -> Set;
        true -> factors_help(String, From+2, Size, sets:add_element(C, Set))
    end.



% A System has form {system, Set, Size}

-spec check_factors(string(), tuple()) -> boolean(). 

check_factors(String, System) ->
    {system, Factors, Size} = System,
    SFact = factors_of(String, Size),
    sets:is_subset(SFact, Factors).


-spec sigma(string()) -> string(). 

sigma(String) ->
    lists:filter(fun(X) -> not(lists:member(X, "[.]")) end, String).

% Check if a System is conflictual

conflictual(Str1, Str2) ->
    Str1 =/= Str2 andalso sigma(Str1) =:= sigma(Str2).

-spec check_system(tuple()) -> boolean().

check_system(System) ->
    {system, Factors, _} = System,
    Confl = sets:fold(
              fun(X,B) ->
                      sets:fold(
                        fun(Y,B1) ->
                                case conflictual(X,Y) of
                                    true  -> [{conflict, X, Y} | B1];
                                    false -> B1
                                end
                        end, B, Factors)
              end, [], Factors),
    %% Note: Factors will contain both {conflict, A, B} and {conflict, B, A}
    io:format("Conflicts: ~p~n", [Confl]),
    Confl.


% Look-back/Look-ahead automaton construction

-spec automaton_states(tuple()) -> sets:set().

automaton_states(System) ->
    {system, Factors, Size} = System,
    States = sets:fold(
               fun(X,B) ->
                       sets:fold(
                         fun(Y,B1) ->
                                 % this is to avoid spurious beginnings
                                 Flag = hd(Y) =/= hd("#") andalso lists:last(X) =/= hd("#"), 
                                 V = X ++ tl(Y),
                                 case check_factors(V, System) andalso Flag of
                                     true ->  R1 = [tl(V)];
                                     false -> R1 = []
                                 end,
                                 V1 = lists:droplast(X) ++ Y,
                                 case check_factors(V1, System) andalso Flag of
                                     true ->  R2 = [lists:droplast(V1) | R1];
                                     false -> R2 = R1
                                 end,
                                 sets:union(sets:from_list(R2),B1)
                         end, B, Factors)
               end, sets:new(), Factors),
    %% This manages "ad hoc" starting and ending states
    Start = sets:filter(fun(X) -> hd(X) =:= hd("#") end, States),
    Start1 = sets:fold(
               fun(X,B) -> 
                       sets:add_element("." ++ string:substr(X,1,2*Size-3),B)
               end, sets:new(), Start),
    End = sets:filter(fun(X) -> lists:last(X) =:= hd("#") end, States),
    End1 = sets:fold(
             fun(X,B) -> 
                     sets:add_element(tl(X) ++ ".",B)
             end, sets:new(), End),
    sets:union(States,sets:union(Start1,End1)).


is_transition(State1, State2) ->
    tl(State1) =:= lists:droplast(State2).

transitions(States) ->
    sets:fold(
      fun(X,B) ->
              sets:fold(
                fun(Y,B1) ->
                        case is_transition(X,Y) of
                            true  -> O = sets:add_element({X,Y}, B1);
                            false -> O = B1
                        end,
                        O
                end, B, States)
      end, sets:new(), States).

show_automaton(Transitions) ->
    io:format("Writing the automaton...~n"),
    {ok, F} = file:open("automa.dot", write),
    io:fwrite(F,"digraph finite_state_machine {~n",[]),
    io:fwrite(F,"rankdir = LR~n",[]),
    sets:fold(fun(X,_) -> 
                      {From, To} = X,
                      L = string:len(From) div 2,
                      Lab = string:substr(From,L+1,1),
                      F1 = string:substr(From,1,L),
                      F2 = string:substr(From,L+1,2*L),
                      T1 = string:substr(To,1,L),
                      T2 = string:substr(To,L+1,2*L),
                      io:fwrite(F,"  \"~s, ~s\" -> \"~s, ~s\"  [label = ~p] ~n", 
                                [F1,F2,T1,T2, Lab])
              end, [], Transitions),
   io:fwrite(F,"}~n",[]),
   file:close(F),
   os:cmd("dot automa.dot -Tpdf > automa.pdf"). %% uses Graphviz


% get precedences for a subword
factor_precs(Factor, Sys) ->
    {system, W, _Size} = Sys,
    Res = sets:fold(fun(X,Y) ->
                      case sigma(X) =:= Factor of
                          true -> X;
                          false -> Y 
                      end
              end,
              none,
              W),
    if 
        Res =:= none -> error({bad_factor, Factor});
        true -> Res
    end.

insert_precs_h(String, Sys, Sz, From, List) ->
    C = string:substr(String, From, Sz),
    if
        length(C) < Sz -> List;
        true -> insert_precs_h(String, Sys, Sz, From+1, 
                               List ++ [factor_precs(C, Sys)])
    end.

% insert precedences in a string, using Sys
insert_precs(String, Sys) ->
    {system, _, Size} = Sys,
    Sz = Size div 2 + 1,
    Fc = insert_precs_h(String, Sys, Sz, 1, []),
    Str1 = lists:foldl(fun(X,Y) ->
                               Y ++ string:substr(X,1,2)
                       end, "", Fc),
    string:substr(Str1,1,string:len(Str1)-2) ++ lists:last(Fc).



% Reduction and Reduction*
reduction(String, System) ->
    {ok, Handle} = re:compile("\\[[^\\[\\]]+\\]"),
    case re:split(String, Handle, [{parts, 2},{return, list}]) of
        [L,R] ->
            HP = [L ++ "[" ++ R,
                  L ++ "." ++ R,
                  L ++ "]" ++ R],
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
            io:format("----> Stop ~n"), V;
        {no, V} ->
            io:format("Problem with ~s ~nHypotheses:~n ~s ~n ~s ~n ~s ~n", [String|V]),
            {no, String, V}
    end.

% Entry point for reduction*: add precedences, borders, and checks if the staring factors are ok

-spec reduction_star(string(), tuple()) -> string() | tuple().

reduction_star(String, System) ->
    {system, _, Size} = System,
    Str = add_borders(insert_precs(String, System), Size),
    case check_factors(Str, System) of
        true  -> 
            io:format("----> ~s ~n", [Str]),
            reduction_star_h(Str, System);
        false -> io:format("reduction: bad factors in " ++ Str ++ "~n"), {no, Str}
    end.


% Build up a system from examples

ex_to_sys(Inst, Size) -> 
    S = sets:union(lists:map(fun (X) -> bordered_factors_of(X, Size) end, Inst)),
    format_factors(S),
    Sys = {system, S, Size},
    check_system(Sys),
    Sys.

    


