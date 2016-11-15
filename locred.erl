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
        ]).

-export([format_factors/1]).

-export([main/1]).

%% Basic Utils

%% this is used to check if a string contains the correct precedences
check_string(String) ->
    {Res, _} = lists:foldl(fun check_string_help/2, {true, odd}, String),
    Res.

check_string_help(Char,  {true, even}) -> {not(lists:member(Char, "<.>")), odd};
check_string_help(Char,  {true, odd})  -> {lists:member(Char, "<.>"), even};
check_string_help(_Char, {false, _V})  -> {false, _V}.


borders(1) -> "#";
borders(Size) -> lists:append("#.",borders(Size-2)).

add_borders(String, Size) -> lists:append([borders(Size),
                                           "<", String, ">",
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
    F = fun(X) -> not(lists:member(X, "<.>")) end,
    Str1 =/= Str2 andalso lists:filter(F,Str1) =:= lists:filter(F, Str2).

check_system(System) ->
    {system, Factors, _} = System,
    Confl = sets:fold(fun(X,B) ->
                              sets:fold(fun(Y,B1) ->
                                                case conflictual(X,Y) of
                                                    true -> [{conflict, X, Y} | B1];
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



% Reduction and Reduction*
reduction(String, System) ->
    {ok, Handle} = re:compile("<[^<>]+>"),
    case re:split(String, Handle, [{parts, 2},{return, list}]) of
        [L,R] ->
            HP = [lists:append([L,"<",R]),
                  lists:append([L,".",R]),
                  lists:append([L,">",R])],
            case lists:map(fun(X) -> check_factors(X, System) end, HP) of
                [true, false, false] -> [V, _, _] = HP, {ok, V};
                [false, true, false] -> [_, V, _] = HP, {ok, V};
                [false, false, true] -> [_, _, V] = HP, {ok, V};
                _ -> {no, HP}
            end;
        _ -> % No more reductions available
            {stop, String}
    end.

reduction_star(String, System) ->
    case reduction(String, System) of
        {ok, NewString} ->
            io:format("Step: ~s ~n", [NewString]),
            reduction_star(NewString, System);
        {stop, V} ->
            io:format("---> Stop at ~s <---~n", [V]), V;
        {no, V} ->
            io:format("Problem with ~s ~nHypotheses:~n ~s ~n ~s ~n ~s ~n", [String|V]),
            {no, String, V}
    end.


% used as a script:
% escript locred.erl
main(_V) ->
    S0 = bordered_factors_of("e>-.-.e<*.e<*.-.e",5),
    S1 = sets:union(S0, bordered_factors_of("e>-.e<*.-.e",5)),
    S2 = sets:union(S1, bordered_factors_of("e>-.e>-.e",5)),
    S3 = sets:union(S2, bordered_factors_of("-.-.e",5)),
    S4 = sets:union(S3, bordered_factors_of("-.e",5)),
    S5 = sets:union(S4, bordered_factors_of("*.e",5)),
    S6 = sets:union(S5, bordered_factors_of("*.-.e",5)),
    check_system({system, S6, 5}),
    format_factors(S6),
    Sys = {system, S6,5},
    reduction_star(add_borders("e>-.e>-.-.e>-.e",5), Sys),
    io:format("~n"),
    reduction_star(add_borders("e>*.e>*.-.e>*.e",5), Sys).