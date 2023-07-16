/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(dd_navigate,
          [ navigate/1        % +Tree
          ]).
:- use_module(prooftree).
:- autoload(library(ansi_term), [ansi_format/3]).
:- autoload(library(apply), [foldl/4]).
:- autoload(library(edit), [edit/1]).
:- autoload(library(lists), [nth1/3, append/3]).

/** <module> Interactively navigate a proof tree
*/

%!  navigate(+Tree) is semidet.
%
%   Navigate a proof tree  as  produced   by  proof_tree/2.  This  is an
%   interactive process. This predicate succeeds if   the  user hits `q`
%   (quit) and fails to re-satisfy  proof_tree/2   if  the user hits `n`
%   (next).

navigate(Tree) :-
    nav(#{tree:Tree, path:[], factorize:true}).

nav(State) :-
    path_goals(State.path, State.tree, Path),
    sub_tree(State.path, State.tree, SubTree),
    print_node(Path, SubTree, State),
    nav_action(State, State1),
    (   State1.get(done) == true
    ->  true
    ;   State1.get(next) == true
    ->  fail
    ;   nav(State1)
    ).

print_path([]) =>
    ansi_format(comment, '<root>~n', []).
print_path(Path) =>
    ansi_format(comment, 'Callers: ', []),
    print_path_(Path),
    nl(user_output).

print_path_([]) =>
    true.
print_path_([n(I,N,G)|T]) =>
    ansi_format(code, '~p', [G]),
    (   N == 1
    ->  ansi_format(comment, ' <- ', [])
    ;   ansi_format(comment, ' [~d/~d] <- ', [I,N])
    ),
    print_path_(T).

path_goals([], _, []).
path_goals([H|T], Tree, [n(H,NCh,G)|GT]) :-
    pt_goal(Tree, G),
    pt_children(Tree, Children),
    length(Children, NCh),
    nth1(H, Children, SubTree),
    path_goals(T, SubTree, GT).

%!  sub_tree(?Path, +Tree, -SubTree)
%
%   True when SubTree is a sub tree of   Tree at the location defined by
%   Path. Path is a list of  integers,   where  each integer is an index
%   into the list of children.

sub_tree([], Tree, Tree).
sub_tree([H|T], Tree, SubTree) :-
    pt_children(Tree, Children),
    nth1(H, Children, SubTree0),
    sub_tree(T, SubTree0, SubTree).

%!  print_node(+Path, +Tree, +State) is det.
%
%   Print a location in the proof tree.  The goal is the goal associated
%   with Tree, which is a sub-tree of the entire tree.   State is passed
%   for settings.

print_node(Path, Tree, State) :-
    pt_goal(Tree, Goal),
    pt_children(Tree, Children0),
    maplist(pt_goal, Children0, Children),
    clause_factorized(Path, Goal,  Children,
                      FPath, FGoal, FChildren,
                      Subst, State),
    numbervars(v(FGoal,FChildren,FPath,Subst), 0, _,
               [singletons(true)]),
    br_line,
    print_path(FPath),
    print_location(Tree),
    (   Children == []
    ->  ansi_format(bold, '~p.~n', [FGoal])
    ;   ansi_format(bold, '~p :-~n', [FGoal]),
        length(Children, Count),
        foldl(print_body_goal(Count), FChildren, 1, _)
    ),
    (   Subst == []
    ->  true
    ;   ansi_format(comment, '% where~n', []),
        sort(Subst, Subst1),
        forall(member(Var = Value, Subst1),
               ansi_format(code, '~t~p = ~10|~p~n',
                           [Var,Value]))
    ).

print_body_goal(Count, Goal, Nth, Nth1) =>
    Nth1 is Nth+1,
    ansi_format(comment, '~t[~d] ~8|', [Nth]),
    (   Nth == Count
    ->  Sep = '.'
    ;   Sep = ','
    ),
    ansi_format(code, '~p~w~n', [Goal, Sep]).

%!  print_location(+Tree)
%
%   If the goal associated to Tree  is   a  Prolog  predicate, print the
%   clause location that resulted in this answer.

print_location(Tree) :-
    pt_clause(Tree, CRef),
    clause_property(CRef, file(File)),
    clause_property(CRef, line_count(Line)),
    !,
    ansi_format(comment, '% ~w:~d~n', [File, Line]).
print_location(_).

clause_factorized(Path, Goal, Children, FPath, FGoal, FChildren, Subst, State) :-
    State.get(factorize) == true, !,
    clause_factorized(Path, Goal, Children, FPath, FGoal, FChildren, Subst).
clause_factorized(Path, Goal, Children, FPath, FGoal, FChildren, [], _) :-
    v(Path, Goal, Children) = v(FPath, FGoal, FChildren).

%!  clause_factorized(+Path, +Goal, +Children,
%!                    -FPath, -FGoal, -FChildren, -Subst) is det.
%
%   Factorize Goal and Children. We do not factorize goals or terms that
%   are considered too small to be worth factorizing.

clause_factorized(Path, Goal, Children, FPath, FGoal, FChildren, Subst) :-
    term_factorized(v(Goal,Children,Path),
                    v(FGoal,FChildren,FPath), Subst0),
    rebind_small(Subst0, Subst1),
    rebind_goals([FGoal|FChildren], Subst1, Subst).

rebind_small([], []).
rebind_small([Var=Value|T0], T) :-
    term_size(Value, Size),
    Size < 4,
    !,
    Var = Value,
    rebind_small(T0, T).
rebind_small([H|T0], [H|T]) :-
    rebind_small(T0, T).

rebind_goals([], Subst, Subst).
rebind_goals([H0|T], Subst0, Subst) :-
    strip_module(H0, _, H),
    select(X=Value, Subst0, Subst1),
    X == H,
    !,
    H = Value,
    rebind_goals(T, Subst1, Subst).
rebind_goals([_|T], Subst0, Subst) :-
    rebind_goals(T, Subst0, Subst).

%!  nav_action(+Dict0, -Dict) is det.
%
%   Read a command and return a new state.

nav_action(Dict0, Dict) :-
    read_command('Ddebug? ', Command),
    nav_action(Command, Dict0, Dict).

nav_action(Command, Dict0, Dict),
    clause(path_op(Command, _, _), _) =>
    (   path_op(Command, Dict0.path, NewPath),
        Dict1 = Dict0.put(path, NewPath),
        sub_tree(Dict1.path, Dict1.tree, _)
    ->  Dict = Dict1
    ;   ansi_format(warning, 'No more (~w)~n', [Command]),
        nav_action(Dict0,Dict)
    ).
nav_action(find(Term), Dict0, Dict) =>
    find_goal(Term, Dict0, Dict).
nav_action(top, Dict0, Dict) =>
    Dict = Dict0.put(path, []).
nav_action(abort, _, _) =>
    abort.
nav_action(break, Dict0, Dict) =>
    break,
    nav_action(Dict0,Dict).
nav_action(quit, Dict0, Dict) =>
    Dict = Dict0.put(done, true).
nav_action(next, Dict0, Dict) =>
    Dict = Dict0.put(next, true).
nav_action(help, Dict0, Dict) =>
    help,
    nav_action(Dict0,Dict).
nav_action(edit, Dict0, Dict) =>
    sub_tree(Dict0.path, Dict0.tree, Tree),
    pt_clause(Tree, Clause),
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)),
    edit(File:Line),
    nav_action(Dict0,Dict).
nav_action(listing, Dict0, Dict) =>
    sub_tree(Dict0.path, Dict0.tree, Tree),
    pt_clause(Tree, Clause),
    br_line,
    listing(Clause, [source(true)]),
    br_line,
    nav_action(Dict0,Dict).
nav_action(factorize, Dict0, Dict) =>
    negate(Dict0.factorize, New),
    Dict = Dict0.put(factorize, New),
    nav(Dict).

negate(true, false).
negate(false, true).

path_op(up, Path0, Path) :-
    append(Path, [_], Path0).
path_op(down, Path0, Path) :-
    append(Path0, [1], Path).
path_op(down(N), Path0, Path) :-
    append(Path0, [N], Path).
path_op(left, Path0, Path) :-
    append(Path1, [Here], Path0),
    Here1 is Here-1,
    append(Path1, [Here1], Path).
path_op(right, Path0, Path) :-
    append(Path1, [Here], Path0),
    Here1 is Here+1,
    append(Path1, [Here1], Path).

%!  find_goal(+Target:term, +State0, -State) is det.
%
%   Find a goal in the proof tree that matches Target.

find_goal(Term, State0, State) :-
    sub_tree(State0.path, State0.tree, SubTree),
    findall(Path-Goal,
            (   sub_tree(Path, SubTree, Hit),
                pt_goal(Hit, Goal),
                goal_matches(Term, Goal)
            ), Pairs),
    (   Pairs == []
    ->  ansi_format(warning, 'No matching goals~n', []),
        nav_action(State0, State)
    ;   Pairs = [Path-_]
    ->  State = State0.put(path, Path)
    ;   sort(2, @>, Pairs, Sorted),
        length(Sorted, Hits),
        ansi_format(comment, 'Found ~D hits~n', [Hits]),
        forall(nth1(N, Sorted, Path-Goal),
               ( ansi_format(comment, '~t[~d]~8| ', [N]),
                 numbervars(Goal, 0, _, [singletons(true)]),
                 ansi_format(code, '~p~n', [Goal]))),
        (   ask_integer('Please select 1-~d? '-[Hits], 1-Hits, I)
        ->  nth1(I, Sorted, Path-_),
            State = State0.put(path, Path)
        ;   nav_action(State0, State)
        )
    ).

goal_matches(Term, Goal) :-
    Term = Goal,
    !.
goal_matches(Atom, Goal0) :-
    strip_module(Goal0, _, Goal),
    atom(Atom),
    compound(Goal),
    compound_name_arity(Goal, Atom, _).


		 /*******************************
		 *         READ COMMANDS	*
		 *******************************/

%!  read_command(+Prompt, -Command) is det.
%
%   Get a new command from the user.
%
%   @bug Reading a line works poorly  if   with  the prompt. Probably we
%   should use prompt1/1.

read_command(Prompt, Command) :-
    read_key(Prompt, Key),
    (   key_command(Key, Command0, _Comment)
    ->  command_args(Command0, Command)
    ;   char_type(Key, digit(D))
    ->  Command = down(D),
        ansi_format(comment, '[~w]~n', [Command])
    ;   ansi_format(warning,
                    'Unknown command (? for help)~n', []),
        read_command(Prompt, Command)
    ).

command_args(Command, Command) :-
    atom(Command), !,
    ansi_format(comment, '[~w]~n', [Command]).
command_args(Command0, Command) :-
    Command0 =.. [Name|Args0],
    ansi_format(code, '~w ~w? ', [Name, Args0]),
    flush_output(user_output),
    read_line_to_string(user_input, Line),
    split_string(Line, " \t", " \t", Args1),
    (   catch(maplist(convert_arg, Args0, Args1, Args), _, fail)
    ->  Command =.. [Name|Args]
    ;   ansi_format(warning, '~NInvalid arguments~n', []),
        command_args(Command0, Command)
    ).

convert_arg(int, String, Int) =>
    number_string(Int, String).
convert_arg(term, String, Term) =>
    term_string(Term, String).

key_command('?',   help,       "Help").
key_command(up,    up,         "Parent goal").
key_command(down,  down,       "First child").
key_command(left,  left,       "Previous sibling").
key_command(right, right,      "Next sibling").
key_command(k,     up,         "Parent goal"). % vi compatible bindings
key_command(j,     down,       "First child").
key_command(h,     left,       "Previous sibling").
key_command(l,     right,      "Next sibling").
key_command(d,     down(int),  "Down to Nth child").
key_command('1-9', down(int),  "Down to Nth child").
key_command(t,     top,        "Go to the top").
key_command('/',   find(term), "Find goal (below current)").
key_command(e,     edit,       "Edit").
key_command('L',   listing,    "Listing").
key_command('F',   factorize,  "Toggle factorization").
key_command(q,     quit,       "Quit").
key_command(a,     abort,      "Abort").
key_command(b,     break,      "Run nested toplevel").
key_command(n,     next,       "Next answer").

ask_integer(Fmt-Args, Low-High, Int) :-
    High =< 9,
    !,
    ansi_format(bold, Fmt, Args),
    flush_output(user_output),
    get_single_char(X),
    code_type(X, digit(Int)),
    between(Low, High, Int).
ask_integer(Fmt-Args, Low-High, Int) :-
    ansi_format(bold, Fmt, Args),
    flush_output(user_output),
    read_line_to_string(user_input, Line),
    number_string(Int, Line),
    between(Low, High, Int).

help :-
    findall(Command, key_command(_, Command, _Comment), Commands0),
    list_to_set(Commands0, Commands),
    forall((   member(Command, Commands),
               once(key_command(_, Command, Comment)),
               findall(Key, key_command(Key, Command, _), Keys)
           ),
           (   atomics_to_string(Keys, ',', KeysS),
               ansi_format(comment, '% ~w~t~20|~w~n',
                           [KeysS, Comment]))).

%!  read_key(+Prompt, -Key) is det.
%
%   Read the first character for the command.

read_key(Prompt, Key) :-
    ansi_format(bold, '~w', [Prompt]),
    flush_output(user_output),
    with_tty_raw(read_key_(Key, [])).

read_key_(Key, Sofar) :-
    get_code(Code),
    append(Sofar, [Code|T], Codes),
    (   key_code(Key0, Codes)
    ->  (   T == []
        ->  Key = Key0
        ;   append(Sofar, [Code], Sofar1),
            read_key_(Key, Sofar1)
        )
    ;   char_code(Key, Code)
    ).

key_code(up,    `\e[A`).
key_code(down,  `\e[B`).
key_code(left,  `\e[D`).
key_code(right, `\e[C`).

%!  br_line
%
%   Print a line (`<br>`) accross the screen using Unicode.

br_line :-
    tty_width(Width),
    format('~N~`\u2015t~*|~n', [Width]).

tty_width(W) :-
    catch(tty_size(_, TtyW), _, fail),
    !,
    W is max(60, TtyW - 8).
tty_width(60).



