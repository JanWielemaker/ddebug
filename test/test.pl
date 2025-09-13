:- use_module(library(ddebug)).
:- use_module(library(chat80)).
:- ensure_loaded(swi('../../bench/programs/zebra')).

test_chat(N, Sentence, CorrectAnswer) :-
    chat80:test_chat(N, Sentence, CorrectAnswer).

p(X) :-
    q(X),
    r(X).

q(X) :-
    between(1, 10, X).

r(X) :-
    X mod 2 =:= 0.

p2(X) :-
    r2(X),
    q2(X).

q2(X) :-
    between(1, 10, X).

r2(X) :-
    freeze(X, X mod 2 =:= 0).

