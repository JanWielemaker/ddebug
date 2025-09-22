# Declarative Debugger

This package implements half  of  the   ideas  from  Włodzimierz Drabent
described      in      [On       Feasibility        of       Declarative
Diagnosis](https://dx.doi.org/10.4204/EPTCS.385.20),    presented     at
ICLP2023 as a poster.

With _half_, we mean it only  implements the part for debugging _wrong
answers_, not the scenario where no answer is produced.

I implemented this to access the  feasibility to record the proof tree
without  program  transformation  by  exploiting the  hooks  into  the
SWI-Prolog  debugger.   __This requires  the  current  GIT version  or
SWI-Prolog 9.1.12 or later__.

The other question  is whether this really produces  a more productive
debugging experience to find the  root of wrong answers than classical
debugging, i.e.,

  - Hierarchical descent using the normal debugger ("skip", when wrong
    "retry", "creep" and skip the sub-goals one by one).
  - Formulate hypothesis and use spy/1 or break points to inspect the
    execution at this point.
  - Formulate hypothesis and validate these using `assertion/1`.

## Installation

If `git` is installed on your machine, the following should work:

    swipl pack install ddebug

Else,            download           the            code           from
https://github.com/JanWielemaker/ddebug.git,   either  using   git  or
download as  an archive and  unpack.  Next, enter the  directory using
Prolog and run

    swipl pack install .

## Usage

First, load the pack using

    ?- [library(ddebug)].

Now, to debug a goal, run e.g.,

    ?- ddebug(mygoal(X,Y)).

This runs `mygoal(X,Y)`  while collecting the proof  tree.  On success
it displays  the answer  and provides an  interactive browser  for the
proof tree.  Use `?` to display help on the navigation command.

The proof tree is a nested tree of answers to (sub)goals that resulted
in the current answer.  A node is displayed as

  1. Path from the root to the current node
  2. The goal itself
  3. Its sub-goals

It  is presented  as a  _clause_, but  without any  control structure.
I.e., if the  original clauses uses an (if->then;else)  it will either
show `if,then` or `else` depending on  which branch was used to create
this answer.

## Future

Possibly this  is notably useful in  the context of SWISH  or the WASM
based  online   version.   The   availability  of  HTML   should  make
navigation, fold/unfold  and showing details using  tooltips much more
intuitive.

Second,  we  must deal  with  _incompleteness_,  notably lack  of  any
answer.   This  requires  a   different  approach  to  collecting  the
_explored tree_  rather than the  _proof tree_.  This is  described in
the paper cited above.

## Feedback

Please           discuss          at           the          SWI-Prolog
[forum](https://swi-prolog.discourse.group/)
