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

:- module(prolog_prooftree,
          [ proof_tree/2,               % :Goal,-Tree
            pt_children/2,              % +Tree,-Children
            pt_goal/2,                  % +Tree,-Goal
            pt_clause/2                 % +Tree,-ClauseRef
          ]).
:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(edinburgh)).
:- use_module(library(lists)).

/** <module> Run goal and extract a proof tree
*/

:- meta_predicate
    proof_tree(0, -).

%!  proof_tree(:Goal, -Tree)
%
%   Run Goal, capturing the derivation  tree.   Tree  is  a hierarchical
%   structure of nodes of this shape:
%
%       g(Frame, Level, Goal, CRef, Complete, Children)
%
%   Here, `Frame` is the reference to the   Prolog  stack frame that ran
%   the goal. This has no meaning to   the  user. `Level` is the nesting
%   depth of the call, `Goal` is the executed   goal  as it is after the
%   entire derivation succeeded, `CRef` is the clause that produced this
%   answer, `Complete` is internal (means we  have seen the "exit" port)
%   and Children is a list of children in _reverse order_.
%
%   Re-satisfying re-satisfies Goal and  on   success  Tree reflects the
%   proof tree of the new answer.

proof_tree(Goal, Tree) :-
    notrace,
    nodebug,
    empty_assoc(Nodes),
    b_setval(nodes, Nodes),
    setup_call_cleanup(
        asserta((user:prolog_trace_interception(Port, Frame, Choice, Action) :-
                    prolog_prooftree:trace_interception(Port, Frame, Choice, Action)), Ref),
        ( b_setval(collecting, true),
          setup_call_cleanup(
              trace,
              call(Goal),
              nodebug),
          b_setval(collecting, false)
        ),
        erase(Ref)),
    b_getval(root, Tree).

:- public trace_interception/4.
trace_interception(call, Frame, _Choice, Action),
    nb_current(collecting, true) =>
    Action = continue,
    prolog_frame_attribute(Frame, goal, Goal),
    prolog_frame_attribute(Frame, level, Level),
    Node = g(Frame, Level, Goal, _CRef, _Complete, []),
    debug(tree, 'Adding node ~p', [Node]),
    (   parent_node(Frame, PNode)
    ->  add_child(PNode, Node)
    ;   b_getval(nodes, Nodes),
        empty_assoc(Nodes)
    ->  debug(tree, 'Starting with root', []),
        add_node(Node),
        b_setval(root, Node)
    ;   debug(tree, 'Cannot connect ~p', [Node])
    ).
trace_interception(exit, Frame, _Choice, Action),
    nb_current(collecting, true) =>
    Action = continue,
    (   node(Frame, Node)
    ->  ignore(prolog_frame_attribute(Frame, clause, CRef)),
        complete(Node, CRef)
    ;   true
    ).
trace_interception(_Port, _Frame, _Choice, Action) =>
    Action = continue.

%!  parent_node(+Frame, -Node) is semidet.
%
%   Find the Node that belongs to Frame and is our parent.

parent_node(Frame, PNode) :-
    b_getval(nodes, Nodes),
    parent_frame(Frame, Parent),
    get_assoc(Parent, Nodes, PNode),
    !,
    debug(tree, 'Got parent ~p', [PNode]).

parent_frame(Frame, Parent) :-
    prolog_frame_attribute(Frame, parent, Parent0),
    parent_frame_(Parent0, Parent).

parent_frame_(Frame, Frame).
parent_frame_(Frame, Parent) :-
    prolog_frame_attribute(Frame, parent, Parent0),
    parent_frame_(Parent0, Parent).

%!  add_node(+Node) is det.
%
%   Add a node to the global assoc. Note   that the frame may already be
%   in the assoc, but in that case it should already be completed.

add_node(Node) :-
    arg(1, Node, Frame),
    b_getval(nodes, Nodes),
    put_assoc(Frame, Nodes, Node, Nodes2),
    b_setval(nodes, Nodes2).

node(Frame, Node) :-
    b_getval(nodes, Nodes),
    get_assoc(Frame, Nodes, Node).

add_child(Node, Child) :-
    add_node(Child),
    arg(6, Node, Children),
    setarg(6, Node, [Child|Children]).

complete(Node, CRef) :-
    arg(4, Node, CRef),
    arg(5, Node, true).

%!  pt_children(+Tree, -Children) is det.
%!  pt_goal(+Tree, -Goal) is det.
%!  pt_clause(+Tree, -ClauseRef) is semidet.
%
%   Get the interesting components from   the  proof tree. pt_children/2
%   reverses the recorded child nodes to restore the order of execution.

pt_children(Tree, Children) :-
    arg(6, Tree, Children0),
    reverse(Children0, Children).

pt_goal(Tree, Goal) :-
    arg(3, Tree, Goal).

pt_clause(Tree, CRef) :-
    arg(4, Tree, CRef),
    blob(CRef, clause).
