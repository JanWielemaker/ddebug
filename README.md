# Declarative Debugger

This package  implements half  of the  ideas from  Włodzimierz Drabent
described in _"On Feasibility of Declarative Diagnosis"_, presented at
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

  - Hierarchical decent using the normal debugger ("skip", when wrong
    "retry", "creep" and skip the sub-goals one by one).
  - Formulate hypothesis and use spy/1 or break points to inspect the
    execution at this point.
  - Formulate hypothesis and validate these using `assertion/1`.
