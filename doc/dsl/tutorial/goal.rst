.. _dsl-tutorial-goal:

Introduction
============

This tutorial defines a very simple embedded DSL to exercise the
DSL-building macros.


The DSL
-------

We want to build a simple language, a sub-set of Lisp that allow us
very few capabilities:

- ``let`` to define values;
- ``setq`` to assign values to variables; and
- Simple arithmetic with ``+`` and ``-`` over variables and constants.

This isn't as useless an exercise at it might appear. There are
several applications where we want to enforce programmers writing code
that has no loops, for example, so it's guaranteed to terminate.
(Think event handlers or anything needing predictable performance.)


The passes
----------

As mentioned :ref:`elsewhere` <dsl-architecture>` the DSL facility is
built as a nanopass compiler, which makes frequent, small, passes over
the syntax tree to perform checks and re-writing. For our language
we'll define two passes:

- ``elaborate-constants``, to compute constant-valued expressions at
  compile time; and
- ``typecheck``, which is checks that variables are in scope when
  they're mentioned.

The former is a simple recursive pass over the tree that performs
re-writing; the latter requires an additional argument to hold the
current environment.

Next: :ref:`dsl-tutorial-defdsl`
