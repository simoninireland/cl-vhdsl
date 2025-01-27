.. _dsl-tutorial-passes:

Defining functions over the DSL
===============================

Our DSL requires at least two functions to be defined over it, one for
each pass. (It might need others to support these too.)


Lisp syntactic forms
--------------------

To understand how functions are defined, we first need to consider
Lisp syntax a little. Every Lisp expression consists of a small number
of possible expressions: integers, floating-point numbers, strings,
symbols, lists, and so on. The most common element type is a
functional form what consists of a list containing a symbol that
identifies a function and a sequence of arguments (which may
themselves be further functional forms).

A "program" in a DSL -- or indeed in Lisp proper -- usually consists
of one or more functional forms. These appear as nested lists
representing the syntax trees of the program text, where each branch
in the tree consists of a functional form with children that are the
arguments to that form that will (in Lisp, and in many DSLs) be
evaluated and passed to the function identified by the head symbol.

Lisp itself is defined implicitly by a function over its source code:
a function that takes forms and interprets or compiles them. The
exactly way in which this occurs can be different for each functional
form: some forms (like ``+`` or ``mapcar``) are simple functions that
evaluate all their arguments first; some (like ``if`` and ``let``) are
"special" forms that evaluate their arguments in different ways; and
some (like ``defun`` and ``cond``) are macros that take their
arguments unevaluated and re-arrange them into new syntactic forms
that can then be further evaluated.


Declaring a function over a DSL
-------------------------------

A function over some DSL code, then, is simply a function that takes
as its main argument the source tree of interest and does something
with it. A nanopass might return a new source tree; another function
might extract some values from the tree, or check for validity, or
something else.

From above we know there are two different classes of expression:

- atoms like numbers and strings; and
- lists as functional forms.

We expect there to be lots of different functional forms, since these
are the core of the DSL. The function can also take more arguments if
required.

For our example DSL we have two passes, each defined by a function.
Let's define the ``elaborate-constants`` function first. It simply
takes a form in the DSL and operates over it:

.. code-block:: lisp

   (defun/dsl elaborate-constants (form)
      (:documentation "Evaluate any constant expressions in FORM.")
      (:dsl let-assign))

Here we've provided the signature of the function, its docstring, and
which DSL it is part of.

The ``typecheck`` function is similar, but takes an additional
argument storing the environment that lists the current variables in
scope:

.. code-block:: lisp

   (defun/dsl typecheck (form env)
      (:documentation "Type-check FORM in ENV.")
      (:dsl let-assign))

Again, this is a function signature with docstring, defined as being
part of the same DSL.

Next: :ref:`dsl-tutorial-deform`
