.. _dsl-tutorial-defdsl

Defining the DSL
================

The first step is to define the DSL object. This is very simple:

.. code-block:: lisp

   (defdsl let-assign
      (:documentation "A simple assignment language with arithmetic.")

This defines a DSL called ``let-assign``, which is in practice just a
variable with an attached docstring (if provided).

The body of the macro mirrors that of Lisp's ``defclass``, a list of
clauses headed by keywords. This is a pattern that's mirrored in all
the macros.

Next: :ref:`dsl-tutorial-passes`
