.. _core-rep-spec-binders:

Representation-specific binders
===============================

The general-purpose :ref:`vl-binders` provide for the selection of
representations (as wires, registers, and constants) for bindings.
However, sometimes a block of bindings with the *same* representation
is needed, and these can be declared using the representation-specific
binder macros.

There are three macros for the three different representations.

.. code-block:: lisp

   (let-wires ((clk 0 :width 1))
      ...)

   (let-registers ((a 0 :width 8))
      ...)

   (let-constants ((mask #2r111000 ))
      ...)

In each case the variables declared are given the corresponding
representation. These constructs can be freely mixed with "ordinary"
binders as appropriate to make the code clearer.
