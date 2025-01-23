.. _rtl-binders:

Binders
=======

RTLisp's main binding form is ``let``. This behaves as it does in
Lisp, but also accepts additional keyword arguments in each assignment
clause the help code generation.


Example
-------

.. code-block:: lisp

   (let ((a 12 :width 8 :type (fixed-width-unsigned 8)))
      (setq a 34))

defines a ``let`` block that bring a variable ``a`` into scope in its
body. ``a`` is defined to be 8 bits wide and have a type
``(fixed-width-unsigned 8)`` (8-bit unsigned).


Keyword arguments
-----------------

``:width`` *bits*
  Specifies the number of bits used to represent the variable.

``:type`` *type*
  Specifies the Lisp type for the variable. This is closely related to
  the width: width defines the *representation* in terms of bits,
  while type defines the *interpretation* of those bits.

``:as`` *representation*
  Specifies how the variable is represented. Options include
  ``:register`` for a register, and ``:wire`` for wires.
