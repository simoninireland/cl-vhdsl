.. _rtl-binders:

Binders
=======

RTLisp's main binding form is ``let``. This behaves as it does in
Lisp, but also accepts additional keyword arguments in each assignment
clause the help code generation.


Example
-------

.. code-block:: lisp

   (let ((a 12 :type (unsigned-byte 8) :as :register))
      (setq a 34))

defines a ``let`` block that bring a variable ``a`` into scope in its
body. ``a`` is defined to have a type ``(unsigned-byte 8)`` (8-bit
unsigned), and will be represented as a register.


Keyword arguments
-----------------

``:type`` *type*
  Specifies the Lisp type for the variable. This is closely related to
  the width: width defines the *representation* in terms of bits,
  while type defines the *interpretation* of those bits.

``:width`` *bits*
  Shortcut for ``:type (unsigned-byte *bits*)``.

``:as`` *representation*
  Specifies how the variable is represented. Options include
  ``:register`` for a register, ``:wire`` for wires, and ``:constant``
  for constants.


.. note::

   RTLisp also has :ref:`rtl-rep-spec-binders` to save entering
   representations for all variables.
