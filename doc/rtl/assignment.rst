.. _rtl-assignment:

Assignment
==========

Assignments to variables work exactly as in Lisp. The ``setq`` form
assigns values to variables. The ``setf`` form assigns to "generalised
places" that in RTLisp include variables, single a multiple bits in a
variable, and elements of arrays.


Examples
--------

Simple assignment to a variable:

.. code-block:: lisp

   (let ((a 12 :width 8))
      (setq a (* a 2))

Assignment to a single bit within a variable:

.. code-block:: lisp

   (let ((a 12 :width 8))
      (setf (bit a 0) 1))

Assignment to several slices of bits within a variable:

.. code-block:: lisp

   (let ((a 12 :width 8))
      (setf (bits a 2) #2r101)
      (setf (bits a 2 :width 1) #2r1)
      (setf (bits a 2 :end 1) #2r10))

``(setq a 1)`` and ``(setf a 12)`` are equivalent. For the other
generalised places, see under their appropriate access operators.
