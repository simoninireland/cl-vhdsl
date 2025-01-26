.. _rtl-control-flow:

Control flow
============

``progn``
---------

The ``progn`` form, as in Lisp, collects several other forms together
as a unit. It is mainly used in constructs like ``if`` where an arm
must be made from a single form.


``@``
-----

The ``@`` form is specific to RTLisp.

Chip logic often needs to be "sensitised" by changes in a specific set
of wires or values: a classic example is for synchronous components
whose behaviours only happen on clock edges. The ``@`` form guards a
body of forms with a sensitivity list of wires whose changes control
when the body is executed.

.. code-block:: lisp

   (@ ((posedge clk))
      (setf a (* a b 2)))

In this code the body of the ``@`` will be executed whenever there is
a positive (rising) edge on the ``clk`` pin. The sensitivity list can
contain several pins, making the block sensitive to changes in any of
them.


``posedge`` and ``negedge``
---------------------------

These two operators can only appear in sensitivity lists. They set the
``@`` block to be sensitive to positive (rising) or negative (falling)
edges respectively on a wire.
