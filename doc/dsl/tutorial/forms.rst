.. _dsl-tutorial-deform:

Defining functional forms
=========================

Since functional forms are so important in Lisp, we need a way of
defining them.

We need to be careful with our mental model here. We :ref:`previously
<dsl-tutorial-passes>` defined functions *over* the DSL. These are
Lisp-level objects that process DSL programs. We are now defining
forms *within* the DSL: the functional forms that those functions will
operate over. Essentially every form we define *in* the DSL will lead to
some Lisp-level code in the functions *over* the DSL. Indeed, the DSL
macros are basically here to automate the structure we impose on this
code.


Adding a form
-------------

To add a form we need to know two things: the name of the form, and
the arguments it can take. A name can be any symbol. The arguments can
be described using any valid Lisp lambda-list, exactly as could be used
for defining any Lisp function -- because that's in the end what we're
doing.

As an example, let's define addition:

.. code-block:: lisp

   (deform/dsl + (&rest args)
      (:documentation "Addition.")
      (:dsl let-assign))

This defines a functional form ``+`` that takes a list of arguments.
In doing this we take on a debt to the future: we're going to have to
explain to every function over the DSL how it should process this
form.

Next: :ref:`dsl-tutorial-defun-simple`
