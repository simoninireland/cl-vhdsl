.. _dsl-tutorial-defun-simple:

Defining the bodies of functions over a form
============================================

We now have one form and two functions in our DSL. We need to define
how these functions process instances of the form, and we;ll discover
we also need to define how they operate over non-functional forms as
well.


Elaborating constants
---------------------

Let's start with constant elaboration. We want this to take an
expression, check if it's a constant, and if so compute the value and
us it to replace the calculation. So we need also to define whether an
arbitrary DSL expression is in fact a constant or not, and leave
non-constants alone.

.. code-block:: lisp

   (defun/form elaborate-constants + (&rest args)
      (:dsl let-assign)
      (:body
	 (let ((vals (mapcar #'elaborate-constants args)))
	    (if (every #'integerp vals)
	       ;; every argument reduced to an integer literal
	       (apply #'+ vals)

	       ;; not every argument is constant, do nothing
	       form))))

It's hopefully easy to see what's happening here. We're defining
``elaborate-constants`` over a ``+`` form with given arguments. These
arguments are referenced from the form in the ``:body`` clause, the
body of the function applied to this form.

The body of the function is just Lisp, and can make use of all Lisp's
features even though they're not present in the DSL: we're writing a
function *over* the DSL processing code written *in* the DSL. So we
can use ``every``, ``mapcar``, and ``apply`` to process the code.

You'll notice the use of ``form`` in the body. Where does that come
from? -- it's not defined in the function. It was defined as an
argument when we :ref:`defined the functions initially
<dsl-tutorial-passes>`, and so is available to us in *all* the bodies
of that function, holding the entire form we were passed.


Dealing with literals
---------------------

There's obviously something else we need to deal with when thinking
about constants:literal constants themselves. These aren't functional
forms, they're just literals in the DSL program. So we need to define
our ``elaborate-constants`` function to handle thee too -- by leaving
the alone. We do this using a slightly different style:

.. code-block:: lisp

   (defun/form elaborate-constants ((form integer))
      (:dsl let-assign)
      (:body form))

What happens here? We define our function over a form that's declared
to be an integer, using the same syntax as Lisp uses to specialise the
arguments of generic functions. (It'll probably come as no great
surprise to learn that that's exactly what we *are* doing off-stage:
defining generic functions according to some very structured rules.)

Now when ``elaborate-constants`` is passed an integer literal it
returns it; and when passed an addition form it checks all the
arguments, sees if they're constants, and if so adds them up and
returns them.


Type-checking
-------------

``typecheck`` is a function that takes an additional argument holding
the names of all the variables currently in scope. When we define its
body we're going to refer to this environment, and change it.

Let's start by defining the types of integers.

.. code-block:: lisp

   (defun/form typecheck ((form integer) env)
      (:dsl let-assign)
      (:body 'integer))

The body returns ``integer``, which is the Lisp type of integers. We
could alternatively have used ``fixnum`` to stay within a smaller set
of numbers. We didn't need ``env`` for this.

Now let's define the type for addition:

.. code-block:: lisp

   (defun/form typecheck + ((&rest args) env)
      (:dsl let-assign)
      (:body
	 (if (every (lambda (ty)
		       (eql ty 'integer))
		    (mapcar (lambda (arg)
			       (typecheck arg env))
			    args))
	     ;; every argument is an integer, and so are we
	     'integer

	     ;; otherwise we've got a problem
	     (error "Non-integer in arguments to + ~a" args))))

The main part of the check looks at the types of all arguments by
calling ``typecheck`` on them. If they're all integers, so is their
addition; if not, an error is signalled.

.. note::

   If you're a functional programming weenie who likes their
   combinators raw, you can avoid those two lambda-expressions by
   observing that in both cases they're simply currying a function
   with an argument on its right. (Not the usual side for currying,
   but perfectly sensible.) ``alexandria`` has you covered for this,
   and we can re-write the main part of the function as:

   .. code-block:: lisp

      (every (rcurry #'eql 'integer)
	     (mapcar (rcurry #'typecheck env) args)

   Lisp is entirely un-opinionated about these things. It's your DSL:
   code it up however you like.
