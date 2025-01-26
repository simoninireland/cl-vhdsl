.. _dsl:

DSL building
============

Lisp is a "programmable programming language" that makes it easy to
build new abstractions and extensions, using both run-time and
compile-time mechanisms. But this flexibility can be excessive:
sometimes one needs to control exactly what forms can be used in a
particular situation, constructing sub-sets of Lisp possibly extended
with new forms only available from that sub-set and perhaps not from
Lisp code in general. This sort of construction is often called a
*domain-specific language* or DSL that targets programming a
particular type of application domain. Since such a DSL lives inside
Lisp and can available of Lisp functionality, it is sometimes called
an *embedded* DSL.


Macros
------

The DSL-builder's main interface is a collection of macros that draw
together a DSL.

.. toctree::

   dsl/macros
