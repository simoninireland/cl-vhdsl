.. _rtl:

RTLisp
======

Register Transfer Lisp, RTLisp, or RTL, is a domain-specific language
embedded into Common Lisp. It represents the "synthesisable fragment"
of Lisp: the sub-set of Lisp forms that can be synthesised to an FPGA
or similar hardware. Essentially you can think of RTLisp as a
Lisp-like alternative to Verilog -- and indeed RTLisp works by
synthesising a Verilog program from the RTLisp code.

This may not seem very useful, but because RTLisp code is embedded
into Lisp we have all the power of Lisp available to generate RTLisp
code. As long as the result is RTLisp we can use *any* Lisp code to
generate it. Specifically we can define macros that expand Lisp code
into RTLisp code. The result is that we can easily extend RTLisp with
new language constructs, as long as they expand into the core
synthesisable fragment.


Types
-----

RTLisp leverages Lisp's type system by adding types appropriate for
hardware synthesis.

.. toctree::

   rtl/fixed-width


Core forms
----------

The core of RTLisp covers all the constructs that can be directly
synthesised. It includes basic control flow, conditional, and maths
capabilities, as well as decomposition of code into modules and checks
on the types passed to operators.

.. toctree::

   rtl/binders
   rtl/assignment
   rtl/control-flow
   rtl/operators
   rtl/conditionals
   rtl/arrays


Extended forms
--------------

RTLisp is still Lisp and so can use macros to extend the core
language. This includes using familiar macros that extend the
language, as well as some that provide features important for hardware
synthesis that nonetheless don't need to be core language features.

.. toctree::

   rtl/cond
   rtl/with-bitfields
