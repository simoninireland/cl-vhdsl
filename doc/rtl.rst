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

.. toctree::

   rtl/binders
   rtl/assignment
   rtl/operators
   rtl/arrays
   rtl/with-bitfields
