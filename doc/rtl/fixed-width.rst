.. _rtl-fixed-width:

Fixed-width types
=================

RTLisp re-uses the Lisp types as far as possible. The types
``unsigned-byte`` and ``signed-byte`` are used to represent
integers with a fixed bit-width, which may be unsigned or signed.

The type ``bit`` corresponds to the type ``(unsigned-byte 1)``.
