;; Tests of MAKE-BITFIELDS and REPEAT-BITS
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of cl-vhdsl, a Common Lisp DSL for hardware design
;;
;; cl-vhdsl is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-vhdsl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-vhdsl. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl/rtl)
(declaim (optimize debug))


(test test-make-bitfields-literal
  "Test we can make a bitfield from literals."
  (is (subtypep (rtl:typecheck '(rtl:make-bitfields #2r111 #2r100)
			    emptyenv)
		'(rtl::fixed-width-unsigned 6)))

  (is (subtypep (rtl:typecheck '(rtl:make-bitfields
				 #2r111
				 (the (rtl::fixed-width-unsigned 12) 0))
			       emptyenv)
		'(rtl::fixed-width-unsigned 15))))


(test test-make-bitfields-repeat
  "Test we can repeat bitfields."
  (is (subtypep (rtl:typecheck '(rtl:make-bitfields
				 #2r101
				 (rtl:repeat-bits 5 #2r0))
			       emptyenv)
		'(rtl::fixed-width-unsigned 8)))

  ;; repeats must be statically known, but pattern can be variable
  (is (subtypep (rtl:typecheck '(let ((a 8))
				 (rtl:make-bitfields (rtl:repeat-bits 5 a)))
			       emptyenv)
		'(rtl::fixed-width-unsigned 20)))
  (is (subtypep (rtl:typecheck '(let ((a 8))
				 (rtl:make-bitfields (rtl:repeat-bits 5 (+ a 9))))
			       emptyenv)
		'(rtl::fixed-width-unsigned 25)))
  (signals (rtl::not-static)
    (rtl:typecheck '(let ((a 8))
		     (rtl:make-bitfields (rtl:repeat-bits a 0)))
		   emptyenv)))
