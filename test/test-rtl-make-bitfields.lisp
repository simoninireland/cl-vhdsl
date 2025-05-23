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


(test test-make-bitfields-literal
  "Test we can make a bitfield from literals."
  (is (subtypep (rtl:typecheck '(rtl:make-bitfields #2r111 #2r100))
		'(unsigned-byte 6)))

  (is (subtypep (rtl:typecheck '(rtl:make-bitfields
				 #2r111
				 (the (unsigned-byte 12) 0)))
		'(unsigned-byte 15))))


(test test-make-bitfields-extend
  "Test we can extend bitfields."
  (is (subtypep (rtl:typecheck '(rtl:make-bitfields
				 #2r101
				 (rtl:extend-bits #2r0 5)))
		'(unsigned-byte 8)))

  ;; repeats must be statically known, but pattern can be variable
  (is (subtypep (rtl:typecheck '(let ((a 8))
				 (rtl:make-bitfields (rtl:extend-bits a 5))))
		'(unsigned-byte 20)))
  (is (subtypep (rtl:typecheck '(let ((a 8))
				 (rtl:make-bitfields (rtl:extend-bits (+ a 9) 5))))
		'(unsigned-byte 25)))
  (signals (rtl::not-static)
    (rtl:typecheck '(let ((a 8))
		     (rtl:make-bitfields (rtl:extend-bits 0 a))))))


(test test-test-synthesise-make-bitfields
  "Test we can synthesise constructed bitfields."
  (is (rtl:synthesise '(rtl:make-bitfields #2r111 #2r100)))

  (let ((p (copy-tree '(let ((a 1)
			     b)
			(setq b (rtl:make-bitfields (rtl:extend-bits a 5)))))))
    (rtl:typecheck p)
    (rtl:synthesise p)))
