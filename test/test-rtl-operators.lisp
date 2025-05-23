;; Tests of synthesisable fragment passes and synthesis
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


;; ---------- Addition and subtraction ----------

(test test-add-widths
  "Test we can determine the widths of additions."
  (is (subtypep (rtl:typecheck '(+ 1 1))
		'(unsigned-byte 2)))
  (is (subtypep (rtl:typecheck '(+ 15 2))
		`(unsigned-byte 5)))
  (is (not (subtypep (rtl:typecheck '(+ 15 -2))
		     `(unsigned-byte 5))))
  (is (subtypep (rtl:typecheck '(+ 15 -2))
		`(signed-byte 5))))


(test test-width-subtractions
  "Test we can extract the widths of subtractions."
  (is (subtypep (rtl:typecheck '(- 1))
		'(signed-byte 2)))
  (is (subtypep (rtl:typecheck '(- 2 1))
		'(signed-byte 3))))


(test test-synthesise-addition-operators
  "Test we can synthesise addition and subtraction operators."
  ;; arithmetic
  (dolist (op '(+ - *))
    ;; conventional two-operand
    (is (rtl:synthesise `(,op 1 2)))

    ;; Lisp-y multi-operand
    (is (rtl:synthesise `(,op 1 2 3))))

  ;; unary minus
  (is (rtl:synthesise `(- 1))))


;; ---------- Shifts ----------

(test test-width-shifts
  "Test we can extract the widths of shifts."
  (is (subtypep (rtl:typecheck '(rtl::<< 1 2))
		'(unsigned-byte 4)))
  (is (subtypep (rtl:typecheck '(rtl::<< 15 15))
		'(unsigned-byte 19)))

  (is (subtypep (rtl:typecheck '(rtl::>> 16 4))
		'(unsigned-byte 5)))

  ;; wrong number of arguments
  (dolist (op '(rtl::<< rtl::>>))
    (signals (rtl:not-synthesisable)
      (rtl:typecheck `(,op 1 2 3)))
    (signals (rtl:not-synthesisable)
      (rtl:typecheck `(,op 3)))))


(test test-synthesise-shift-operators
  "Test we can synthesise shift operators."
  (dolist (op '(rtl::<< rtl::>>))
    ;; conventional two-operand
    (is (rtl:synthesise `(,op 1 2)))))


;; ---------- Bitwise operators ----------

(test test-typecheck-logop
  "Test we can typecheck the logical operators."
  (is (subtypep (rtl:typecheck '(logand #2r10110 #2r11110))
		'(unsigned-byte 5)))

  (is (subtypep (rtl:typecheck '(logand #2r10110 #2r1111110))
		'(unsigned-byte 7))))
