;; Tests of synthesisable fragment parsing
;;
;; Copyright (C) 2024 Simon Dobson
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
(in-suite cl-vhdsl)

(defvar emptyenv (rtl:empty-environment))


;; ---------- Environments ----------

(test test-names
  "Test we can extract names from an environment."
  ;; empty environment has no names
  (is (equal (rtl:get-environment-names emptyenv)
	     nil))

  ;; single name
  (let ((env1 (rtl:extend-environment 'a '() emptyenv)))
    (is (equal (rtl:get-environment-names env1)
	       '(a)))

    ;; nested environment
    (let ((env2 (rtl:extend-environment 'b '() env1)))
      (is (set-equal (rtl:get-environment-names env2)
		     '(a b)))

      ;; duplicate name
      (let ((env3 (rtl:extend-environment 'a '() env2)))
	(is (set-equal (rtl:get-environment-names env3)
		       '(a b)))))))


(test test-identifiers
  "Test legal and illegal identifiers."

  ;; legal identifiers
  (is (rtl::legal-identifier-p "a"))
  (is (rtl::legal-identifier-p "abc"))
  (is (rtl::legal-identifier-p "abc1"))
  (is (rtl::legal-identifier-p "a1Bc"))
  (is (rtl::legal-identifier-p "_a"))
  (is (rtl::legal-identifier-p "a_"))
  (is (rtl::legal-identifier-p "_123"))
  (is (rtl::legal-identifier-p "_camelCase"))

  ;; illegal idenfiers (bad characters or arrangements)
  (is (not (rtl::legal-identifier-p "_")))
  (is (not (rtl::legal-identifier-p "0abc")))
  (is (not (rtl::legal-identifier-p "01")))

  ;; illegal identifiers (keywords)
  (is (not (rtl::legal-identifier-p "reg")))
  (is (not (rtl::legal-identifier-p "inout")))

  ;; check signalling
  (signals (rtl:not-synthesisable)
    (is (not (rtl::ensure-legal-identifier "reg")))))


;; ---------- Types ----------

(test test-unsigned-range
  "Test the range of an unsigned fixed-width integer."
  (is (typep 12 '(rtl::fixed-width-unsigned 8)))
  (is (typep 12 '(rtl::fixed-width-unsigned 16)))
  (is (typep 128 '(rtl::fixed-width-signed 8)))

  (is (typep 512 '(rtl::fixed-width-unsigned 16)))
  (is (not (typep 512 '(rtl::fixed-width-unsigned 8))))

  (is (not (typep -64 '(rtl::fixed-width-unsigned 8)))))


(test test-signed-range
  "Test the range of a signed fixed-width integer."
  (is (typep 12 '(rtl::fixed-width-signed 8)))
  (is (typep 12 '(rtl::fixed-width-signed 16)))
  (is (typep 127 '(rtl::fixed-width-signed 8)))
  (is (typep -128 '(rtl::fixed-width-signed 8)))

  (is (not (typep 128 '(rtl::fixed-width-signed 8))))
  (is (not (typep -129 '(rtl::fixed-width-signed 8))))
  (is (not (typep -64 '(rtl::fixed-width-signed 4)))))


(test test-bitwidths-integer-types
  "Test we can extract the widths of integer types."
  (is (= (rtl::bitwidth '(rtl:fixed-width-unsigned 8) ()) 8))
  (is (= (rtl::bitwidth '(rtl:fixed-width-signed 8) ()) 8)))


(test test-bitwidths-integer-constants
  "Test we can extract bit widths of integer constants."
  (is (= (rtl::bitwidth 0 ()) 0))
  (is (= (rtl::bitwidth 1 ()) 1))
  (is (= (rtl::bitwidth 2 ()) 2))
  (is (= (rtl::bitwidth 127 ()) 7))

  (is (= (rtl::bitwidth -127 ()) 8))
  (is (= (rtl::bitwidth -1 ()) 2)))


;; ---------- Argument and other detailed parsing ----------

(test test-split-args-params
  "Test we can split args and params (for modules)."
  (let ((decls '(a (b :width 2)
		 :key c (d 12))))
    (destructuring-bind (args params)
	(cl-vhdsl/rtl::split-args-params decls)
      (is (equal args
		 '(a (b :width 2))))
      (is (equal params
		 '(c (d 12)))))))


(test test-split-args
  "Test we can split args with no params (for modules)."
  (let ((decls '(a (b :width 2))))
    (destructuring-bind (args params)
	(cl-vhdsl/rtl::split-args-params decls)
      (is (equal args
		 '(a (b :width 2))))
      (is (equal params nil)))))


(test test-split-params
  "Test we can split params with no args (for modules)."
  (let ((decls '(:key c (d 12))))
    (destructuring-bind (args params)
	(cl-vhdsl/rtl::split-args-params decls)
      (is (equal args nil))
      (is (equal params '(c (d 12)))))))


;; ---------- Synthesis ----------

;; It's not generally possible to check the results of synthesis, so
;; we just make sure about the errors

(test test-operators
  "Test we can synthesise operators."
  ;; arithmetic
  (dolist (op '(+ - *))
    ;; conventional two-operand
    (is (rtl:synthesise `(,op 1 2)
			:rvalue))

    ;; Lisp-y multi-operand
    (is (rtl:synthesise `(,op 1 2 3)
			:rvalue)))

  ;; unary minus
  (is (rtl:synthesise `(- 1)
		      :rvalue))

  ;; nested
  (is (rtl:synthesise `(+ 1 (- (* 2 3 -1)))
			:rvalue))

  ;; shifts
  (dolist (op '(rtl::<< rtl::>>))
    ;; conventional two-operand
    (is (rtl:synthesise `(,op 1 2)
			:rvalue))

    ;; only two operands
    (signals (rtl:not-synthesisable)
      (rtl:synthesise `(,op 1)
		      :rvalue))
    (signals (rtl:not-synthesisable)
      (rtl:synthesise `(,op 1 2 3)
		      :rvalue))))


(test test-synthesise-setf
  "Test we can synthesise assignments."
  ;; as statements
  (is (rtl:synthesise '(setf a 5)
		      :statement))
  (is (rtl:synthesise '(setf a 5 :sync t)
		      :statement)))


(test test-synthesise-progn
  "Test we can synthesise PROGN forms."
  (is (rtl:synthesise '(progn
			(setf a 5)
			(setf b 34))
		      :statement)))


(test test-synthesise-binders
  "Test we can synthesise binders."
  ;; as statements
  (is (rtl:synthesise `(let ((a 1 :width 8))
			 (setf a (+ a 1)))
		      :statement))

  ;; can't return values in statement role
  (signals (rtl:not-synthesisable)
    (rtl:synthesise `(let ((a 1 :width 8))
		       (+ a 1))
		    :statement)))


(test test-synthesise-module
  "Test we can syntheise a module with a variety of features."
  (is (rtl:synthesise '(rtl::module test ((clk :width 1 :direction :in)
					  (a :width 8 :direction :in)
					  (b :width 4)
					  :key e (f 45))
			(let ((x 0 :width 8)
			      (y 10 :width 8))
			  (when (rtl::posedge clk)
			    (setf x (+ x b) :sync t))))
		      :toplevel)))


(test test-synthesise-if-statement
  "Test we can synthesise if forms."
  ;; as statements
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))
			    (setf a (+ 1 3))))
		      emptyenv))

  ;; no else branch
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))))
		      emptyenv)))


(test test-blink
  "Test we can synthesise the blink application."
  (is (rtl:synthesise '(rtl::module blink
			((clk  :width 1 :direction :in)
			 (leds :width 5 :direction :out)
			 :key (width 5) (delay 22))
			(let ((counter 0 :width (+ bits delay))
			      (out 0 :width bits))
			  (when (rtl::posedge clk)
			    (setf counter (+ counter 1))
			    (setf out (rtl::>> counter delay)))
			  (rtl::wire leds out))))))
