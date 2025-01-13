;; Tests of binders
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


(test test-let-single
  "Test we can typecheck an expression."
  (is (subtypep (rtl:typecheck '(let ((a 1 :width 5))
				 (+ 1 a))
			       emptyenv)
		'(rtl::fixed-width-unsigned 6))))


(test test-let-single-infer-width
  "Test we can infer a width."
  (is (subtypep (rtl:typecheck '(let ((a 1))
				 (+ 1 a))
			       emptyenv)
		'(rtl::fixed-width-unsigned 2))))


(test test-let-double
  "Test we can typecheck an expression with two variables."
  (is (subtypep (rtl:typecheck '(let ((a 1 :width 8)
				      (b 6 :width 16))
				 (+ a b))
			       emptyenv)
		'(rtl::fixed-width-unsigned 24))))


(test test-let-too-narrow
  "Test we pick up too-wide initial values."
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(let ((a 100 :width 5))
		     (+ 1 a))
		   emptyenv)))


(test test-let-widen
  "Test we can take the width from a given type."
  (is (subtypep (rtl:typecheck '(let ((a 5 :type (rtl::fixed-width-unsigned 8)))
				 (+ 1 a))
			       emptyenv)
		'(rtl::fixed-width-unsigned 9))))


(test test-let-scope
  "Test we catch variables not declared."
  (signals (rtl:unknown-variable)
    (rtl:typecheck '(let ((a 1))
		     (+ 1 b))
		   emptyenv)))


(test test-let-result
  "Test we pick up the right result type."
  (is (subtypep (rtl:typecheck '(let ((a 99)
				      (b 100 :width 8))
				 (+ b 1)
				 (+ b a b))
			       emptyenv)
		'(rtl::fixed-width-unsigned 10))))


(test test-let-constant
  "Test we admit constant bindings."
  (is (subtypep (rtl:typecheck '(let ((a 15 :as :constant))
				 a)
			       emptyenv)
		'(rtl::fixed-width-unsigned 4))))


(test test-let-naked
  "Test that we accept "naked" declarations."
  (is (subtypep (rtl:typecheck '(let ((a 10)
				      b)
				 (+ a b))
			       emptyenv)
		`(rtl::fixed-width-unsigned ,(1+ rtl::*default-register-width*)))))


(test test-synthesise-binders
  "Test we can synthesise binders."
  ;; as statements
  (is (rtl:synthesise `(let ((a 1 :width 8))
			 (setf a (+ a 1)))
		      :inblock))

  ;; with constants
  (is (rtl:synthesise '(let ((a 1 :width 8)
			     (b 23 :as :constant))
			(setf c (+ a b)))
		      :inblock))

  ;; with wires
  (is (rtl:synthesise '(let ((a 1 :width 8)
			     (b 0 :width 16 :as :wire))
			(setf c (+ a b)))
		      :inblock))

  ;; can't return values in statement role
  (signals (error)
    (rtl:synthesise `(let ((a 1 :width 8))
		       (+ a 1))
		    :inblock)))
