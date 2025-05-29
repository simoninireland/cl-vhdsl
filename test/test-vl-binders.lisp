;; Tests of binders
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/vl)


(test test-let-single
  "Test we can typecheck an expression."
  (is (subtypep (vl:typecheck '(let ((a 1 :width 5))
				 (+ 1 a)))
		'(unsigned-byte 6))))


(test test-let-at-least-one
  "Test that a variable gets at least a width of one bit."
  (is (equal '(unsigned-byte 1)
	     (vl:typecheck '(let ((a 0))
			      a)))))


(test test-let-single-infer-width
  "Test we can infer a width."
  (is (subtypep (vl:typecheck '(let ((a 1))
				 (+ 1 a)))
		'(unsigned-byte 2))))


(test test-let-double
  "Test we can typecheck an expression with two variables."
  (is (subtypep (vl:typecheck '(let ((a 1 :width 8)
				      (b 6 :width 16))
				 (+ a b)))
		'(unsigned-byte 24))))


(test test-let-too-narrow
  "Test we pick up too-wide initial values."
  (signals (vl:type-mismatch)
    (vl:typecheck '(let ((a 100 :type (unsigned-byte 5)))
		     (+ 1 a)))))


(test test-let-widen
  "Test we can take the width from a given type."
  (is (subtypep (vl:typecheck '(let ((a 5 :type (unsigned-byte 8)))
				 (setf a (+ 1 a))))
		'(unsigned-byte 9))))


(test test-let-missing-width-type-conflicts
  "Test we pick up an inferred width conflicting with a set type"
  (signals (vl:type-mismatch)
    (vl:typecheck '(let ((a 5))
		     (setq a 16)))))


(test test-let-scope
  "Test we catch variables not declared."
  (signals (vl:unknown-variable)
    (vl:typecheck '(let ((a 1))
		     (+ 1 b)))))


(test test-let-result
  "Test we pick up the right result type."
  (is (subtypep (vl:typecheck '(let ((a 99)
				      (b 100 :width 8))
				 (+ b 1)
				 (+ b a b)))
		'(unsigned-byte 10))))


(test test-let-constant
  "Test we admit constant bindings."
  (is (subtypep (vl:typecheck '(let ((a 15 :as :constant))
				 a))
		'(unsigned-byte 4))))


(test test-let-naked
  "Test that we accept "naked" declarations."
  (is (subtypep (vl:typecheck '(let ((a 10)
				      b)
				 (+ a b)))
		`(unsigned-byte 5))))


(test test-binders-conditional
  "Test we can assign to a conditional."
  (is (subtypep (vl:typecheck '(let ((a 1)
				     (b 23))
				(let ((c (if (= a 0)
					     23
					     1)
					 :as :wire
					 :type (unsigned-byte 32)))
				  (setq a c))))
		'(unsigned-byte 32))))


(test test-binders-legalise
  "Test we can legalise variables within a binding."
  (is (tree-equal (vl::legalise-variables `(let ((a 12)
						 (a-b-c 35)
						 (c 45)
						 (d-e-f 77)
						 d)
					     (let (e)
					       (setq a (+ d a-b-c d-e-f))))
					  '())
		  '(let ((a 12)
			 (a_b_c 35)
			 (c 45)
			 (d_e_f 77)
			 d)
		    (progn
		      (let (e)
			(progn
			  (setq a (+ d a_b_c d_e_f)))))))))


(test test-binders-free-variables
  "Test we can extract free variables correctly"
  (is (set-equal (vl:free-variables '(let ((a 10))
				      (+ a b)))
		 '(b)))
  (is (set-equal (vl:free-variables '(let ((a 10)
					   b)
				      (+ a b)))
		 '())))


(test test-synthesise-binders
  "Test we can synthesise binders."
  ;; as statements
  (dolist (x '((let ((a 1 :width 8))
		 (setf a (+ a 1)))
	       (let ((a 1 :width 8)
		     (b 23 :as :constant)
		     c)
		 (setf c (+ a b)))
	       (let ((a 1 :width 8)
		     (b 0 :width 16 :as :wire)
		     c)
		 (setf c (+ a b)))))
    (let ((p (copy-tree x)))
      (vl:typecheck p)
      (is (vl:synthesise p)))))


(test test-let-width
  "Test the :width shortcuts works."
  (is (subtypep (vl:typecheck '(let ((a 0 :width 12))
				  a))
		'(unsigned-byte 12))))
