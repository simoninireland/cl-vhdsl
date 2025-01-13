;; Tests of sinple assignment and generalised places
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


;; ---------- Single- and double-armeed condtionals (IF) ----------

(test test-if-then-else
  "Test we can check a complete if form."
  (is (subtypep (rtl:typecheck '(if 1
				 (+ 1 2)
				 (+ 16 8))
			       emptyenv)
		 '(rtl::fixed-width-unsigned 6))))


(test test-if-then
  "Test we can check an incomplete if form."
  (is (subtypep (rtl:typecheck '(if 1
				 (+ 1 2))
			       emptyenv)
		 '(rtl::fixed-width-unsigned 3))))


(test test-synthesise-if-statement
  "Test we can synthesise if forms."
  ;; as statements
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))
			    (setf a (+ 1 3))))
		      :inmodule))
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))

			    ;; a two-form else branch
			    (setf a (+ 1 3))
			    (setf a 12)))
		      :inmodule))
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (progn
			      ;; a two-form else branch
			      (setf a (+ 1 2))
			      (setf a (+ 1 3)))
			    (setf a 12)))
		      :inmodule))

  ;; no else branch
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))))
		      :inblock)))


;; ---------- Multi-armed value comparisons (CASE) ----------

(test test-case-compatible
  "Test we can typecheck cases with compatible clauses."
  (is (subtypep (rtl:typecheck '(let ((a 12)
				      b)
				 (case a
				   (1
				    (setf b 23))
				   (2
				    (setf b 34))
				   (t
				    (setf b 0))))
			       emptyenv)
		'(rtl::fixed-width-unsigned 8))))


(test test-case-incompatible
  "Test we catch cases with incompatible clauses."
  (signals (rtl:type-mismatch)
    (is (rtl:typecheck '(let ((a 12)
			      b)
			 (case a
			   (1
			    (setf b 23))
			   (2456
			    (setf b 34))))
		       emptyenv)
	'(rtl::fixed-width-unsigned 8))))


(test test-synthesise-case
  "Test we can synthesise a CASE."
  (is (rtl:synthesise '(let ((a 12)
			     (b 0))
			(case a
			  (1
			   (setf b 23))
			  (2
			   (setf b 34 :sync t)
			   (setf a 0))
			  (t
			   (setf b 0))))
		      :inblock)))
