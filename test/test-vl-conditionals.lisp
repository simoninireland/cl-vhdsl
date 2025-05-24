;; Tests of sinple assignment and generalised places
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
(in-suite verilisp/rtl)
q

;; ---------- Single- and double-armeed condtionals (IF) ----------

(test test-if-then-else
  "Test we can check a complete if form."
  (is (subtypep (vl:typecheck '(if 1
				 (+ 1 2)
				 (+ 16 8)))
		'(unsigned-byte 6))))


(test test-if-then
  "Test we can check an incomplete if form."
  (is (subtypep (vl:typecheck '(if 1
				 (+ 1 2)))
		 '(unsigned-byte 3))))


(test test-synthesise-if-statement
  "Test we can synthesise if forms."
  ;; as statements
  (dolist (x '((let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))
			    (setf a (+ 1 3))))
	       (let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))

			    ;; a two-form else branch
			    (setf a (+ 1 3))
			    (setf a 12)))
	       (let ((a 0 :width 4))
			(if (logand 1 1)
			    (progn
			      ;; a two-form else branch
			      (setf a (+ 1 2))
			      (setf a (+ 1 3)))
			    (setf a 12)))))
    (let ((p (copy-tree x)))
      (vl:typecheck p)
      (is (vl:synthesise p))))

  ;; no else branch
  (let ((p (copy-tree ' (let ((a 0 :width 4))
			  (if (logand 1 1)
			    (setf a (+ 1 2)))))))
      (vl:typecheck p)
      (is (vl:synthesise p))))


;; ---------- Multi-armed value comparisons (CASE) ----------

(test test-case-compatible
  "Test we can typecheck cases with compatible clauses."
  (is (subtypep (vl:typecheck '(let ((a 12)
				      b)
				 (case a
				   (1
				    (setf b 23))
				   (2
				    (setf b 34))
				   (t
				    (setf b 0)))))
		'(unsigned-byte 8))))


(test test-case-incompatible
  "Test we catch cases with incompatible clauses."
  (signals (vl:type-mismatch)
    (is (vl:typecheck '(let ((a 12)
			      b)
			 (case a
			   (1
			    (setf b 23))
			   (2456
			    (setf b 34)))))
	'(unsigned-byte 8))))


(test test-synthesise-case
  "Test we can synthesise a CASE."
  (let ((p '(let ((a 12)
		  (b 0))
	     (case a
	       (1
		(setf b 23))
	       (2
		(setf b 34 :sync t)
		(setf a 0))
	       (t
		(setf b 0))))))
    (vl:typecheck p)
    (is (vl:synthesise p))))


(test test-synthesise-case-assignment
  "Test we can assign to the results of a CASE block."
  (let ((p '(let ((a 1)
		  (b 2))
	     (setq a
	      (case b
		(1 12)
		(2 (+ a 1))
		(t 0))))))
    (vl:typecheck p)
    (is (vl:synthesise p))))


(test test-synthesise-case-complex-bodies
  "Test we can't synthesise CASE assignments where the bodies are too complicated."
  (signals (vl:not-synthesisable)
    (let ((p '(let ((a 1)
		    (b 2))
	       (setq a
		(case b
		  (1 12)
		  (2
		   (setq b 12)
		   (+ a 1))
		  (t 0))))))
      (vl:typecheck p)
      (vl:synthesise p))))
