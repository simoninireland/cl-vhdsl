;; Tests of functions
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


;; ---------- Declarations ----------

(test test-function-decl
  "Test we can declare functions."
  (vl::with-new-frame
    (vl::declare-variable 'b '((:type (unsigned-byte 8))))

    ;; normal declaration
    (let ((p (copy-tree '(flet ((lsb ()
				 (vl:bref b 0)))
			  (setf b (+ b 1))))))
      (is (vl:typecheck p)))

    ;; reject functions with arguments
    (let ((p (copy-tree '(flet ((lsb (a)
				 (vl:bref a 0)))
			  (setf b (+ b 1))))))
      (signals (vl:type-mismatch)
	(vl:typecheck p)))


    ;; reject functions with too-complicated bodies
    (let ((p (copy-tree '(flet ((lsb ()
				 (setf b 1)
				 (setf b (+ b 1))))
			  (setf b (+ b 1))))))
      (signals (cvl:type-mismatch)
	(vl:typecheck p)))))


(test test-function-decl-synthesise
  "Test we can synthesise wires."
  (let ((p (copy-tree '(flet ((lsb ()
			       (+ 1 2 3)))
			(let (b)
			  (setf b (+ b 1)))))))
    (vl:typecheck p)
    (is (vl:synthesise p))))


;; ---------- Calls ----------

(test test-function-call
  "Test we can type a function call."
  (vl::with-new-frame
    (vl::declare-variable 'f '((:as :function)
			       (:type (function () (unsigned-byte 8)))))

    (is (subtypep (vl:typecheck '(f)) '(unsigned-byte 8)))

    ;; fail if we provide an argument
    (signals (vl:type-mismatch)
      (vl:typecheck '(f 1)))))


(test test-synthesise-function-call
  "Test we can synthesise a function call."
  (vl::with-new-frame
    (vl::declare-variable 'f '((:as :function)
			       (:type (function () (unsigned-byte 8)))
			       (:initial-value (+ 1 2 3))))

    (is (vl:synthesise '(f) ))))
