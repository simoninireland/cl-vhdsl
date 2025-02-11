;; Tests of macro expansion pass
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


;; ---------- Macros imported from Common Lisp unchanged ----------

(test test-expand-cond
  "Test we can expand the COND macro."
  (let* ((f '(cond ((= a 12)
		    (+ a 1))
	      ((> a 34)
	       (+ a 67))
	      (t
	       0)))
	 (g (rtl::expand-macros f)))
    (is (equal g '(if (= a 12)
		   (+ a 1)
		   (if (> a 34)
		       (+ a 67)
		       (the t 0)))))))


(test test-no-expand-and
  "Test we don't expand AND, which is a macro in Common Lisp."
  (is (equal (rtl::expand-macros '(and a b))
	     '(and a b))))


;; ---------- Macros (re-)implemented ----------

(test test-expand-when
  "Test we can expand WHEN conditionals."
  (is (equal (rtl::expand-macros '(when (= a b)
				   (+ a 1)
				   (- b 1)))
	     '(if (= a b)
	       (progn
		 (+ a 1)
		 (- b 1))))))


(test test-expand-unless
  "Test we can expand UNLESS conditionals."
  (is (equal (rtl::expand-macros '(unless (= a b)
				   (+ a 1)
				   (- b 1)))
	     '(if (not (= a b))
	       (progn
		 (+ a 1)
		 (- b 1))))))


(test test-expand-incf
  "Test we canm expand INCF macros."
  (is (equal (rtl::expand-macros '(let ((a 1))
				   (incf a)))
	     '(let ((a 1))
	       (setf a (+ a 1)))))

  (is (equal (rtl::expand-macros '(let ((a 1))
				   (incf (bit a 5))))
	     '(let ((a 1))
	       (setf (bit a 5) (+ (bit a 5) 1))))))


(test test-expand-decf
  "Test we canm expand DECF macros."
  (is (equal (rtl::expand-macros '(let ((a 1))
				   (decf a)))
	     '(let ((a 1))
	       (setf a (+ a 1)))))

  (is (equal (rtl::expand-macros '(let ((a 1))
				   (decf (bit a 5))))
	     '(let ((a 1))
	       (setf (bit a 5) (+ (bit a 5) 1))))))


(test test-expand-maths
  "Test we can expand the maths operators."
  (is (equal (rtl::expand-macros '(1+ 45))
	     '(+ 45 1)))
  (is (equal (rtl::expand-macros '(1- 45))
	     '(- 45 1)))
  (is (equal (rtl::expand-macros '(rtl::2* 45))
	     '(rtl::<< 45 1))))


(test test-expand-tests
  "Test we can expand the maths tests."
  (is (equal (rtl::expand-macros '(rtl:0= 45))
	     '(= 45 0)))
  (is (equal (rtl::expand-macros '(rtl:0/= 45))
	     '(/= 45 0))))
