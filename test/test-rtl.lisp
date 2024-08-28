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

;; ---------- Basic features ----------

(test test-constants
  "Test we can recognise constants."
  (is (rtl::constant-p (ast:parse 1)))
  (is (rtl::constant-p (ast:parse '(+ 1 2)))))


(test test-operators
  "Test we can recognise (non)-operators."

  ;; (Free variables to avoid being eliminated as constants)

  ;; arithmetic operators
  (is (rtl::operator-p (ast:parse '(+ 1 a))))
  (is (rtl::operator-p (ast:parse '(- 1 a))))
  (is (rtl::operator-p (ast:parse '(* 1 a))))
  (is (rtl::operator-p (ast:parse '(/ 1 a))))
  (is (rtl::operator-p (ast:parse '(mod 1 a))))
  (is (rtl::operator-p (ast:parse '(ash a -1))))

  ;; bitwise operators
  (is (rtl::operator-p (ast:parse '(lognot a))))
  (is (rtl::operator-p (ast:parse '(logand 45 a))))
  (is (rtl::operator-p (ast:parse '(logior 56 a))))
  (is (rtl::operator-p (ast:parse '(logxor 345 a))))

  ;; logical operators
  ;; (note that and and or are macros that expand to conditionals)
  (is (rtl::operator-p (ast:parse '(not a))))
  (is (rtl::operator-p (ast:parse '(= a 12))))
  (is (rtl::operator-p (ast:parse '(/= a 12 b))))
  (is (rtl::if-p (ast:parse (macroexpand '(and t a b)))))
  (is (rtl::constant-p (ast:parse (macroexpand '(and nil a b))))) ;; short-circuiting
  (is (rtl::let-p (ast:parse (macroexpand '(or b nil a)))))

  ;; non-operators
  (is (not (rtl::operator-p (ast:parse '(~ 1 a)))))     ;; not an operator
  (is (rtl::operator-p (ast:parse '(ash a -1 45)))))    ;; too many arguments


(test test-if
  "Test we can recognise valid and invalid if- and cond--forms."

  ;; (Free variables to avoid being eliminated as constants)

  (is (rtl::if-p (ast:parse '(if b a))))
  (is (rtl::if-p (ast:parse '(if b a (+ a 1)))))
  (is (rtl::if-p (ast:parse '(cond ((evenp a) a)
				   ((oddp (+ a 1))))))))


(test test-assignment
  "Test we can do assignments."

  ;; (Free variables to avoid being eliminated as constants)

  (is (rtl::setf-p (ast:parse'(setf A (+ 1 2 3 a))))))


;; ---------- Fragments ----------

(test test-simple
  "Test simple fragments."
  (rtl:validate '(let ((var (+ 24 2)))
		   (setf var (1+ var)))))


(test test-free
  "Test a fragment for free variables."

  ;; closed in environment
  (rtl:validate '(let ((val (+ A (ash B 8))))
		   (setf A val))
		'(A B))

  ;; unhandled free variable
  (signals rtl:unknown-variable
    (rtl:validate '(let ((val (+ A (ash B 8))))
		     (setf A val))
		  '(A))))
