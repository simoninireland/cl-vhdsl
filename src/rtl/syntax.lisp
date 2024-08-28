;; Tester functions for RTLisp
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

(in-package :cl-vhdsl/rtl)

;; ---------- Operators ----------

(defvar *arithmetic-operators* '(+ - * / % ash)
  "Arithmetic operators.")


(defvar *bitwise-operators* '(logand logior lognot logxor)
  "Bitwise arithmetic operators.")


(defvar *logical-operators* '(and or not = /=)
  "Logical operators.")


(defun operator-p (form)
  "Test that FORM is a valid operator form."
  (and (typep form 'ast::function-application)
       (let ((op (slot-value (slot-value form 'ast::operator) 'ast::symbol)))
	 (or (member op *arithmetic-operators*)
	     (member op *bitwise-operators*)
	     (member op *logical-operators*)))))


;; ---------- Assignments and local variables ----------

(defun let-p (form)
  "Test that FORM is  valid let block."
  (and (typep form 'ast::let-form)
       (let ((body (slot-value form 'ast::iprogn-forms)))
	 (every #'expression-p body))))


;; ---------- Expressions ----------

(defun expression-p (form)
  "Test that FORM is a valid expression form."
  (funcall (one-of-p let-p operator-p) form))


;; ---------- Fragments ----------

(defun fragment-p (form)
  "Test that FORM is a valid RTLisp fragment."
  (funcall (one-of-p expression-p) form))
