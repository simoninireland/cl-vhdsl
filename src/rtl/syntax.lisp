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

;; Throughout this file "form" means "abstract syntax tree as returned by
;; ast:parse from parsing a Lisp s-expression".


;; ---------- Operators ----------

(defvar *arithmetic-operators* '(+ - * / mod ash)
  "Arithmetic operators.")


(defvar *bitwise-operators* '(logand logior lognot logxor)
  "Bitwise arithmetic operators.")


(defvar *logical-operators* '(not = /=)
  "Logical operators.")
;; ...plus and and or, which are implemented as macros expanding to
;; let or if forms


(defun operator-p (form)
  "Test that FORM is a valid operator form."
  (and (typep form 'ast::function-application)
       (let ((op (slot-value (slot-value form 'ast::operator) 'ast::symbol)))
	 (or (member op *arithmetic-operators*)
	     (member op *bitwise-operators*)
	     (member op *logical-operators*)))))


;; ---------- Constants ----------

(defun constant-p (form)
  "Test whether FORM evaluates to a constant that can be compiled away."
  (typep form 'ast::constant-form))


;; ---------- Assignments and local variables ----------

(defun let-p (form)
  "Test that FORM is a valid let block."
  (and (or (typep form 'ast::let-form)
	   (typep form 'ast::let*-form))
       (let ((body (slot-value form 'ast::iprogn-forms)))
	 (every #'expression-p body))))


;; ---------- Conditionals ----------

(defun if-p (form)
  "Test that FORM is a conditional."
  (or (typep form 'ast::if-form)
      (typep form 'ast::cond-form)))


;; ---------- Assignments ----------

(defun setf-p (form)
  "Test that FORM is a setf."
  (typep form 'ast::setf-form))


;; ---------- Expressions ----------

(defun expression-p (form)
  "Test that FORM is a valid expression form."
  (funcall (any-of-p constant-p let-p operator-p if-p setf-p)
	   form))


;; ---------- Fragments ----------

(defun fragment-p (form)
  "Test that FORM is a valid RTLisp fragment."

  ;; only expressions at the moment
  (funcall (any-of-p expression-p) form))


(defun closed-fragment-p (form vars)
  "Test that FORM is a fragment with any free variables mentioned in VARS.

VARS being empty implies that FORM is a closed-form fragment."
  (subsetp (ast:free-variables form) vars))
