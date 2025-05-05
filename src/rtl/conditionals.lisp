;; Synthesisable conditionals
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

(in-package :cl-vhdsl/rtl)
(declaim (optimize debug))


;; ---------- if (statement) ----------

(defmethod typecheck-sexp ((fun (eql 'if)) args env)
  (destructuring-bind (condition then &rest else)
      args
    (let ((tycond (typecheck condition env))
	  (tythen (typecheck then env))
	  (tyelse (if else
		      (typecheck `(progn ,@else) env))))
      (ensure-boolean tycond env)

      ;; the type of the expression is the widest of the
      ;; types of the two arms
      (if else
	  (lub tythen tyelse env)
	  tythen))))


(defmethod synthesise-sexp ((fun (eql 'if)) args (context (eql :inblock)))
  (destructuring-bind (condition then &rest else)
      args
    ;; condition
    (as-literal "if(")
    (synthesise condition :inexpression)
    (as-literal ")")
    (as-newline)

    ;; then arm
    (as-block (list then) :inblock
	      :before "begin" :after "end"
	      :always t)

    ;; else arm
    (when else
      (if (and (listp else)
	       (eql (caar else) 'if))
	  ;; else arm is another if, don't indent
	  (progn
	    (as-literal "else ")
	    (as-block else :inblock
		      :indent nil
		      :always t))

	  ;; otherwise indent
	  (progn
	    (as-literal "else" :newline t)
	    (as-block else :inblock
		      :before "begin" :after "end"
		      :always t))))))


(defmethod synthesise-sexp ((fun (eql 'if)) args (context (eql :inmodule)))
  (synthesise-sexp fun args :inblock))


;; ---------- if (expression) ----------

(defmethod synthesise-sexp ((fun (eql 'if)) args (context (eql :inexpression)))
  (destructuring-bind (condition then else)
      args
    (as-literal "(")
    (synthesise condition :inexpression)
    (as-literal " ? ")
    (synthesise then :inexpression)
    (as-literal " : ")
    (synthesise else :inexpression)
    (as-literal ")")))


;; ---------- case ----------

(defun typecheck-clause (clause ty env)
  "Typecheck case CLAUSE in ENV.

The value of the cluase should have a type compatible with TY.
Return the type of the clause body."
  (destructuring-bind (val &rest body)
      clause
    (if (not (eql val 't))
	(let ((tyval (typecheck val env)))
	  (ensure-subtype tyval ty env)))
    (typecheck (cons 'progn body) env)))


(defun typecheck-clauses (clauses ty env)
  "Typecheck case CLAUSES in ENV.

The clauses' test values should be compatible with TY.
The type is the lub of the clause types."
  (foldr (lambda (tyl clause)
	   (lub tyl (typecheck-clause clause ty env) env))
	 clauses nil))


(defmethod typecheck-sexp ((fun (eql 'case)) args env)
  (destructuring-bind (condition &rest clauses)
      args
    (let ((ty (typecheck condition env)))
      (typecheck-clauses clauses ty env))))


(defun synthesise-clause (clause context)
  "Synthesise case CLAUSE in CONTEXT."
  (destructuring-bind (val &rest body)
      clause
    (if (eql val 't)
	(as-literal "default")
	(synthesise val :inexpression))
    (as-literal ":":newline t)

    (as-block body context
	      :before "begin"
	      :after "end")))


(defmethod synthesise-sexp ((fun (eql 'case)) args (context (eql :inblock)))
  (destructuring-bind (condition &rest clauses)
      args
    (as-literal"case (")
    (synthesise condition :inexpression)
    (as-literal ")" :newline t)

    (as-block clauses :inblock :process #'synthesise-clause)

    (as-literal "endcase" :newline t)))

(defmethod synthesise-sexp ((fun (eql 'case)) args (context (eql :inmodule)))
  (synthesise-sexp fun args :inblock))


(defun synthesise-nested-if (condition clauses)
  "Synthesise a nested IF corresponding to CLAUSES applied to testing CONDITION."
  (let* ((clause (car clauses))
	 (val (car clause))
	 (body (cdr clause)))

    ;; can only operate with single-clause bodies
    (when (> (length body) 1)
      (error 'not-synthesisable :hint "CASE statements in assignments must have simple (one-form) bodies"))

    (if (> (length clauses) 1)
	`(if (= ,condition ,val)
	     ,(car body)

	     ,(synthesise-nested-if condition (cdr clauses)))

	;; if we're the last clause, last alternative is 0
	`(if (= ,condition ,val)
	     ,(car body)

	     0))))


(defmethod synthesise-sexp ((fun (eql 'case)) args (context (eql :inexpression)))
  (destructuring-bind (condition &rest clauses)
      args
    ;; synthsise a nested if expression and synthesisise that
    ;; (almost like we're a macro, but just for this particular context)
    (synthesise (synthesise-nested-if condition clauses) :inexpression)))
