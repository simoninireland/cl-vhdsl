;; The simple DSL from the tutorial
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

(in-package :cl-vhdsl/examples/dsl-tutorial)


;; ---------- Defining the DSL ----------

(defdsl let-assign
    (:documentation "A simple assignment language with arithmetic"))


;; ---------- Declaring functions over the DSL ----------

(defun/dsl elaborate-constants (form)
  (:documentation "Evaluate any constant expressions in FORM.")
  (:dsl let-assign))


(defun/dsl typecheck (form env)
  (:documentation "Type-check FORM in ENV.")
  (:dsl let-assign))


;; ---------- Adding forms ----------

(deform/dsl + (&rest args)
  (:documentation "Addition.")
  (:dsl let-assign))


;; ---------- Defining functions over the forms ----------

(defun/form elaborate-constants + (&rest args)
  (:dsl let-assign)
  (:body
   (let ((vals (mapcar #'elaborate-constants args)))
     (if (every #'integerp vals)
	 ;; every argument reduced to an integer literal
	 (apply #'+ vals)

	 ;; not every argument is constant, do nothing
	 form))))


(defun/form elaborate-constants ((form integer))
  (:dsl let-assign)
  (:body form))


(defun/form typecheck ((form integer) env)
  (:dsl let-assign)
  (:body 'integer))


(defun/form typecheck + ((&rest args) env)
  (:dsl let-assign)
  (:body
   (if (every (lambda (ty)
		(eql ty 'integer))
	      (mapcar (lambda (arg)
			(typecheck arg env))
		      args))
       ;; every argument is an integer, and so are we
       'integer

       ;; otherwise we've got a problem
       (error "Non-integer in arguments to + ~a" args))))


;; ---------- Adding forms and functions together ----------

(defun elaborate-operator (op args)
  "Elaborate constants in an operator OP over ARGS.

This recurses over ARGS and, if all the results are integer
literals, computes the result under OP and returns it; otheriwse,
it leaves the form unchanged."
   (let ((vals (mapcar #'elaborate-constants args)))
     (if (every #'integerp vals)
	 ;; every argument reduced to an integer literal
	 (apply op vals)

	 ;; not every argument is constant, do nothing
	 form)))


(defun typecheck-operator (op args)
  "Typecheck an operator OP over ARGS.

This assumes that each argument in ARGS must be an integer,
which results in an integer type for the operator application.
Any other result is an error."
  (if (every (lambda (ty)
	       (eql ty 'integer))
	     (mapcar (lambda (arg)
		       (typecheck arg env))
		     args))
      ;; every argument is an integer, and so are we
      'integer

      ;; otherwise we've got a problem
      (error "Non-integer in arguments to + ~a" args)))


(deform/dsl - (&rest args)
  (:documentation "Subtraction.")
  (:dsl let-assign)

  (:body elaborate-constants
	 (elaborate-operator #'- args))

  (:body typecheck
	 (typecheck-operator #'- args)))


(deform/dsl * (&rest args)
  (:documentation "Multiplication.")
  (:dsl let-assign)

  (:body elaborate-constants
	 (elaborate-operator #'* args))

  (:body typecheck
	 (typecheck-operator #'* args)))


;; ---------- Let binding ----------

(defun empty-environment ()
  "Return an empty environment."
  nil)


(defun extract-decls-variables (decls)
  "Return the variables in DECLS.

We check that each is a symbol."
  (if-let ((nonv (find-if (lambda (decl)
			    (not (symbolp (car v))))
			  decls)))
    (error "Non-variable in let bindings: ~a" nonv)

    ;; otherwise the list contains only symbols
    (mapcar #'car decls)))


(defun extend-environment (decls env)
  "Extend ENV with the new variables in DECLS.

We check that there's no shadowing."
  ;; check for shadowing
  (if-let ((shadow (find-if (lambda (v)
			      (member (car v) env))
			    decls)))
    (error "Variable ~a is shadowed by a new declaration" shadow))

  ;; type-check all the initial values
  (let ((tys (mapcar (lambda (decl)
		       (typecheck (cadr decl) env)))))
    ;; the extended environment has the new variables
    ;; and types prepended to the existing environment
    (append (mapcar #'cons
		    (mapcar #'car decls)
		    (mapcar #'cadr decls))
	    env)))


(deform/dsl let (decls &rest body)
  (:documentation "Let bindings.")
  (:dsl let-assign)

  (:body elaborate-constants
	 (let ((newbody (mapcar #'elaborate-constants body)))
	   `(let ,decls ,@newbody)))

  (:body typecheck
	 (let* ((vars (extract-decls-variables decls))
		(ext (extend-environment vars env)))
	   (let ((tys (mapcar (lambda (arg)
				(typecheck arg ext)))
		      args))

	     ;; the type of the let is the type of the last
	     ;; form in the body
	     (car (last tys))))))


;; ---------- Dealing with variable references ----------

(defun/form elaborate-constants ((form symbol))
  (:dsl let-assign)
  (:body form))


(defun/form typecheck ((form symbol) env)
  (:dsl let-assign)
  (:body
   (if-let ((m (assoc form env)))
     ;; if we find the variable, return its type
     (cddr m)

     ;; otherwise it's unknown
     (error "Undeclared variable ~a" form))))


;; ---------- Building the DSL processor ----------

(defun check-let-assign (form)
  "Run the nanopasses over FORM.

Return FORM with constants elaborated and types checked."
  (let ((el (elaborate-constants form)))
    (destructuring-bind (ty env)
	(typecheck form (empty-environment))

      ;; if we get here the form is type-correct, so just return it
      (declare (ignore ty env))
      el)))


;; ---------- Wrapping-up DSL code ----------



;; ---------- Tests ----------

;; simple closed-form expression
(eval (check-let-assign '(let ((a 1)
			       (b 2))
			  (+ a (* b b)))))

;; using quasiquoting to build the DSL form
(let ((val 23))
  (eval (check-let-assign `(let ((a 1)
				 (b (- ,val 2)))
			     (+ a (* b b))))))
