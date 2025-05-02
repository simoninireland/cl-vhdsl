;; The compiler passes
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


;; ---------- Helper macros ----------

;; Needs more finesse -- at the moment does nothing useful

(defmacro with-rtl-errors-not-synthesisable (&body body)
  "Run BODY within a handler that makes RTLisp errors non-synthesisable.

Non-error conditions are ignored; non-RTLisp-specific errors are reported
as NON-SYNTHESISABLE errors."
  `(handler-bind ((error #'(lambda (cond)
			     (cond ((subtypep (type-of cond) 'rtl-condition)
				    (error cond))
				   (t
				    (error cond))))))
     ,@body))


(defparameter *current-form-queue* nil
  "The current form being evaluated.

This is a stack of forms being evaluated, used to contextualise
conditions.")


(defmacro with-current-form (form &body body)
  "Execute BODY within the current FORM.

Any conditions reported in BODY will be pointed as FORM as the current
form."
  `(unwind-protect
	(progn
	  (push ,form *current-form-queue*)
	  ,@body)

     (pop *current-form-queue*)))


(defun current-form ()
  "Return the current form."
  (car *current-form-queue*))


;; ---------- Variable shadowing ----------

;; For now we disallow shadowing variables in nested scopes
;; We should actually disallow shadowing in parallel scopes as well,
;; since the float-let-blocks pass will place them all at the same level
;; (Don't want to leave detection till then because of macro-expansion.)

(defgeneric detect-shadowing (form env)
  (:documentation "Check whether FORM shadows variables declared in ENV.

This pass is used to disallow shadowing in all case.")
  (:method ((form integer) env)
    '())
  (:method ((form symbol) env)
    '())
  (:method ((form list) env)
    (destructuring-bind (fun &rest args)
	form
      (detect-shadowing-sexp fun args env))))


(defgeneric detect-shadowing-sexp (fun args env)
  (:documentation "Check whether FUN called on ARGS in ENV shadows any variables.")
  (:method (fun args env)
    (mapc (rcurry #'detect-shadowing env) args)
    t))


;; ---------- Variable re-writing ----------

(defgeneric rewrite-variables (form rewrite)
  (:documentation "Re-write free occurrances of variables in FORM.

The re-write rules in REWRITE are an alist mapping variable
names to their new form. No checks are performed.")
  (:method ((form integer) rewrite)
    form)
  (:method ((form symbol) rewrite)
    (if-let ((a (assoc form rewrite
		       :key #'symbol-name
		       :test #'string-equal)))
      ;; reference to rewriteable variable, re-write it
      (let ((w (cadr a)))
	w)

      ;; leave alone
      form))
  (:method ((form list) rewrite)
    (destructuring-bind (fun &rest args)
	form
      (rewrite-variables-sexp fun args rewrite))))


(defgeneric rewrite-variables-sexp (fun args rewrite)
  (:documentation "Rewite variables in REWRITE in FUN applied to ARGS.

Note that /everything/ gets re-written by default, including FUN.
(This is the only consistent way to deal with, for example, macros
that haven't yet been expanded in the body of a macro that needs to
re-write variables, wuch as WITH-BITFIELDS.) Override the default
method to change this behaviour.")
  (:method (fun args rewrite)
    (mapcar (rcurry #'rewrite-variables rewrite)
	    `(,fun ,@args))))


;; ---------- Constant folding ----------

(defgeneric fold-constant-expressions (form)
  (:documentation "Fold constants in expression FORM.

This folds and simplifies expressions, eliminating any
calculations that can be done early.")
  (:method (form)
    form)
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (fold-constant-expressions-sexp fun args))))


(defgeneric fold-constant-expressions-sexp (fun args)
  (:documentation "Fold constant expressions in FUN applied to ARGS.")
  (:method (fun args)
    (cons fun (mapcar #'fold-constant-expressions args))))



;; ---------- Type and width checking and inference ----------

(defgeneric typecheck (form env)
  (:documentation "Type-check FORM in ENV.")
  (:method ((form list) env)
    (let ((fun (car form))
	  (args (cdr form)))
      (with-rtl-errors-not-synthesisable
	(with-current-form (cons fun args)
	  (expand-type-parameters (typecheck-sexp fun args env) env))))))


;; Wrap an error catcher around the function to convert parsing
;; issues (which signal implementation-specific conditions)
;; into not-synthesisable conditions

;; (defmethod typecheck :around (form env)
;;   (handler-bind
;;       ((error (lambda (cond)
;;		(error 'not-synthesisable :underlying-condition cond
;;					  :hint "Mistake"))))
;;     (call-next-method)))


(defgeneric typecheck-sexp (fun args env)
  (:documentation "Type-check the application of FUN to ARGS in ENV."))


(defgeneric typecheck-sexp-setf (selector val env selectorargs &key sync)
  (:documentation "Type-check a SETF form allowing generalised places.

This matches a form (SETF (SELECTOR SELECTORARGS) VAL) and allows
different selectors to be used as generalised places."))


;; ---------- Let block coalescence ----------

(defgeneric float-let-blocks (form)
  (:documentation "Float nested LET blocks in FORM to the outermost level.

Return a list consisting of the new form and any declarations floated.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-rtl-errors-not-synthesisable
	(float-let-blocks-sexp fun args)))))


(defgeneric float-let-blocks-sexp (fun args)
  (:documentation "Float nested LET blocks in FUN applied to ARGS.

The default recurses into each element of ARGS and reconstructs
the form with re-written versions of ARGS.

Return a list consisting of the new form and any declarations floated.")
  (:method (fun args)
    (flet ((pairwise-append (old form)
	     (destructuring-bind (oldbody olddecls)
		 old
	       (destructuring-bind (newbody newdecls)
		   (float-let-blocks form)
		 (list (append oldbody (if (atom newbody)
					   (list newbody)
					   (list newbody)))
		       (append olddecls newdecls))))))

      (destructuring-bind (fargs fdecls)
	  (foldr #'pairwise-append args '(() ()))
	`((,fun ,@fargs) ,fdecls)))))


;; ---------- PROGN coalescence ----------

(defgeneric simplify-progn (form)
  (:documentation "Collapse unnecessary PROGN forms in FORM.

This removes the PROGN around a single other form, as
well as PROGNs nested inside other PROGNs.")
  (:method ((form list))
    (let ((fun (car form))
	  (args (cdr form)))
      (with-rtl-errors-not-synthesisable
	(simplify-progn-sexp fun args)))))


(defgeneric simplify-progn-sexp (fun args)
  (:documentation "Simplify PROGN blocks in FUN applied to ARGS.")
  (:method (fun args)
    `(,fun ,@(mapcar #'simplify-progn args))))


;; ---------- Macro expansion ----------

(defgeneric expand-macros (form)
  (:documentation "Expand macros in FORM.")
  (:method (form)
    form)
  (:method ((form list))
    (destructuring-bind (fun &rest args)
	form
      (expand-macros-sexp fun args))))


(defun expand-descend (fun args)
  "Expand macros in ARGS when FUN applied."
  `(,fun ,@(remove-if #'null (mapcar (lambda (arg)
				       (unless (null arg)
					 (expand-macros arg)))
				     args))))


(defgeneric expand-macros-sexp (fun args)
  (:documentation "Expand macros in FUN applied to ARGS.")
  (:method (fun args)
    (declare (optimize debug))
    (with-rtl-errors-not-synthesisable
      (if-let ((m (assoc fun *macros*)))
	;; macro is expandable, replace with real name if there is one
	(let ((realfun (or (safe-cadr m)
			   fun)))
	  (multiple-value-bind (expansion expanded)
	      (macroexpand-1 (cons realfun args))
	    (if expanded
		;; form was expanded by macro, expand the expansion
		(expand-macros expansion)

		;; form wasn't expanded, descend into the form
		(expand-descend fun args))))

	;; macro is not expandable, descend into the form
	(expand-descend fun args)))))




;; ---------- Synthesis ----------

;; In an expression-oriented language like Lisp, any form can
;; be used within the body or arguments of any other form.
;; In statement-oriented languages like Verilog, however, there
;; are more restrictions on what can appear where and often
;; specific syntax to be used for the same concepts in different
;; plaes (for example if statements /versus/ conditional expressions).
;;
;; To bridge this gap, synthesis occurs in a specific /context/
;; indicating how the form is being used. The context for synthesising
;; a sub-form can be set by its parent form, and may be different to
;; that form's own context.
;;
;; We use the following contexts:
;;
;; - :toplevel -- for top-level items like modules
;; - :inmodule -- for elements directly within a module
;; - :inblock -- for elements used as statements within a block
;; - :inexpression -- for expressions
;; - :indeclaration -- for declarations
;; - :inassignment -- as the target for an assignment

(defgeneric synthesise (form context)
  (:documentation "Synthesise the Verilog for FORM.

The form may have a specified role of position indicated by CONTEXT.
This may be used to specialise synthesis methods according to
syntactic classes and the like.

A NOT-SYNTHESISABLE error will be signalled for all underlying
conditions.")
  (:method ((form list) context)
    (let ((fun (car form))
	  (args (cdr form)))
      (with-rtl-errors-not-synthesisable
	(synthesise-sexp fun args context)
	t))))


(defgeneric synthesise-sexp (fun args context)
  (:documentation "Write the synthesised Verilog of FUN called with ARGS.

The synthesised code may depend on the role or position CONTEXT,
which can be used to specialise the method."))


;; ---------- Lispification ----------

(defgeneric lispify (form env)
  (:documentation "Convert FORM to a Lisp expression  in ENV.")
  (:method ((form list) env)
    (let ((fun (car form))
	  (args (cdr form)))
      (with-rtl-errors-not-synthesisable
	(lispify-sexp fun args env)))))


(defgeneric lispify-sexp (fun args env)
  (:documentation "Convert FUN applied to ARGS in ENV to Lisp.

The default leaves the expression unchanged, i.e., assumes that
this RTLisp fragment is valid Lisp.")
  (:method (fun args env)
    (let ((lispargs (mapcar (rcurry #'lispify env) args)))
      `(,fun ,@lispargs))))
