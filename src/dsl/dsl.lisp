;; Embedded DSL generator
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

(in-package :cl-vhdsl/dsl)
(declaim (optimize debug))


;; ---------- DSL declaration ----------

(defclass dsl ()
  ((forms
    :documentation "Function forms available within the DSL."
    :initform nil
    :accessor dsl-forms)
   (macros
    :documentation "Macros expanded within the DSL."
    :initform nil
    :accessor dsl-macros)
   (functions
    :documentation "Functions over forms in the DSL."
    :initform nil
    :accessor dsl-functions)
   (description
    :documentation "Description of the DSL."
    :initarg :description
    :reader description))
  (:documentation "An embedded domain-specific language."))


(defun dsl-form-p (name dsl)
  "Test whether NAME is a valid form within DSL."
  (not (null (member name (dsl-forms dsl)))))


(defun add-dsl-form (name dsl)
  "Add forms NAME as valid within DSL.

Forms can be declared more than once."
  (unless (dsl-form-p name dsl)
    (appendf (dsl-forms dsl) (list name))))


(defun ensure-dsl-form (name dsl)
  "Ensure that NAME is declared as valid form in DSL."
  (unless (dsl-form-p name dsl)
    (error 'unknown-dsl-form :form name :dsl dsl)))


(defun dsl-function-p (name dsl)
  "Test whether NAME is a valid function over DSL."
  (not (null (assoc name (dsl-functions dsl)))))


(defun add-dsl-function (name args dsl)
  "Add a function NAME taking ARGS over DSL."
  (when (dsl-function-p name dsl)
    (error 'duplicate-dsl-function :function name :dsl dsl))

  (appendf (dsl-functions dsl) (list (list name args))))


(defun ensure-dsl-function (f dsl)
  "Ensure that F is declared as a function over DSL."
  (unless (dsl-function-p f dsl)
    (error 'unknown-dsl-function :function f :dsl dsl)))


(defun get-dsl-function-arguments (name dsl)
  "Retrieve the argument namess to function NAME over DSL."
  (if-let ((m (assoc name (dsl-functions dsl))))
    (cadr m)))


(defun get-dsl-function-form-argument (name dsl)
  "Return the argument name used for forms in function NAME."
  (if-let ((args (get-dsl-function-arguments name dsl)))
    (car args)))


(defun get-dsl-function-extra-arguments (name dsl)
  "Return the extra argument names in function NAME."
  (if-let ((args (get-dsl-function-arguments name dsl)))
    (cdr args)))



;; ---------- Names of functions ----------

(defun fun/dsl-name (f)
  "Return the name of the function F over the DSL."
  (intern (symbol-name f)))


(defun fun/dsl-form-name (f)
  "Return the name of function F when applied for DSL forms."
  (intern (concat (symbol-name f) "/form")))


;; ---------- Macro body parser helpers ----------

(defvar *current-dsl* nil
  "The current DSL being defined.

Set by IN-DSL. Accessed by GET-CURRENT-DSL and used as the default in
definitions that do not have an explicit :DSL clause.")


(defun set-current-dsl (dsl)
  "Set the current DSL receiving definitions."
  (setq *current-dsl* dsl))


(defmacro in-dsl (&optional dsl)
  "Define DSL as the current DSL.

DSL should be an unquoted symbol naming the DSL. If none
is provided the current DSL is removed."
  `(set-current-dsl ,dsl))


(defun get-current-dsl ()
  "Return the current DSL being defined."
  *current-dsl*)


(defun get-clause (tag clauses &optional def)
  "Return the value associated with TAG in CLAUSES.

If clause matches return DEF, which is NIL by default."
  (if-let ((m (assoc tag clauses)))
    (cadr m)

    def))


(defun get-body (clauses)
  "Extract the clauses of CLAUSES specifying code.

The body clauses won't have a keyword symbol as its head.
There should only be one."
  (flet ((body-clause-p (cl)
	   (and (listp cl)
		(symbolp (car cl))
		(not (eql (symbol-package (car cl))
			  (find-package "KEYWORD"))))))

    (let ((cl (remove-if-not #'body-clause-p clauses)))
      (if (= (length cl) 0)
	(error "No body")
	cl))))


(defun get-dsl-clause (clauses)
  "Extract the DSL defined in CLAUSES.

The DSL clause consists of a :DSL tag and a symbol used to
identify the DSL. If the DSL exists, return it; if not,
and there is a current DSL, return that; otherwise signal a
NO-CURRENT-DSL error."
  (or (if-let ((dsl (get-clause :dsl clauses)))
	(symbol-value dsl))
      (get-current-dsl)
      (error 'no-current-dsl
	     :hint "Specify a DSL explicitly or use IN-DSL")))


(defun split-dslfun-args (args)
  "Split ARGS into the form argument and function argument pattern."
  (list (car args)
	(cdr args)))


;; ---------- DSL macros ----------

(defmacro defdsl (name &rest clauses)
  "Define a new DSL named NAME.

The DSL is simply a variable containing an instance of the
DSL class."
  (let ((doc (get-clause :documentation clauses "A DSL.")))

    `(defparameter ,name (make-instance 'dsl :description ,doc))))


(defmacro defun/dsl (name args &rest clauses)
  "Define a function over over a DSL."
  (let ((fname (fun/dsl-name name))
	(formname (fun/dsl-form-name name))
	(formarg (car args))
	(extraargs (cdr args))
	(doc (get-clause :documentation clauses "A function over a DSL."))
	(dsl (get-dsl-clause clauses)))

    (with-gensyms (formfun formargs)
      `(progn
	 (add-dsl-function ',name ',args ,dsl)

	 (defgeneric ,fname ,args
	   (:documentation ,doc)

	   (:method ((,formarg list) ,@extraargs)
	     (let ((,formfun (car ,formarg))
		   (,formargs (cdr ,formarg)))
	       (,formname ,formfun ,formargs ,@extraargs))))))))


(defmacro defdslform (name &rest clauses)
  "Declare forms named NAME in the DSL."
  (let ((dsl (get-clause :dsl clauses (get-current-dsl))))
    `(add-dsl-form ',name ,dsl)))


(defun defdslfun-over-form (f name args clauses)
  "Define a method of F that operates on forms named NAME with given ARGS."
  (let* ((dsl (get-dsl-clause clauses))
	 (fname (fun/dsl-form-name f))
	 (body (get-body clauses))
	 (formarg (get-dsl-function-form-argument f dsl))
	 (extraargs (get-dsl-function-extra-arguments f dsl)))
    (with-gensyms (argsarg)
      `(progn
	 (ensure-dsl-function ',f ,dsl)
	 (ensure-dsl-form ',name ,dsl)

	 (defmethod ,fname ((,formarg (eql ',name)) ,argsarg ,@extraargs)
	   (destructuring-bind (,@args)
	       ,argsarg
	     ,@body))))))


(defun defdslfun-over-whole-form (f spec clauses)
  "Define a method specialising on a whole form within a DSL."
  (let ((dsl (get-clause :dsl clauses (get-currrent-dsl))))
    (ensure-dsl-function name dsl)

    (let ((fname (fun/dsl-name f))
	  (body (get-body-clause clauses)))
      (destructuring-bind ()))

    )
  )


(defmacro defdslfun (f &rest rest)
  "Define an arm of function F over a DSL."
  (let ((spec (car rest)))
    (cond ((listp spec)
	   ;; specialiser is a list, switching on the form
	   (defdslfun-over-whole-form f spec (cdr rest)))


	  ((atom spec)
	   ;; specialiser is a symbol, switching on the form's lead symbol
	   (destructuring-bind (name args &rest clauses)
	       rest
	     (defdslfun-over-form f name args clauses)))

	  (t
	   (error "Malformed DSL function definition")))))

;; ---------- Examples ----------

(defdsl rtl
    (:documentation "The synthesisable fragment of Lisp."))


;; Functions over the DSL's forms

(defun/dsl typecheck (form env)
  (:documentation "Type-check FORM in ENV.")
  (:dsl rtl))


(defdslfun typecheck ((form integer))
  (:dsl rtl)
  (let ((w (bitwidth form env)))
    (if (< w 0)
	`(fixed-width-signed ,w)
	`(fixed-width-unsigned ,w))))


(defdslfun typecheck ((form symbol))
  (:dsl rtl)
  (get-type form env))


(defdslform +
  (:documentation "Addition operator")
  (:dsl rtl))


(defdslfun typecheck + (&rest args)
  (:dsl rtl)
  (typecheck-addition args env))


(defdslmacro when (condition &body body)
  (:dsl rtl)
  (:import when))


(defdslmacro always (&body body)
  (:dsl rtl)
  `(when t ,@body))


;; all a form's functions in one go

(defdslform << (val offset)
  (:documentation "Left-shift operator")
  (:dsl rtl)

  (typecheck
   (let ((tyval (typecheck val env))
	 (tyoffset (typecheck offset env)))
     (ensure-fixed-width tyval)
     (ensure-fixed-width tyoffset)

     ;; the width is the width of the value plus the
     ;; maximum number that can be in the offset
     `(fixed-width-unsigned ,(+ (bitwidth tyval env)
				(1- (ash 1 (bitwidth tyoffset env)))))))

  (synthesise
   (as-infix '<< (list val offset))))
