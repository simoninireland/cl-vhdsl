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
  "Ensure that NAME is declared as a form in DSL."
  (unless (dsl-form-p name dsl)
    (error 'unknown-dsl-form :form name :dsl dsl)))


(defun dsl-macro-p (name dsl)
  "Test whether NAME is a macro within DSL."
  (not (null (member name (dsl-macro dsl)))))


(defun add-dsl-macro (name dsl &key import)
  "Add macro NAME as valid within DSL.

If the :IMPORT key is given, it indicates an existing (Lisp-level)
macro that is im[ported direcrtly into the DSL."
  (unless (dsl-form-p name dsl)
    (error 'duplicate-dsl-macro :macro name :dsl dsl))

  (appendf (dsl-macros dsl) (list (list name import))))


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



;; ---------- Generated names of functions ----------

(defun fun/dsl-name (f)
  "Return the name of the function F over the DSL."
  (intern (symbol-name f)))


(defun fun/dsl-form-name (f)
  "Return the name of function F when applied for DSL forms."
  (intern (concat (symbol-name f) "/form")))


;; ---------- Current DSL ----------

(defvar *current-dsl* nil
  "The current DSL being defined.

Set by IN-DSL. Accessed by GET-CURRENT-DSL and used as the default in
definitions that do not have an explicit :DSL clause.")


(defun set-current-dsl (dsl)
  "Set the current DSL receiving definitions."
  (setq *current-dsl* dsl))


(defun get-current-dsl ()
  "Return the current DSL being defined."
  *current-dsl*)


(defun get-dsl (sym)
  "Get the DSL named by SYM."
  (symbol-value sym))


;; ---------- Managing clauses ----------

(defun clause-p (cl)
  "Test if CL is a clause.

Clauses are lists beginning with a keyword."
  (and (listp cl)
       (symbolp (car cl))
       (eql (symbol-package (car cl))
	    (find-package "KEYWORD"))))


(defun required-clauses (req clauses)
  "Ensure that all tags in REQ appear in CLAUSES."
  (mapc (lambda (tag)
	   (when (null (get-clause tag clauses))
	     (error 'missing-required-clause :tag tag
					     :tags req
					     :hint "Ensure all required clauses are present")))
	 req))


(defun permitted-clauses (per clauses)
  "Ensure that only tags in PER appear in clauses."
  (let ((cltags (mapcar #'car clauses)))
    (mapc (lambda (tag)
	    (unless (member tag per)
	      (error 'unpermitted-clause :tag tag
					 :tags per
					 :hint "Ensure only permitted clauses are present")))
	   cltags)
    ))


(defun find-clause (tag clauses)
  "Find the clauses in CLAUSES whose tag is TAG."
  (find-if (lambda (cl)
	     (and (clause-p cl)
		  (eql (car cl) tag)))
	   clauses))


(defun find-clauses (tag clauses)
  "Return all clauses in CLAUSES whose tag is TAG."
  (remove-if-not (lambda (cl)
		   (and (clause-p cl)
			(eql (car cl) tag)))
		 clauses))


(defun get-clause (tag clauses &optional def)
  "Return the value associated with TAG in CLAUSES.

The value is the first (and assumed only) element within the clause.

If no clause matches return DEF, which is NIL by default."
  (if-let ((cl (find-clause tag clauses)))
    ;; found the clause, return the head of its body
    (cadr cl)

    ;; not found, return default
    def))


(defun get-clause-list (tag clauses &optional def)
  "Return the value associated with TAG in CLAUSES.

The value is assumed to be a list containing the remaining
elements in the clause.

If no clause matches return DEF, which is NIL by default."
  (if-let ((cl (find-clause tag clauses)))
    ;; found the clause, return its body
    cl

    ;; not found, return default
    def))


(defun get-dsl-clause (clauses)
  "Extract the DSL defined in CLAUSES.

The DSL clause consists of a :DSL tag and a symbol used to
identify the DSL. If the DSL exists, return it; if not, and
there is a current DSL, return that; otherwise signal a
NO-CURRENT-DSL error."
  (or (get-clause :dsl clauses)
      (get-current-dsl)
      (error 'no-current-dsl
	     :hint "Specify a DSL explicitly or use IN-DSL"))
  (get-clause :dsl clauses))


(defun get-body (clauses)
  "Return the body clause.

The body clause may contain an atom, a list, or several of these."
  (get-clause-list :body clauses))


(defun get-bodies (clauses)
  "Return a list of all the clauses tagged :BODY in CLAUSES."
  (let ((clbody (find-clauses :body clauses)))
    (mapcar #'cdr clbody)))


(defun split-dslfun-args (args)
  "Split ARGS into the form argument and function argument pattern."
  (list (car args)
	(cdr args)))


;; ---------- DSL macros ----------

(defmacro defdsl (name &rest clauses)
  "Define a new DSL instance named NAME.

The instance is simply a variable containing and instance
of the DSL class.

CLAUSES can contain metadata include :DOCUMENTATION for providing a
docstring."
  (permitted-clauses '(:documentation) clauses)

  (let ((doc (get-clause :documentation clauses "A DSL.")))

    `(defparameter ,name (make-instance 'dsl :description ,doc))))


(defmacro in-dsl (&optional dsl)
  "Define DSL as the current DSL.

DSL should be an unquoted symbol naming the DSL. If none
is provided the current DSL is removed."
  `(set-current-dsl ,dsl))


(defmacro defun/dsl (f args &rest clauses)
  "Define a function over a DSL.

F is the name of the function, which will be called with the
lambda list defined in ARGS.

CLAUSES can contain :DSL and :DOCUMENTATION clauses."
  (permitted-clauses '(:documentation :dsl) clauses)

  (let ((fname (fun/dsl-name f))
	(formname (fun/dsl-form-name f))
	(formarg (car args))
	(extraargs (cdr args))
	(doc (get-clause :documentation clauses "A function over a DSL."))
	(dslsym (get-dsl-clause clauses)))

    (with-gensyms (formfun formargs)
      `(progn
	 (add-dsl-function ',f ',args ,dslsym)

	 (defgeneric ,fname ,args
	   (:documentation ,doc)

	   (:method ((,formarg list) ,@extraargs)
	     (let ((,formfun (car ,formarg))
		   (,formargs (cdr ,formarg)))
	       (,formname ,formfun ,formargs ,@extraargs))))))))


(defun defun/form-over-form (f name args clauses)
  "Define a method of F that operates on forms named NAME with given ARGS."
  (permitted-clauses '(:dsl :body) clauses)

  (let* ((dslsym (get-dsl-clause clauses))
	 (dsl (get-dsl dslsym))
	 (fname (fun/dsl-form-name f))
	 (body (get-body clauses))
	 (formarg (get-dsl-function-form-argument f dsl))
	 (extraargs (get-dsl-function-extra-arguments f dsl)))
    (with-gensyms (argsarg)
      `(progn
	 (ensure-dsl-function ',f ,dslsym)
	 (ensure-dsl-form ',name ,dslsym)

	 (defmethod ,fname ((,formarg (eql ',name)) ,argsarg ,@extraargs)
	   (destructuring-bind (,@args)
	       ,argsarg
	     ,@body))))))


(defun defun/form-over-whole-form (f spec clauses)
  "Define a method specialising on a whole form within a DSL."
  (permitted-clauses '(:dsl :body) clauses)

  (let* ((dslsym (get-dsl-clause clauses))
	 (dsl (get-dsl dslsym))
	 (fname (fun/dsl-name f))
	 (body (get-body clauses))
	 (extraargs (get-dsl-function-extra-arguments f dsl)))
    `(progn
       (ensure-dsl-function ',fname ,dslsym)

       (defmethod ,fname (,@spec ,@extraargs)
	 ,@body))))


(defmacro defun/form (f &rest rest)
  "Define an arm of function F over a DSL.

There are two forms to this macro. The first takes a list as
argument, which is treated as a specialsation for the form
passed to the function. The rest of the arguments for metadata
and the body of the function when specialised like this.

The second form takes an atom representing a form within the
DSL, folloed by a lambda list. The body of the function is
applied to the arguments as described in this lambda list.

Both forms accept a :DSL clause. "
  (let ((spec (car rest)))
    (cond ((listp spec)
	   ;; specialiser is a list, switching on the form
	   (defun/form-over-whole-form f spec (cdr rest)))

	  ((atom spec)
	   ;; specialiser is a symbol, switching on the form's lead symbol
	   (destructuring-bind (name args &rest clauses)
	       rest
	     (defun/form-over-form f name args clauses)))

	  (t
	   (error "Malformed DSL function definition")))))


(defun deform/dsl-short (name clauses)
  "Declare a form named NAME in the DSL."
  (permitted-clauses '(:dsl :documentation) clauses)

  (let ((dslsym (get-dsl-clause clauses)))
    `(add-dsl-form ',name ,dslsym)))


(defun deform/dsl-function (f name args dslsym body)
  "Define function F over the form."
  (let ((fname (fun/dsl-name f)))
    `(defun/form ,fname ,name ,args
       ,(if dslsym
	    `(:dsl ,dslsym))
       (:body ,@body))))


(defun deform/dsl-full (name args clauses)
  "Define a DSL form named NAME taking ARGS, and a set of functions for it."
  (permitted-clauses '(:dsl :body :documentation) clauses)

  (let* ((dslsym (get-dsl-clause clauses))
	 (dsl (get-dsl dslsym))
	 (doc (get-clause :documentation clauses "A DSL form."))
	 (fns (get-bodies clauses))
	 (fndefs (mapcar (lambda (def)
			   (destructuring-bind (f &rest body)
			       def
			     (deform/dsl-function f name args dslsym body)))
			 fns)))

    `(progn
       ;; define the form in the dsl
       (deform/dsl ,name
	   ,(if dslsym
		`(:dsl ,dslsym))
	 (:documentation ,doc))

       ;; define the function mathods on this form
       ,@fndefs)))


(defmacro deform/dsl (name &rest rest)
  "Define a form called NAME within the DSL.

This macro has short and long forms. The short form accepts a symbol
and defines it as usable as a form within the DSL.

The long form takes a symbol and a lambda list, followed by a
sequence of clauses

"
  (let ((args (car rest)))
    (cond ((and (listp args)
		(not (null args))
		(not (clause-p args)))
	   ;; definition has an arg list, so it's a full definition
	   (deform/dsl-full name args (cdr rest)))

	  ((or (null rest)
	       (clause-p (car rest)))
	   ;; no args, so it's a short definition
	   (deform/dsl-short name rest))

	  (t
	   (error "Malformed DSL form definition")))))


(defmacro defmacro/dsl (name args &body clauses)
  "Define a macro NAME for use in a DSL."
  (let* ((dslsym (get-dsl-clause clauses))
	 (dsl (get-dsl dslsym))
	 (import (get-dsl-clause :import clauses))
	 (body (get-body clauses)))

    (if import
	;; importing an existing macro
	`(add-dsl-macro ',name ,dslsym :import ',import)

	;; defining a new macro
	`(progn
	   (add-dsl-macro ',name ,dslsym)

	   (defmacro ,name ,args
	     ,@body)))

    )

  )


;; ---------- Examples ----------

;; (defmacro/dsl when (condition &body body)
;;   (:dsl rtl)
;;   (:import when))


;; (defmacro/dsl always (&body body)
;;   (:dsl rtl)
;;   `(when t ,@body))
