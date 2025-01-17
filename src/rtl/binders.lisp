;; Synthesisable variable declarations and bindings
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
(declaim (optimize debug))


;; A binder defines an environment, and we expect the following
;; keys to be provided where appropriate:
;;
;; - :width -- the width of a value in bits
;; - :type -- the type being held by the binding
;; - :direction -- direction of dataflow for module arguments
;; - :as -- the representation to be used, one of Lregister, :wire, :constant
;;
;; Default and consistency checks are applied.

(defun width-can-store-p (w ty env)
  "Test whether W bits can accommodate the values of TY in ENV."
  (>= w (bitwidth ty env)))


(defun ensure-width-can-store (w ty env)
  "Ensure that W bits can accommodate the values of TY in ENV."
  (unless (width-can-store-p w ty env)
    (error 'type-mismatch :expected ty :got w)))


(deftype representation ()
  "The type of variable representations.

Valid representations are :REGISTER, :WIRE, or :constant."
  '(member :register :wire :constant))


(defun representation-p (rep)
  "Test REP is a valid variable representation."
  (typep rep 'representation))


(defun ensure-representation (rep)
  "Ensure REP is a valid variable representation.

Signal VALUE-MISMATCH as an error if not."
  (unless (representation-p rep)
    (error 'value-mismatch :expected (list :register :wire :constant) :got rep)))


;; the lambda list is the "wrong way round" from "normal" to allow
;; this function to be folded across a list of declarations
(defun typecheck-decl (env decl)
  "Typecheck DECL in ENV, returning ENV extended by DECL."
  (if (listp decl)
      ;; full declaration
      (destructuring-bind (n v &key width type (as :register))
	  decl
	(ensure-representation as)

	(let ((ty (typecheck v env)))
	  (if type
	      ;; if a type is provided, make sure the initial
	      ;; value fits in it and then use that as the type
	      ;; for the binding
	      (progn
		(ensure-subtype ty type)
		(setq ty type)))

	  (if width
	      (let ((w (eval-in-static-environment width env)))
		;; if a width is provided, make sure it's enough to
		;; accommodate the type
		(ensure-width-can-store w ty env)

		;; widen the type to match the width
		(setq ty (widen-fixed-width ty width))))

	  (extend-environment n `((:initial-value ,v)
				  (:type ,ty)
				  ,(if width
				       `(:width ,width))
				  ;;(:width ,width)
				  (:as ,as))
			      env)))
      ;; otherwise we have a naked name, so apply the defaults
      (typecheck-decl env `(,decl 0 :width ,*default-register-width*))))


(defun typecheck-env (decls env)
  "Type-check the declarations DECLS to extend ENV."
  (foldr #'typecheck-decl decls env))


(defmethod typecheck-sexp ((fun (eql 'let)) args env)
  (let ((decls (car args))
	(body (cdr args)))
    (let ((ext (typecheck-env decls env)))
      (mapn (rcurry #'typecheck ext) body))))


(defun rewrite-variables-decl (decl rewrite)
  "Re-write the values of DECL using REWRITE."
  (destructuring-bind (n v &rest keys)
      decl
    `(,n ,(rewrite-variables v rewrite) ,@keys)))


(defmethod rewrite-variables-sexp ((fun (eql 'let)) args rewrite)
  (destructuring-bind (decls &rest body)
      args
    (let* ((rwdecls (mapcar (rcurry #'rewrite-variables-decl rewrite) decls))

	   ;; remove any re-writes referring to shadowed variables
	   (rwenv (typecheck-env rwdecls (empty-environment)))
	   (rwrewrite (remove-if (rcurry #'variable-defined rwenv)
				 rewrite :key #'car))

	   ;; re-write the body with these new re-writes
	   (rwbody (mapcar (rcurry #'rewrite-variables rwrewrite) body)))
      `(let ,rwdecls
	 ,@rwbody))))


;; These two functions should only re-write non-constant initial values

(defun float-let-blocks-decl (decl)
  "Decompose DECL into an assignment.

This is only required if DECL's value is not a constant,
and where there is a sensible initialiser."
  (destructuring-bind (n v &key &allow-other-keys)
      decl
    (if (array-value-p v)
	nil

	;; set the initial value
	(if (= v 0)
	    nil
	    `(setq ,n ,v)))))


(defun float-let-blocks-zero (decl)
  "Re-write DECL to have a zero initial value.

If the initial value is an array, handle it differently."
  (destructuring-bind (n v &rest keys)
      decl
    (if (array-value-p v)
	;; array values stay declared as such
	`(,n ,v ,@keys)

	;; non-arrays are zeroed
	`(,n 0 ,@keys))))


(defun remove-nulls (l)
  "Remove all sub-lists of L that are nil."
  (remove-if #'null l))


(defmethod float-let-blocks-sexp ((fun (eql 'let)) args)
  (declare (optimize debug))
  (destructuring-bind (decls &rest body)
      args

    ;; turn initial values into assignments
    ;; TODO: should ignore 0s
    (let ((assignments (remove-nulls (mapcar #'float-let-blocks-decl decls)))
	  (basedecls (remove-nulls (mapcar #'float-let-blocks-zero decls))))
      ;; extract the body and decls of the body
      (destructuring-bind (newbody newdecls)
	  (float-let-blocks `(progn ,@assignments ,@body))

	;; return the re-written body and decls
	(list newbody
	      (append basedecls newdecls))))))


(defmethod detect-shadowing-sexp ((fun (eql 'let)) args env)
  (destructuring-bind (decls &rest body)
      args
    (let ((vars (mapcar #'car decls)))
      (dolist (n vars)
	(if (variable-defined n env)
	    (error 'duplicate-variable :variable n
				       :hint "Variable shadows a previous definition"))))

    (let ((ext (typecheck-env decls env)))
      (mapc (rcurry #'detect-shadowing ext) body)
      t)))


(defun array-value-p (form)
  "Test whether FORM is an array constructor."
  (and (listp form)
       (eql (car form) 'make-array)))


(defun synthesise-register (decl context)
  "Synthesise a register declaration within a LET block.

The register has name N and initial value V, with the optional
WIDTH defaulting to the system's global width."
  (destructuring-bind  (n v &key (width *default-register-width*) &allow-other-keys)
      decl
    (as-literal "reg [ ")
    (synthesise width :inexpression)
    (as-literal " - 1 : 0 ] ")
    (synthesise n :indeclaration)
    (if (array-value-p v)
	;; synthesise the array constructor
	(synthesise v :indeclaration))
    (as-literal ";")))


(defun synthesise-wire (decl context)
  "Synthesise a wire declaration within a LET block.

The wire has name N and (ignored) initial value V, with the optional
WIDTH defaulting to the system's global width."
  (destructuring-bind  (n v &key (width *default-register-width*) &allow-other-keys)
      decl
    (as-literal "wire ")
    (when (> width 1)
      (as-literal"[ ")
      (synthesise width :inexpression)
      (as-literal " - 1 : 0 ] "))
    (synthesise n :indeclaration)
    (when (array-value-p v)
      ;; synthesise the array constructor
      (synthesise v :indeclaration))
    (as-literal";")))


(defun synthesise-constant (decl context)
  "Synthesise a constant declaration DECL within a LET block.

Constants turn into local parameters."
  (destructuring-bind (n v &key &allow-other-keys)
      decl
    (as-literal "localparam ")
    (synthesise n :inexpression)
    (as-literal " = ")
    (synthesise v :inexpression)
    (as-literal ";")))


(defun synthesise-decl (decl context)
  "Synthesise DECL in CONTEXT."
  (destructuring-bind (n v &key width type as)
      decl
    (case as
      (:constant
       (synthesise-constant decl context))
      (:register
       (synthesise-register decl context))
      (:wire
       (synthesise-wire decl context))
      (t
       (synthesise-register decl context)))))


(defmethod synthesise-sexp ((fun (eql 'let)) args (context (eql :inmodule)))
  (let ((decls (car args))
	(body (cdr args)))

    ;; synthesise the constants and registers
    (as-block decls context :newlines t
			    :process #'synthesise-decl)
    (if (> (length decls) 0)
	(as-literal "" :newline t))

    ;; synthesise the body
    (as-body body :inblock)))


(defmethod synthesise-sexp ((fun (eql 'let)) args (context (eql :inblock)))
  (synthesise-sexp fun args :inmodule))
