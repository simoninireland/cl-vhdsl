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
    (signal 'width-mismatch :expected (bitwidth ty env) :got w)))


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
    (error 'representation-mismatch :expected (list :register :wire :constant) :got rep)))


(defun name-in-decl (decl)
  "Extract the name being defined by DECL."
  (if (listp decl)
      (car decl)
      decl))


(defun typecheck-decl (env decl)
  "Typecheck DECL in ENV, returning ENV extended by DECL."
  (with-current-form decl
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
		    (setq ty (widen-fixed-width ty w))))

	      (extend-environment n `((:initial-value ,v)
				      (:type ,ty)
				      ,(if width
					   `(:width ,width))
				      ;;(:width ,width)
				      (:as ,as))
				  env)))

	  ;; otherwise we have a naked name, so apply the defaults
	  (typecheck-decl env `(,decl 0 :width ,*default-register-width* :as :register)))))


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
  (declare (optimize debug))
  (destructuring-bind (decls &rest body)
      args
    (let* ((rwdecls (mapcar (rcurry #'rewrite-variables-decl rewrite) decls))

	   ;; remove any re-writes referring to shadowed variables
	   (rwnames (mapcar #'name-in-decl rwdecls))
	   (rwrewrite (remove-if (lambda (rw)
				   (member (car rw) rwnames))
				 rewrite))

	   ;; re-write the body with these new re-writes
	   (rwbody (mapcar (rcurry #'rewrite-variables rwrewrite) body)))

      ;; rewrite the form to use re-written decls and the body
      ;; re-written respecting shadowing
      `(let ,rwdecls
	 ,@rwbody))))


(defun expand-macros-key (l kv)
  "Expand macros in the value part of a key-value pair KV to build L."
  (destructuring-bind (k v)
      kv
    (let ((nv (expand-macros v)))
      (append l (list k nv)))))


(defun expand-macros-decl (decl)
  "Expand macros in the value of DECL."
  (if (listp decl)
      ;; full declaration, expand the value and keys
      (destructuring-bind (n v &rest keys)
	  decl
	(let ((newkeys (foldr #'expand-macros-key
			      (adjacent-pairs keys)
			      '())))
	  `(,n ,(expand-macros v) ,@newkeys)))

      ;; naked name, leave it alone
      decl))


(defmethod expand-macros-sexp ((fun (eql 'let)) args)
  (destructuring-bind (decls &rest body)
      args
    (let ((newdecls (mapcar #'expand-macros-decl decls))
	  (newbody (expand-macros (cons 'progn body))))
      `(let ,newdecls
	 ,newbody))))


;; These two functions should only re-write non-constant initial values

;; (defun float-let-blocks-decl (decl)
;;   "Generate the declaration part for DECL."
;;   (destructuring-bind (n v &rest keys)
;;       decl
;;     `(,n ,(if (or (array-value-p v)
;;		  (module-value-p v))
;;	      ;; array initialisers are retained
;;	      v

;;	      ;; non-arrays, check for constant
;;	      (let ((sv (eval-if-static v (empty-environment)))) ; this is too strong
;;		(or sv 0)))
;;	 ,@keys)))


;; (defun float-let-blocks-init (decl)
;;   "Generate a SETF to set DECL an appropriate initial value.

;; This is only required if DECL's value is not an array, not a constant,
;; and where there is a sensible initialiser."
;;   (destructuring-bind (n v &key &allow-other-keys)
;;       decl

;;     (if (and (not (or (array-value-p v)
;;		      (module-value-p v)))
;;	     (null (eval-if-static v (empty-environment))))
;;	`(setq ,n ,v))))


;; (defmethod float-let-blocks-sexp ((fun (eql 'let)) args)
;;   (destructuring-bind (decls &rest body)
;;       args

;;     ;; turn initial values into assignments
;;     (let ((basedecls (remove-nulls (mapcar #'float-let-blocks-decl decls)))
;;	  (assignments (remove-nulls (mapcar #'float-let-blocks-init decls))))
;;       ;; extract the body and decls of the body
;;       (destructuring-bind (newbody newdecls)
;;	  (float-let-blocks `(progn ,@assignments ,@body))

;;	;; return the re-written body and decls
;;	(list newbody
;;	      (append basedecls newdecls))))))

;; Simplified floating, keeping the initial values -- which I think is fine

(defmethod float-let-blocks-sexp ((fun (eql 'let)) args)
  (destructuring-bind (decls &rest body)
      args

    (destructuring-bind (newbody newdecls)
	(float-let-blocks `(progn ,@body))

      ;; return the re-written body and decls
      (list newbody
	    (append decls newdecls)))))


(defun simplify-implied-progn (body)
  "Simplify an implied PROGN represented by BODY.

This removes nested PROGN blocks, singleton PROGNs that can be
repalced by a list of forms, and other simplifications needed
by LET and MODULE forms."
  (foldr (lambda (l arg)
	   (if (and (listp arg)
		    (eql (car arg) 'progn))
	       (append l (cdr arg))
	       (append l (list arg))))
	 body
	 '()))


(defmethod simplify-progn-sexp ((fun (eql 'let)) args)
  (destructuring-bind (decls &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(let ,decls ,@(simplify-implied-progn newbody)))))


(defmethod detect-shadowing-sexp ((fun (eql 'let)) args env)
  (destructuring-bind (decls &rest body)
      args
    (let ((vars (mapcar #'car decls)))
      (dolist (n vars)
	(if (variable-defined-p n env)
	    (error 'duplicate-variable :variable n
				       :hint "Variable shadows a previous definition"))))

    (let ((ext (typecheck-env decls env)))
      (mapc (rcurry #'detect-shadowing ext) body)
      t)))


(defun array-value-p (form)
  "Test whether FORM is an array constructor."
  (and (listp form)
       (eql (car form) 'make-array)))


(defun module-value-p (form)
  "Test whether FORM is a module constructor."
  (and (listp form)
       (eql (car form) 'make-instance)))


(defun synthesise-register (decl context)
  "Synthesise a register declaration within a LET block.

The register has name N and initial value V, with the optional
WIDTH defaulting to the system's global width."
  (destructuring-bind  (n v &key (width *default-register-width*) &allow-other-keys)
      decl
    (as-literal "reg [ ")
    (if (array-value-p v)
	;; synthesise the width as the width of the array element
	(synthesise (array-element-width v) :inexpression)

	;; otherwise use the given width
	(synthesise width :inexpression))
    (as-literal " - 1 : 0 ] ")
    (synthesise n :indeclaration)
    (if (array-value-p v)
	;; synthesise the array bounds and initialisation
	(synthesise-array-init n v)

	;; synthesise the assignment to the initial value
	(progn
	  (as-literal " = ")
	  (synthesise v :inexpression)))
    (as-literal ";")))


(defun synthesise-wire (decl context)
  "Synthesise a wire declaration within a LET block.

The wire has name N and initial value V, with the optional WIDTH
defaulting to the system's global width. If the initial value is zero
the wire is left un-driven."
  (destructuring-bind  (n v &key (width *default-register-width*) &allow-other-keys)
      decl
    (as-literal "wire ")
    (when (> width 1)
      (as-literal"[ ")
      (synthesise width :inexpression)
      (as-literal " - 1 : 0 ] "))
    (synthesise n :indeclaration)
    (if (array-value-p v)
	;; synthesise the array constructor
	(synthesise v :indeclaration)

	;; synthesise the assignment to the initial value
	(if (static-constant-p v nil)
	    (let ((iv (ensure-static v nil)))
	      (unless (= iv 0)
		;; initial value isn't statially zero, synthesise
		(as-literal " = ")
		(synthesise v :inexpression))
	      (as-literal";"))

	    ;; initial value is an expression, synthesise
	    (progn
	      (as-literal " = ")
	      (synthesise v :inexpression)
	      (as-literal";"))))))


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


(defun synthesise-module-instanciation (decl)
  "Synthesise DECL as a module instanciation."
  (destructuring-bind (n v &key &allow-other-keys)
      decl
    (destructuring-bind (modname &rest initargs)
	(cdr v)
      (synthesise-module-instance n modname initargs))))


(defun synthesise-decl (decl context)
  "Synthesise DECL in CONTEXT."
  (destructuring-bind (n v &key width type as)
      decl
    (if (module-value-p v)
	;; instanciating a module
	(synthesise-module-instanciation decl)

	;; otherwise, creating a variable
	(case as
	  (:constant
	   (synthesise-constant decl context))
	  (:register
	   (synthesise-register decl context))
	  (:wire
	   (synthesise-wire decl context))
	  (t
	   (synthesise-register decl context))))))


(defmethod synthesise-sexp ((fun (eql 'let)) args (context (eql :inmodule)))
  (let ((decls (car args))
	(body (cdr args)))

    ;; synthesise the constants and registers
    (as-block-forms decls context :process #'synthesise-decl)
    (if (> (length decls) 0)
	(as-blank-line))

    ;; synthesise the body
    (as-block-forms body :inmodule)))


(defmethod synthesise-sexp ((fun (eql 'let)) args (context (eql :inblock)))
  (synthesise-sexp fun args :inmodule))
