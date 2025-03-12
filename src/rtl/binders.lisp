;; Variable declarations and bindings
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
  (>= (eval-in-static-environment w env)
      (eval-in-static-environment (bitwidth ty env) env)))


(defun ensure-width-can-store (w ty env)
  "Ensure that W bits can accommodate the values of TY in ENV."
  (unless (width-can-store-p w ty env)
    (signal 'width-mismatch :expected (bitwidth ty env) :got w)))


;; ---------- Representationa ----------

(deftype representation ()
  "The type of variable representations.

Valid representations in LET forms are :REGISTER, :WIRE, or :CONSTANT.
(:PARAMETER is also valid from MODULE forms.)"
  '(member :register :wire :constant))


(defun representation-p (rep)
  "Test REP is a valid variable representation."
  (typep rep 'representation))


(defun ensure-representation (rep)
  "Ensure REP is a valid variable representation.

Signal REPRESENTATION-MISMATCH as an error if not."
  (unless (representation-p rep)
    (error 'representation-mismatch :expected (list :register :wire :constant) :got rep)))


;; ---------- Typechecking ----------

(defun name-in-decl (decl)
  "Extract the name being defined by DECL."
  (if (listp decl)
      (car decl)
      decl))


(defun typecheck-decl (decl env)
  "Extend ENV with the declarations in DECLS."
  (declare (optmize debug))
  (with-current-form decl
    (if (listp decl)
	;; full declaration
	(destructuring-bind (n v &key width type (as :register))
	    decl
	  (ensure-representation as)

	  ;; a decl may come with zero, one, or both of a type, and width
	  (let ((tyv (typecheck-form v env))
		inferred-type
		inferred-width)
	    (cond ((special-value-p v)
		   ;; special values don't have widths
		   (setq inferred-type tyv))

		  ((and (null type)
			(null width))
		   ;; no explicit settings, start from initial value
		   (setq inferred-type tyv)
		   (setq inferred-width (bitwidth tyv env)))

		  ((null type)
		   ;; evaluate the width
		   (setq inferred-width (eval-in-static-environment width env))

		   ;; check that the initial value fits in this width
		   (ensure-width-can-store inferred-width tyv env)

		   ;; set the type to this width
		   (setq inferred-type `(fixed-width-unsigned ,inferred-width)))

		  ((null width)
		   ;; evaluate the type
		   (setq inferred-type (expand-type-parameters type env))

		   ;; check that the initial value fits in this type
		   (ensure-subtype tyv inferred-type)

		   ;; set the width to match this type
		   (setq inferred-width (bitwidth inferred-type env)))

		  (t
		   ;; evaluate both
		   (setq inferred-width (eval-in-static-environment width env))
		   (setq inferred-type (expand-type-parameters tyv env))

		   ;; make sure the type and width are consistent
		   (unless (= inferred-width (bitwidth inferred-type env))
		     (error 'width-mismatch :expected inferred-width
					    :got inferred-type
					    :hint "Explicit type and width must be consistent"))))

	    (declare-variable n `((:initial-value ,v)
				  (:type ,inferred-type)
				  (:width ,inferred-width)
				  (:as ,as))
			      env)))

	;; otherwise we have a naked name, so apply the defaults
	(typecheck-decl `(,decl 0 :width ,*default-register-width* :as :register) env))))


(defun typecheck-env (decls env)
  "Type-check the declarations DECLS to extend ENV."
  (mapc (rcurry #'typecheck-decl env) decls))


(defun typecheck-infer-decl (decl env)
  "Re-write declarations in DECL to match ENV.

ENV may include :TYPE and :WIDTH assignments, which are used to
update the values in DECL as long as they're consistent."
  (with-current-form decl
    (if (listp decl)
	;; full declaration
	(destructuring-bind (n v &key width type (as :register))
	    decl
	  (ensure-representation as)

	  ;; a decl may come with zero, one, or both of a type, and width
	  (let ((inferred-type (get-type n env))
		(inferred-width (get-width n env))
		assigned-type
		assigned-width)
	    (cond ((special-value-p v)
		   ;; special values don't have widths, do nothing
		   t)

		  ((and (null type)
			(null width))
		   ;; no explicit settings, set to the inferred values
		   (setq assigned-type inferred-type)
		   (setq assigned-width inferred-width))

		  ((null type)
		   ;; check explicit width is consistent
		   (let ((w (eval-in-static-environment width env)))
		     (unless (>= w inferred-width)
		       (signal 'width-mismatch :expected inferred-width
					       :got w
					       :hint "Make sure assigned width is wide enough for the values assigned to it"))

		     ;; set the type to the assigned width
		     (setq inferred-type `(fixed-width-unsigned ,w))))

		  ((null width)
		   ;; check explicit type is consistent
		   (let ((tyv (expand-type-parameters (typecheck-form v env) env)))
		     (unless (subtypep inferred-type tyv)
		       (signal 'type-mismatch :expected inferred-type
					      :got tyv
					      :hint "Make sure assigned type is widthe nough for the values assigned to it"))

		     ;; set the width to match this type
		     (setq inferred-width (bitwidth tyv env))))

		  (t
		   (let ((w (eval-in-static-environment width env))
			 (tyv (expand-type-parameters (typecheck-form v env) env)))
		     (unless (>= w inferred-width)
		       (signal 'width-mismatch :expected inferred-width
					       :got w
					       :hint "Make sure assigned width is wide enough for the values assigned to it"))
		     (unless (subtypep inferred-type tyv)
		       (signal 'type-mismatch :expected inferred-type
					      :got tyv
					      :hint "Make sure assigned type is widthe nough for the values assigned to it")))))

	    ;; update the declaration if it's changed
	    (when assigned-type
	      (setf (cdr (last decl 2)) (list (car (last decl)) :type assigned-type))
	      (signal 'type-inferred :variable n
				     :inferred assigned-type))
	    (when assigned-width
	      (setf (cdr (last decl 2)) (list (car (last decl)) :width assigned-width))
	      (signal 'width-inferred :variable n
				      :inferred assigned-width))))

	;; otherwise we have a naked name, so apply the defaults
	(typecheck-infer-decl `(,decl 0 :width ,*default-register-width* :as :register) env))))


(defun typecheck-infer-decls (decls env)
  "Re-write declarations in DECLS to match ENV."
  (mapcar (rcurry #'typecheck-infer-decl env) decls))


(defmethod typecheck-sexp ((fun (eql 'let)) args env)
  (declare (optimize debug))
  (let ((decls (car args))
	(body (cdr args)))
    (let ((ext (add-frame env)))
      (typecheck-env decls ext)

      ;; capture type of the last form
      (let ((ty (mapn (rcurry #'typecheck-form ext) body)))
	;; resolve any inferred widths and types
	(typecheck-infer-decls decls ext)

	;; return the type
	ty))))


;; ---------- Variable re-writing ----------

(defun rewrite-variables-keys (kvs rewrite)
  "Rewrite variables in the values of the key/value pairs KVS using REWRITE."
  (flet ((rewrite-key-value (l kv)
	   (append l (list (car kv) (rewrite-variables (cadr kv) rewrite)))))
    (foldr #'rewrite-key-value (adjacent-pairs kvs) '())))


(defun rewrite-variables-decl (decl rewrite)
  "Re-write the values of DECL using REWRITE."
  (destructuring-bind (n v &rest keys)
      decl
    (if keys
	`(,n ,(rewrite-variables v rewrite) ,(rewrite-variables-keys keys rewrite))

	;; no keys to add
	`(,n ,(rewrite-variables v rewrite)))))


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


;; ---------- Macro expansion ----------

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


;; ---------- Floating and simplification ----------

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


;; ---------- Shadowing ----------

(defmethod detect-shadowing-sexp ((fun (eql 'let)) args env)
  (destructuring-bind (decls &rest body)
      args
    (let ((vars (mapcar #'car decls)))
      (dolist (n vars)
	(if (variable-declared-p n env)
	    (error 'duplicate-variable :variable n
				       :hint "Variable shadows a previous definition"))))

    (let ((ext (add-frame env)))
      (typecheck-env decls ext)

      (mapc (rcurry #'detect-shadowing ext) body)
      t)))


;; ---------- Synthesis ----------

(defun array-value-p (form)
  "Test whether FORM is an array constructor."
  (and (listp form)
       (eql (car form) 'make-array)))


(defun module-value-p (form)
  "Test whether FORM is a module constructor."
  (and (listp form)
       (eql (car form) 'make-instance)))


(defun special-value-p (form)
  "Test wether FORM denotes a special value.

Special values are things like array constructors and mdule instanciations."
  (or (array-value-p form)
      (module-value-p form)))


(defun normal-value-p (form)
  "Test whether FORM denotes a ormal value.

Normal values are those that are not special in the sense of
SPECIAL-VALUE-P. Specifically, normal values have a bit-width."
  (not (special-value-p form)))


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
	(synthesise-array-init n v)

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
