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
  "Extract the name being declared by DECL.

The name is the first element, whether or not DECL is a list."
  (safe-car decl))


(defun typecheck-decl (decl env)
  "Extend ENV with the variable declared in DECL."
  (declare (optimize debug))

  (if (listp decl)
      ;; full declaration
      (destructuring-bind (n v &key
				 width
				 type
				 (as :register))
	  decl

	;; if we have a width, it's a shortcut for unsigned-byte
	(if width
	    (let* ((w (eval-in-static-environment width env))
		   (ty `(unsigned-byte ,w)))
	      (if type
		  ;; if we have a type, it must match
		  (ensure-subtype ty type env)

		  ;; if not, re-assign is to the shortcut
		  (setq type ty))))

	;; initial inferred type
	(let ((ity (if type
		       (expand-type-parameters type env)
		       '(unsigned-byte 1))))

	  ;; typecheck initial value
	  (let ((vty (typecheck v env)))
	    (if type
		;; type provided, ensure it works
		(ensure-subtype vty type env)

		;; no type provided, infer from the value
		(setq ity vty)))

	  (declare-variable n `((:type ,type)
				(:inferred-type ,ity)
				(:as ,as)
				(:initial-value ,v)
				(:type-constraints (,ity)))
			    env)))

      ;; "naked" declaration
      ;; TODO: What is the correct default width? -- 1 means it'll get widened
      ;; as needed, so is perhaps correct?
      (declare-variable decl `((:inferred-type (unsigned-byte 1))
			       (:type-constraints ((unsigned-byte 1)))
			       (:as :register)
			       (:initial-value 0))
			env)))


(defun typecheck-env (decls env)
  "Type-check the declarations DECLS to extend ENV."
  (mapc (rcurry #'typecheck-decl env) decls))


(defun typecheck-constraints (constraints env)
  "Return the type satifying CONSTRAINTS in ENV.

Each constraint is a type needed by some operation in the scope of a
variable. The inferred type is the widest type (least uppoer-bound)
that can accommodate all these constraints, assuming that there is
one."
  (foldr (rcurry #'lub env) constraints nil))


(defun typecheck-infer-decl (n env)
  "Return a full declaration for N in ENV."
  (declare (optimize debug))

  ;; resolve type constraints
  (let* ((constraints (get-environment-property n :type-constraints env))
	 (inferred-type (typecheck-constraints constraints env)))

    ;; check inferred type against any explicit type
    ;; this will signal a problem but not fail the type-checking
    ;; pass, to allow for systems that don't care about precision
    ;; TODO: Should we allow this, or be tighter?
    (if-let ((ty (get-environment-property n :type env :default nil)))
      ;; the type we use is the one supplied
      (setq inferred-type ty)

      ;; no type provided, note that we inferred it
      (signal 'type-inferred :variable n
			     :inferred inferred-type))

    `(,n ,(get-initial-value n env)
	 :type ,inferred-type
	 :as ,(get-representation n env))))


(defun typecheck-infer-decls (decls env)
  "Re-write declarations in DECLS to match ENV."
  (mapcar (rcurry #'typecheck-infer-decl env) (mapcar #'name-in-decl decls)))


(defmethod typecheck-sexp ((fun (eql 'let)) args env)
  (declare (optimize debug))
  (let ((decls (car args))
	(body (cdr args)))
    (let ((ext (add-frame env)))
      (typecheck-env decls ext)

      ;; capture type of the last form
      (let ((ty (mapn (rcurry #'typecheck ext) body)))
	;; update declarations with any inferred types and other properties
	(let ((newdecls (typecheck-infer-decls decls ext)))
	  (setf (car args) newdecls))

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

(defun array-type-p (type)
  "Test whether TYPE is an array type.

There's a slight problem in that the size or shape of the array
type may be RTLisp expressions, whcih don't play well with SUBTYPEP.
To avoid this we do the check manually."
  (or (and (listp type)
	   (eql (car type) 'array))
      (eql type 'array)))


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


(defun synthesise-register (decl env context)
  "Synthesise a register declaration within a LET block.

The register has name N and initial value V, with the optional
WIDTH defaulting to the system's global width."
  (destructuring-bind  (n v &key type &allow-other-keys)
      decl

    (as-literal "reg ")
    (let ((width (if (array-type-p type)
		     ;; width is the width of the element type
		     (bitwidth (cadr type) '())

		     ;; width is of the type itself
		     (bitwidth type '()))))
      (when (or (not (numberp width))
		(> width 1))
	;; we have a width (or a width expression)
	(as-literal"[ ")
	(synthesise width env :inexpression)
	(as-literal " - 1 : 0 ] "))
      (synthesise n env :indeclaration)
      (if (array-value-p v)
	  ;; synthesise the array bounds and initialisation
	  (synthesise-array-init n v env)

	  ;; synthesise the assignment to the initial value
	  (progn
	    (as-literal " = ")
	    (synthesise v env :inexpression)))
      (as-literal ";"))))


(defun synthesise-wire (decl env context)
  "Synthesise a wire declaration within a LET block.

The wire has name N and initial value V, with the optional WIDTH
defaulting to the system's global width. If the initial value is zero
the wire is left un-driven."
  (destructuring-bind  (n v &key type &allow-other-keys)
      decl

    (let ((width (if (array-type-p type)
		     ;; width is the width of the element type
		     (bitwidth (cadr type) '())

		     ;; width is of the type itself
		     (bitwidth type '()))))
      (as-literal "wire ")
      (when (or (not (numberp width))
		(> width 1))
	;; we have a width (or a width expression)
	(as-literal"[ ")
	(synthesise width env :inexpression)
	(as-literal " - 1 : 0 ] "))
      (synthesise n env :indeclaration)
      (if (array-value-p v)
	  ;; synthesise the array constructor
	  (synthesise-array-init n v env)

	  ;; synthesise the assignment to the initial value
	  (if (static-constant-p v nil)
	      (let ((iv (ensure-static v nil)))
		(unless (= iv 0)
		  ;; initial value isn't statially zero, synthesise
		  (as-literal " = ")
		  (synthesise v env :inexpression))
		(as-literal";"))

	      ;; initial value is an expression, synthesise
	      (progn
		(as-literal " = ")
		(synthesise v env :inexpression)
		(as-literal";")))))))


(defun synthesise-constant (decl env context)
  "Synthesise a constant declaration DECL within a LET block.

Constants turn into local parameters."
  (destructuring-bind (n v &key &allow-other-keys)
      decl
    (as-literal "localparam ")
    (synthesise n env :inexpression)
    (as-literal " = ")
    (synthesise v env :inexpression)
    (as-literal ";")))


(defun synthesise-module-instanciation (decl env)
  "Synthesise DECL as a module instanciation."
  (destructuring-bind (n v &key &allow-other-keys)
      decl
    (destructuring-bind (modname &rest initargs)
	(cdr v)
      (synthesise-module-instance n modname initargs env))))


(defun synthesise-decl (decl env context)
  "Synthesise DECL in ENV in CONTEXT."
  (destructuring-bind (n v &key type as)
      decl
    (if (module-value-p v)
	;; instanciating a module
	(synthesise-module-instanciation decl env)

	;; otherwise, creating a variable
	(case as
	  (:constant
	   (synthesise-constant decl env context))
	  (:register
	   (synthesise-register decl env context))
	  (:wire
	   (synthesise-wire decl env context))
	  (t
	   (synthesise-register decl env context))))))


(defmethod synthesise-sexp ((fun (eql 'let)) args env (context (eql :inmodule)))
  (let ((decls (car args))
	(body (cdr args)))

    ;; synthesise the constants and registers
    (as-block-forms decls env context :process #'synthesise-decl)
    (if (> (length decls) 0)
	(as-blank-line))

    ;; synthesise the body
    (as-block-forms body env :inmodule)))


(defmethod synthesise-sexp ((fun (eql 'let)) args env (context (eql :inblock)))
  (synthesise-sexp fun args env :inmodule))
