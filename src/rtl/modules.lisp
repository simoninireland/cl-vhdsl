;; Top-level modules
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


;; ---------- Module interfaces ----------

(deftype module-interface (&optional (parameters ()) (arguments ()))
  "The type of module interfaces.

Interfaces consist of two lists, of parameters and arguments. At present
we don't define any sub-typing relationships."
  t)


(defun module-interface-parameters (ty)
  "Return the list of parameter decls to modfule interface TY."
  (cadr ty))


(defun module-interface-arguments (ty)
  "Return the list of argument decls to modfule interface TY."
  (caddr ty))


(defmethod expand-type-parameters-type ((ty (eql 'module-interface)) args env)
  `(module-interface ,@args))


;; ---------- Module late initialisation ----------

(defvar *module-late-initialisation* nil
  "List of functions that synthesise late intiialisation in modules.

This is primarily for initialising arrays representing ROMs, where
the conents are loaded from a file.")


(defun clear-module-late-initialisation ()
  "Clear the late initialisation functions."
  (setq *module-late-initialisation* nil))


(defun module-late-initialisation-p ()
  "Test whether there is late initialisation to be done."
  (not (null *module-late-initialisation*)))


(defun add-module-late-initialisation (f)
  "Add F to be run to synthesise late initialisation for the current module.

Each function should take no arguments, and run the necessary synthesis code."
  (appendf *module-late-initialisation* (list f)))


(defun run-module-late-initialisation ()
  "Run all the late synthesis functions.

The late intiialisations are cleared once they have been run."
  (dolist (f *module-late-initialisation*)
    (funcall f))

  (clear-module-late-initialisation))


;; ---------- Modules ----------

(deftype direction ()
  "The type of dataflow directions.

Valid directions are :IN, :OUT, and :INOUT, and are seen from the
perspective of inside the module."
  '(member :in :out :inout))


(defun direction-p (dir)
  "Test DIR is a valid pin direction."
  (typep dir 'direction))


(defun ensure-direction (dir)
  "Ensure DIR is a valid pin direction.

Signal VALUE-MISMATCH as an error if not."
  (unless (direction-p dir)
    (error 'value-mismatch :expected (list :in :out :inout) :got dir)))


(defun split-args-params (decls)
  "Split DECLS into arguments and parameters.

Arguments come first, and can either be bare symbols or lists of name
and properties. Parameters come after any :key marker and consists of
either bare names ot lists of names and values."
  (let ((i (position '&key decls)))
    (if i
	;; parameters (and possibly arguments)
	(list (subseq decls 0 i)
	      (subseq decls (1+ i)))

	;; just arguments
	(list decls nil))))


(defun module-parameter-p (n env)
  "Test whether N is a module paramater in ENV.

Module parameters have type :lisp to indicate that they should
be interpolated."
  (eql (get-type n env) :lisp))


(defun typecheck-module-param (env decl)
  "Type-check a module parameter declaration DECL in ENV.

The value of the parameter, if provided, is evaluated as a Lisp
expression in the current Lisp environment, *not* in RTLisp's
environment. This means that parameter values can't be defined in terms
of other parameter values."
  (declare (optimize debug))
  (if (listp decl)
      ;; standard declaration
      (destructuring-bind (n v)
	  decl

	(let ((val (eval v)))
	  (declare-variable n `((:initial-value ,val)
				(:as :parameter))
			    env)))

      ;; naked paramater
      (declare-variable decl `((:initial-value 0)
			       (:as :parameter))
			env))

  env)


(defun typecheck-module-params (decls env)
  "Type-check the module parameter declarations DECLS to extend ENV."
  (foldr #'typecheck-module-param decls env))


(defun typecheck-module-arg (env decl)
  "Type-check a module argument declaration DECL in ENV."
  (destructuring-bind (n &key
			   type
			   width
			   (direction :in)
			   (as :wire))
      decl
    (ensure-direction direction)

    ;; if we have a width, it's a shortcut for unsigned-byte
    (if width
	(let* ((w (eval-in-static-environment width env))
	       (ty `(unsigned-byte ,w)))
	  (if type
	      ;; if we have a type, it must match
	      (ensure-subtype ty type env)

	      ;; if not, re-assign is to the shortcut
	      (setq type ty))))

    (declare-variable n `((:type ,type)
			  (:direction ,direction))
		      env)))


(defun env-from-module-decls (args params)
  "Create an environment from PARAMS and ARGS declarations of a module interface."
  (let* ((env (add-frame (empty-environment)))
	 (extparams (typecheck-module-params params env)))
    (typecheck-module-args args extparams)))


(defun typecheck-module-args (decls env)
  "Type-check the module argument declarations DECLS to extend ENV."
  (foldr #'typecheck-module-arg decls env))


(defun make-module-environment (decls)
  "Return an environment built from the DECLS of a module."
  (destructuring-bind (modargs modparams)
      (split-args-params decls)

    ;; catch modules with no wires or registers
    (unless (> (length modargs) 0)
      (error 'not-synthesisable :hint "Module must import at least one wire or register"))

    ;; create the environment
    (env-from-module-decls modargs modparams)))


(defun make-module-interface-type (decls)
  "Return the module interface type of the DECLS of a module."
  (destructuring-bind (modargs modparams)
      (split-args-params decls)
    `(module-interface ,modparams ,modargs)))


(defmethod typecheck-sexp ((fun (eql 'module)) args env)
  (destructuring-bind (modname decls &rest body)
      args

    (let ((ext (make-module-environment decls)))
      ;; typecheck the body of the module in its environment
      (typecheck (cons 'progn body) ext)

      ;; return the interface type
      (make-module-interface-type decls))))


(defmethod float-let-blocks-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (destructuring-bind (newbody newdecls)
	(float-let-blocks `(progn ,@body))
      (list
       `(module ,modname ,decls
		,(if newdecls
		     ;; declare the floated declarations
		     `(let ,newdecls
			,newbody)

		     ;; no declarations, just return the new body
		     newbody))
       '()))))


(defmethod simplify-progn-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(module ,modname ,decls ,@(simplify-implied-progn newbody)))))


(defmethod detect-shadowing-sexp ((fun (eql 'module)) args env)
  (destructuring-bind (modname decls &rest body)
      args
    (mapc (rcurry #'detect-shadowing env) body)
    t))


(defun synthesise-param (decl)
  "Return the code for parameter DECL."
  (if (listp decl)
      ;; parameter with an initial value
      (destructuring-bind (n v)
	  decl
	(as-literal "parameter ")
	(synthesise n :indeclaration)
	(as-literal " = ")
	(synthesise v :inexpression))

      ;; naked parameter
      (progn
	(as-literal "parameter ")
	(synthesise decl :indeclaration))))


(defun synthesise-arg (decl)
  "Return the code for argument DECL."
  (declare (optimize debug))

  (destructuring-bind (n &key direction type (as :wire))
      decl
    (let ((width (bitwidth type (empty-environment))))
      (as-literal (format nil "~a ~a"
			  (case direction
			    (:in    "input")
			    (:out   "output")
			    (:inout "inout"))
			  (if (and (integerp width)
				   (= width 1))
			      ""
			      (format nil "[ ~(~a~) - 1 : 0 ] " width))))
      (synthesise n :indeclaration))))


(defmethod synthesise-sexp ((fun (eql 'module)) args (context (eql :toplevel)))
  (destructuring-bind (modname decls &rest body)
      args
    (as-literal "module ")
    (synthesise modname :inexpression)

    (destructuring-bind (args params)
	(split-args-params decls)
      ;; parameters
      (if params
	  (as-argument-list params :indeclaration :before " #(" :after ")"
						  :sep ", "
						  :process (lambda (form context)
							     (synthesise-param form))))

      ;; arguments
      (as-argument-list args :indeclaration :before "(" :after ");"
					    :sep ", "
					    :process (lambda (form context)
						       (synthesise-arg form))))
    (as-blank-line)

    ;; body
    (with-indentation
	(as-block-forms body :inmodule))

    ;; late initialisationn (if any)
    (when (module-late-initialisation-p)
	(as-blank-line)
	(as-literal "initial begin" :newline t)
	(with-indentation
	  (run-module-late-initialisation))
	(as-literal "end" :newline t))

    (as-blank-line)
    (as-literal "endmodule // ")
    (as-literal (format nil "~(~a~)" modname) :newline t)
    (as-blank-line)))


;; ---------- Module instanciation ----------

(defun get-argument-or-parameter (n decls)
  "Retrieve argument or parameter N from DECLS.

N should be a string, which is matched against DECLS by symbol name."
  (if-let ((v (assoc n decls :key #'symbol-name :test #'string-equal)))
    (cdr v)))


(defun argument-for-module-interface-p (a intf)
  "Test whether A is an argument of INTF."
  (not (null (get-argument-or-parameter a (module-interface-arguments intf)))))


(defun parameter-for-module-interface-p (a intf)
  "Test whether A is a parameter of INTF."
  (not (null (get-argument-or-parameter a (module-interface-parameters intf)))))


(defun module-arguments-match-interface-p (intf modargs)
  "Test that MODARGS conform to INTF.

All the arguments in INTF must be provided in MODARGS. All the
MODARGS must refer to an argument or a parameter of INTF."
  (and
   ;; every module argument is provided
   (every (lambda (arg)
	    (member arg modargs :test #'string-equal))
	  (mapcar #'symbol-name (mapcar #'car (module-interface-arguments intf))))

   ;; every modarg is either a module argument or parameter
   (every (lambda (arg)
	    (or (argument-for-module-interface-p arg intf)
		(parameter-for-module-interface-p arg intf)))
	  modargs)))


(defun ensure-module-arguments-match-interface (modname intf modargs)
  "Ensure that MODARGS or module MODNAME match INTF.

This is tested according to MODULE-ARGUMENTS-MATH-INTERFACE-P
and causes a NOT-IMPORTABLE error if not."
  (unless (module-arguments-match-interface-p intf modargs)
    (error 'not-importable :module modname
			   :hint "Check that arguments in the import match the module type")))


(defun keys-to-arguments (modname modargs)
  "Extract the keys from MODARGS when importing MODNAME."
  (labels ((every-argument (l)
	     "Return a list containing every argument element of L."
	     (cond ((null l)
		    '())
		   (t
		    (cons (car l)
			  (every-argument (cddr l)))))))

    (unless (evenp (length modargs))
      (error 'not-importable :module modname
			     :hint "Uneven number of module arguments"))

    (mapcar #'symbol-name (every-argument modargs))))


(defun env-from-module-interface (intf)
  "Return an environment corresponding to INTF."
  (env-from-module-decls (module-interface-arguments intf) (module-interface-parameters intf)))


(defmethod typecheck-sexp ((fun (eql 'make-instance)) args env)
  (destructuring-bind (modname &rest initargs)
      args

    ;; skip over leading quote of module name,
    ;; for compatability with Common Lisp usage
    (unquote modname)

    (let ((intf (get-module-interface modname))
	  (modargs (keys-to-arguments modname initargs)))
      ;; ensure we have all the arguments we need
      (ensure-module-arguments-match-interface modname intf modargs)

      ;; typecheck the provided arguments against the interface
      (let ((modenv (env-from-module-interface intf))
	    (initargs-plist (plist-alist initargs)))
	(dolist (arg modargs)
	  (let ((v (cdr (assoc arg initargs-plist
			       :key #'symbol-name
			       :test #'string-equal))))
	    (cond ((argument-for-module-interface-p arg intf)
		   (let ((tyval (typecheck v env))
			 (tyarg (get-type arg modenv)))
		     (ensure-subtype tyval tyarg env)))
		  ((parameter-for-module-interface-p arg intf)
		   (let ((tyval (typecheck (eval-in-static-environment v env) env))
			 (tyarg (get-type arg modenv)))
		     (ensure-subtype tyval tyarg env))))))

	intf))))


(defmethod rewrite-variables-sexp ((fun (eql 'make-instance)) args rewrites)
  (labels ((rewrite-args (l)
	     (if (null l)
		 l
		 (append (list (car l)
			     (rewrite-variables (cadr l) rewrites))
			 (rewrite-args (cddr l))))))

    (destructuring-bind (modname &rest initargs)
	args
      `(,fun ,modname ,@(rewrite-args initargs)))))


(defun synthesise-param-binding (decl context args)
  "Synthesise the binding of parameter DECLs from ARGS."
  (destructuring-bind (n v)
      decl
    (if-let ((m (assoc n args
		       :key #'symbol-name
		       :test #'string-equal)))
      (let ((v (cdr m)))
	(as-literal ".")
	(synthesise n :indeclaration)
	(as-literal "(")
	(synthesise v :inexpression)
	(as-literal ")")))))


(defun synthesise-arg-binding (decl context args)
  "Synthesise the binding of DECL from ARGS."
  (destructuring-bind (n &key &allow-other-keys)
      decl
    (let ((v (cdr (assoc n args
			 :key #'symbol-name
			 :test #'string-equal))))
      (as-literal ".")
      (synthesise n :indeclaration)
      (as-literal "(")
      (synthesise v :inexpression)
      (as-literal ")"))))


(defun synthesise-module-instance-params (initargs intf)
  "Synthesise the parameter bindings INITARGS of INTF."
  (declare (optimize debug))
  (let ((paramdecls (module-interface-parameters intf)))
    ;; extract all the parameters actually specified
    (let* ((paramkeys (alist-keys paramdecls))
	   (args-alist (plist-alist initargs) )
	   (argkeys (alist-keys args-alist))
	   (paramsgiven (intersection paramkeys argkeys
				      :key #'symbol-name :test #'string-equal))
	   (paramdeclsgiven (remove-if (lambda (ndecl)
					 (not (member (symbol-name (car ndecl)) paramsgiven
						      :key #'symbol-name :test #'string-equal)))
				       paramdecls)))

      (if paramdeclsgiven
	  (progn
	    (as-literal " ")
	    (as-argument-list paramdeclsgiven :indeclaration
			      :before "#(" :after ")"
			      :process (rcurry #'synthesise-param-binding args-alist)))

	  (as-literal " ")))))


(defun synthesise-module-instance-args (initargs intf)
  "Synthesise the argument bindings INITARGS of INTF."
  (let ((argdecls (module-interface-arguments intf)))
    (as-argument-list argdecls :indeclaration
		      :before "(" :after ");"
		      :process (rcurry #'synthesise-arg-binding (plist-alist initargs)))))


(defun synthesise-module-instance (n modname initargs)
  "Synthesise the module instanciation MODNAME with gibven INITARGS assigning the instance to N."
  ;; skip over leading quote of module name,
  ;; for compatability with Common Lisp usage
  (unquote modname)

  (let ((intf (get-module-interface modname)))
    (synthesise modname :indeclaration)
    (synthesise-module-instance-params initargs intf)
    (synthesise n :indeclaration)
    (as-literal " ")
    (synthesise-module-instance-args initargs intf)))


(defmethod synthesise-sexp ((fun (eql 'make-instance)) args (context (eql :inexpression)))
  (labels ((args-to-alist (plist)
	     "Convert a plist of arguments to an alist, respecting sub-lists. "
	     (if (null plist)
		 plist
		 (cons (list (car plist) (cadr plist))
		       (args-to-alist (cddr plist))))))

    (destructuring-bind (modname &rest initargs)
	args

      ;; skip over leading quote of module name,
      ;; for compatability with Common Lisp usage
      (unquote modname)

      (let ((intf (get-module-interface modname))
	    (modargs (keys-to-arguments modname initargs)))

	;; arguments
	(as-argument-list (arguments intf) :indeclaration
			  :before "(" :after ");"
			  :process (rcurry #'synthesise-arg-binding (args-to-alist initargs)))))))

(defmethod synthesise-sexp ((fun (eql 'make-instance)) args (context (eql :inblock)))
  (synthesise-sexp fun args :inmodule))
