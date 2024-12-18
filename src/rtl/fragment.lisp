;; The synthesisable fragment of Lisp
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

;; ---------- Enironment ----------

(defparameter *default-register-width* 8
  "Default width for registers.

This is used absent any specfic width specification. It will
generally reflect the word size of the desired circuit, for
example 8, 16, 32, or 64 bits.")


(defvar *identifier-regexp* (create-scanner "^([A-Za-z][A-Za-z0-9_]*)|(_[A-Za-z0-9_]+)$")
  "The regexp scanner used for determining legal variable names.

Legal names contains letters, digits, and underscores, not leading
with a digit. This is a /lot/ more restrictive than Lisp's legal
names, and is intentionally quite conservative with respect to Verilog
identifier names.")


;; There are probably more reserved words
(defvar *reserved-words* '("module" "input" "output" "inout"
			   "always" "posedge" "assign"
			   "parameter" "localparam" "reg" "signed")
  "Reserved words in Verilog.

These are used to prevent poor choices of variable name: they
all match *IDENTIFIER-REGEXP* but still can't be used.")


;; ---------- Type environments ----------

(defun empty-environment ()
  "Return the empty type environment."
  '())


(defun extend-environment (n props env)
  "Return a type environment extending ENV with a binding of N to PROPS.

The new binding will mask any existing bindings of N in ENV.
ENV itself is unchanged.

The properties must include:

   - :type -- the type associated with the name

and may include:

   - :width -- the width in bits assigned to the variable
   - :initial-value -- the initial value assigned to the name
   - :direction -- for arguments to modules, the direction of information
     flow, which should be :in, :out, or :inout
   - :constant -- T if the value is constant and cannot be re-assigned"
  (ensure-legal-identifier n)
  (cons (cons n props) env))


(defun legal-identifier-p (n)
  "Ensure N is a legal identifier.

This uses *IDENTIFIER-REGEXP* for legality while avoiding strings
in *RESERVED-WORDS*."
  (when (symbolp n)
    (setq n (symbol-name n)))
  (and (not (member n *reserved-words* :test #'string-equal))
       (scan *identifier-regexp* n)))


(defun ensure-legal-identifier (n)
  "Ensure that N is a legal identifier.

Signals NOT-SYNTHESISABLE as an error if not."
  (unless (legal-identifier-p n)
    (error 'not-synthesisable :fragment n)))


(defun get-environment-properties (n env)
  "Return the key/value list for N in ENV.

An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (if-let ((kv (assoc n env)))
    (cdr kv)
    (error 'unknown-variable :variable n)))


(defun get-environment-names (env)
  "Return the names in ENV."
  (mapcar #'car env))


(defun get-environment-property (n prop env)
  "Return PROP for N in ENV."
  (cadr (assoc prop (get-environment-properties n env))))


(defun variable-defined (n env)
  "Test whether N is defined in ENV."
  (not (null (assoc n env))))


(defun get-type (n env)
  "Return the type of N in ENV."
  (get-environment-property n :type env))


(defun get-width (n env)
  "Return the width of N in ENV."
  (if-let ((props (get-environment-property n :width env)))
    props

    ;; default value
    *default-register-width*))


(defun get-initial-value (n env)
  "Return the initial value of N in ENV."
  (if-let ((props (get-environment-property n :initial-value env)))
    props

    ;; default value
    0))


(defun get-constant (n env)
  "Return whether N is constant in ENV."
  (if-let ((const (get-environment-property n :constant env)))
    const

    ;; default is not constant
    nil))


(defun get-direction (n env)
  "Return the direction of N in ENV."
  (get-environment-property n :direction env))



;; ---------- Free variables ----------

(defgeneric free-variables (form env)
  (:documentation "Return the free variables in FORM under ENV.

A free variable is one that is not contained under a binding."))


(defmethod free-variables ((form fixnum) env)
  '())


(defmethod free-variables ((form symbol) env)
  (if (variable-defined env)
      '()
      (list n)))


(defmethod free-variables ((form list) env)
  (let ((fun (car form))
	(args (cdr form)))
    (free-variables-sexp fun args env)))


(defgeneric free-variables-sexp (fun args env)
  (:documentation "Return all the variables in FUN called on ARGS that are free in ENV.

The default for this function simple cascades FREE-VARIABLES into
each argument in ARGS, whih is appropriate behaviour for all Lisp
forms except those that alter the lexical environment.")
  (:method (fun args env)
    (apply #'union (mapcar #'free-variables args))))


(defun closed-p (form env)
  "Test whether FORM is closed in ENV, i.e., contains no free variables."
  (null (free-variables form env)))


;; ---------- Constants ----------

(defun resolve-constant (form env)
  "Resolve FORM to a constant.

If FORM is a symbol, its value is looked-up in ENV."
  (if (symbolp form)
      (get-initial-value form env)
      form))


(defgeneric constant-p (form env)
  (:documentation "Test whether FORM is a constant in ENV.

The default for this function simple cascades CONSTANT-P into
each argument in ARGS, whih is appropriate behaviour for all Lisp
forms except those that alter the lexical environment.")
  (:method (form env)
    (let ((fun (car form))
	  (args (cdr form)))
      ;; in Common Lisp AND is a macro not a function, so
      ;; we need to use this idiom instead
      (every #'identity (mapcar (lambda (form) (constant-p form env))
				args)))))


(defmethod constant-p ((form integer) env) t)


(defmethod constant-p ((form symbol) env)
  (or (module-parameter-p form env)
      (get-constant form env)))


;; ---------- Synthesis ----------

;; We use the following positions/roles:
;;
;;   - :statement
;;   - :lvalue
;;   - :rvalue

(defgeneric synthesise (form as &optional str)
  (:documentation "Synthesise the Verilog for FORM to stream STR.

The form may have a specified role of position indicated by AS.
This may be used to specialise synthesis methods according to
syntactic classes and the like."))


(defmethod synthesise ((form list) as &optional str)
  (let ((fun (car form))
	(args (cdr form)))
    (synthesise-sexp fun args as str)))


(defgeneric synthesise-sexp (fun args as str)
  (:documentation "Write the synthesised Verilog of FUN called with ARGS on STR.

The synthesised code may depend on the role or position AS."))


;; ---------- Structure checks ----------

(defgeneric check (form env)
  (:documentation "Return the type of FORM in ENV."))


(defmethod check ((form list) env)
  (let ((fun (car form))
	(args (cdr form)))
    (check-sexp fun args env)))


(defgeneric check-sexp (fun args env)
  (:documentation "Return the type of calling FUN on ARGS in ENV."))


(defun ensure-subtype (form ty env)
  "Ensure F1 is a sub-type of ty in FORM

Signals TYPE-MISMATCH if FORM isn't a sub-type, which may be ignored
in many applications."
  (let ((fty (check form env)))
    (or (typep fty ty)
	(signal 'type-mismatch :expected ty :got fty))))



;; ---------- Lisp forms ----------

;; Literals

(defmethod check ((form integer) env)
  (let ((bits (bitwidth form env)))
    `(fixed-width-integer ,bits)))


(defmethod synthesise ((form integer) as &optional str)
  (format str "~s" form))


;; Symbols

(defmethod check ((form symbol) env)
  (if (module-parameter-p form env)
      ;; a parameter, just substitute its value
      (check (get-initial-value form env) env)

      ;; otherwise, return its type
      (get-type form env)))


(defun check-lvalue (form args env)
  "Check FORM with the given ARGS in ENV as an lvalue."
  (let ((dir (get-direction form env)))
    (when (eql dir :in)
      (error 'direction-mismatch :variable form :expected '(:out :inout) :got dir)))

  (check form env))


(defmethod synthesise ((form symbol) as &optional str)
  (format str "~s" form))


(defmethod synthesise ((form symbol) (as (eql :lvalue)) &optional str)
  (let ((fargs (if args
		   (format nil "[~a]" args)
		   "")))
    (format str "~a~a" form fargs)))


;; Operators

(defmethod check-sexp ((fun (eql '+)) args env)
  (flet ((bitwidth-addition (w v)
	   "The width needed is one plus the widest of V and the current width W".
	   (1+ (max (bitwidth (check v env) env)
		    w))))
    (let ((bits (foldr #'bitwidth-addition args 0)))
      `(fixed-width-integer ,bits))))


(defmethod synthesise-sexp ((fun (eql '+)) args (as (eql :rvalue))  str)
  (format str "(~{~a ~^+ ~})" (mapcar (lambda (form) (synthesise form as)) args)))


(defmethod check-sexp ((fun (eql '-)) args env)
  (flet ((bitwidth-subtraction (w v)
	   "The width needed is one plus the widest of V and the current width W".
	   (1+ (max (bitwidth (check v env) env)
		    w))))
    (let ((bits (foldr #'bitwidth-subtraction args 0)))
      `(fixed-width-integer ,bits))))


(defmethod synthesise-sexp ((fun (eql '-)) args (as (eql :rvalue))  str)
  (format str "(~{~a ~^-0 ~})" (mapcar (lambda (form) (synthesise form as)) args)))


(defmethod check-sexp ((fun (eql '*)) args env)
  (flet ((bitwidth-multiplication (w v)
	   "The width needed is the sum of the widths of V and W".
	   (+ (max (bitwidth (check v env) env)
		   w))))
    (let ((bits (foldr #'bitwidth-multiplication args 0)))
      `(fixed-width-integer ,bits))))


(defmethod synthesise-sexp ((fun (eql '*)) args (as (eql :rvalue))  str)
  (format str "(~{~a ~^* ~})" (mapcar (lambda (form) (synthesise form as)) args)))


;; Verilog provides left and right shift operators; Common Lisp uses ash
;; and switches depending on the sign of the shift (negative for right).
;; That behaviour seems impossible to synthesise without using an extra
;; register, so we provide two different operators instead. (This will
;; change if I can figure out a way to synthesise ash.)

(defmethod check-sexp ((fun (eql '<<)) args env)
  (destructuring-bind (val offset)
      args
    (let ((vty (check val env))
	  (oty (check offset env)))
      vty)))


(defmethod check-sexp ((fun (eql '>>)) args env)
  (destructuring-bind (val offset)
      args
    (let ((vty (check val env))
	  (oty (check offset env)))
      vty)))


(defmethod synthesise-sexp ((fun (eql '<<)) args (as (eql :rvalue)) str)
  (destructuring-bind (val offset)
      args
    (format str "(~a << ~a)"
	    (synthesise val as)
	    (synthesise offset as))))


(defmethod synthesise-sexp ((fun (eql '>>)) args (as (eql :rvalue))  str)
  (destructuring-bind (val offset)
      args
    (format str "(~a >> ~a)"
	    (synthesise val as)
	    (synthesise offset as))))


;; Comparisons

(defun ensure-boolean (form env)
  "Ensure FORM is a boolean (bit) in ENV.

Signals TYPE-MISMATCH if FORM isn't boolean, which may be ignored
in many applications."
  (ensure-subtype form 'bit env))


(defmethod check-sexp ((fun (eql 'logand)) args env)
  (every (lambda (term) (ensure-boolean term env)) args)
  'bit)


(defmethod synthesise-sexp ((fun (eql 'logand)) args as str)
  (format str "(~{~a ~^& ~})" (mapcar (lambda (form) (synthesise form :rvale str)) args)))


;; Control flow

(defmethod check-sexp ((fun (eql 'progn)) args env)
  (car (last (mapcar (lambda (form) (check form env)) args))))


(defmethod synthesise-sexp ((fun (eql 'progn)) args as str)
  (format str "~{~a ~^~&~}" (mapcar (lambda (form) (synthesise form :statement))
				    args)))


(defmethod check-sexp ((fun (eql 'when)) args env)
  (let ((test (car args))
	(body (cdr args)))
    (if (subtypep (check test env) 'bit)
	;; check the body in an implicit PROGN
	(check (cons 'progn args) env)

	(error 'not-synthesisable :fragment (cons fun args)))))


(defmethod synthesise-sexp ((fun (eql 'when)) args (as (eql :statement)) str)
  (let ((test (car args))
	(body (cdr args)))
    (format str "always @(~a) begin~&~a~&end"
	    (synthesise test :rvalue)
	    (synthesise (cons 'progn body) :statement))))


(defmethod check-sexp ((fun (eql 'if)) ergs env)
  (destructuring-bind (condition then &optional else)
      args
    (ensure-boolean condition env)
    (let ((tty (check then env)))
      (if else
	  (let ((ety (check else env)))
	    (ensure-subtype ety tty)))  ;; this may be a bit conservative?
      tty)))


;; statement form
(defmethod synthesise-sexp ((fun (eql 'if)) args (as (eql :statement)) str)
  (destructuring-bind (condition then &optional else)
      args
    (let ((condbranch (synthesise condition :rvalue))
	  (thenbranch (synthesise then :statement))
	  (elsebranch (if else
			  (format nil "else~&~a" (synthesise else :statement))
			  "")))
      (format str "if (~a)~&~a~&~a~&end~&"
	      condbranch
	      thenbranch
	      elsebranch))))


;; conditional expression form
(defmethod synthesise-sexp ((fun (eql 'if)) args (as (eql :rvalue)) str)
  (destructuring-bind (condition then &optional else)
      args
    (let ((condbranch (synthesise condition :rvalue))
	  (thenbranch (synthesise then :rvalue))
	  (elsebranch (if else
			  (format nil ": ~a" (synthesise else :rvalue))
			  "")))
      (format str "~a ? ~a ~a"
	      condbranch
	      thenbranch
	      elsebranch))))


;; LET and LET* bindings

(defun check-decl (decl env checkdecl)
  "Check a declaration DECL in ENV using CHECKDECL and use to extend ENV.

Declarations consist of a list containing a variable name, an
initial value, and an optional list of key-values pairs.

Return ENV extended with the declaration of N."
  (destructuring-bind (n v &key type (width *default-register-width*))
      decl
    (let* ((ty (funcall checkdecl v env))
	   (narrowedty (if width
			   `(fixed-width-integer ,width)
			   ty))
	   (props `((:type ,narrowedty)
		    (:width ,width)
		    (:initial-value ,v))))
      (extend-environment n props env))))


(defun check-let (checkdecl args env)
  "Check LET and LET* declarations using CHECKDECL.

This function factors-out the commonalities between the two Lisp
binding operators, with the difference being provided by the CHECKDECL
function that is folded across the bindings in the car of ARGS to
construct an environment in which to evaluate the cdr of ARGS (the
body of the LET or LET*)."
  (flet ((build-environment (xenv decl)
	   (check-decl decl xenv checkdecl)))
    (let* ((decls (car args))
	   (body (cdr args))
	   (newenv (foldr #'build-environment decls env)))
      (check-sexp 'progn body newenv))))


(defmethod check-sexp ((fun (eql 'let)) args env)
  ;; assignments are all checked in the outer environment
  (check-let (lambda (v xenv) (check v env))
	     args env))


(defmethod check-sexp ((fun (eql 'let*)) args env)
  ;; assignments are checked in the environment formed as each
  ;; binding is established
  (check-let (lambda (v xenv) (check v xenv))
	     args env))


(defmethod free-variables-sexp ((fun (eql 'let)) args env)
  ;; assignments are closed in the in the outer environment
  (fee-variables-let (lambda (v xenv) (free-variables v env))
		     args env))


(defmethod free-variables-sexp ((fun (eql 'let*)) args env)
  ;; assignments are closed in the environment formed as each
  ;; binding is established
  (free-variables-let (lambda (v xenv) (free-variables v xenv))
		      args env))


(defun synthesise-register (n v &key (width *default-register-width*))
  "Synthesise a register declaration within a LET block.

The register has name N and initial value V, with the optional
WIDTH defaulting to the system's global width."
  (format nil "reg [ ~a - 1 : 0] ~a = ~a;"
	  (synthesise width :lvalue)
	  n v))


(defmethod synthesise-sexp ((fun (eql 'let)) args as str)
  (let ((defs (car args))
	(body (cdr args)))
    (format str "~{~a ~^~& ~}"
	    (append  (mapcar (lambda (def)
			       (apply #'synthesise-register def))
			     defs)
		     (list (synthesise (cons 'progn body) :statement))))))


;; Modules

(defclass module ()
  ()
  (:documentation "Base class for modules."))


(defun direction-p (dir)
  "Test DIR is a valid pin direction.

Valid directions are :IN, :OUT, and :INOUT"
  (member dir '(:in :out :inout)))


(defun ensure-direction (dir)
  "Ensure DIR is a valid pin direction.

Signal VALUE-MISMATCH as an error if not."
  (unless (direction-p dir)
    (error 'value-mismatch :expected (list :in :out :inout) :got dir)))


(defun check-arg (decl env)
  "Check a module argument declaration DECL in ENV.

Return an environment extended with the declaration."
  (if (symbolp decl)
      ;; declaration is just a name, fill in defaults
      (let ((props  `((:type (fixed-width-integer ,*default-register-width*))
		      (:width ,*default-register-width*)
		      (:direction :in))))
	(extend-environment decl props env))

      ;; full declaration
      (destructuring-bind (n &key (width *default-register-width*) (direction :in))
	  decl
	(ensure-direction direction)

	(let* ((ty `(fixed-width-integer ,width))
	       (props `((:type ,ty)
			(:width ,width)
			(:direction ,direction))))
	  (extend-environment n props env)))))


(defun check-param (decl env)
  "Check a module parameter declaration DECL in ENV.

Parameters are Lisp values that are interpolated.

Return an environment extended with the declaration."
  (if (symbolp decl)
      ;; declaration is just a name that needs to be provided
      (let ((props '((:type :lisp)
		     (:initial-value nil)
		     (:direction :in)
		     (:constant t))))
	(extend-environment decl props env))

      ;; declaration is a name and a default value
      (destructuring-bind (n v)
	  decl
	(let ((props `((:type :lisp)
		       (:initial-value ,v)
		       (:direction :in)
		       (:constant t))))
	  (extend-environment n props env)))))


(defun split-args-params (decls)
  "Split DECLS into arguments and parameters.

Arguments come first, and can either be bare symbols or lists of name
and properties. Parameters come after any :key marker and consists of
either bare names ot lists of names and values."
  (let ((i (position ':key decls)))
    (if i
	;; parameters (and possibly arguments)
	(list (subseq decls 0 i)
	      (subseq decls (1+ i)))

	;; just arguments
	(list decls nil))))


(defun check-module-args (decls env)
  "Check a module's argument DECLS in ENV.

Return an environment containing these declarations."
  (flet ((build-environment-args (xenv decl)
	   (check-arg decl xenv)))
    (foldr #'build-environment-args decls env)))  ;; empty initial env always?


(defun check-module-params(decls env)
  "Check a module's parameter DECLS in ENV.

Return an environment containing these declarations."
  (flet ((build-environment-params (xenv decl)
	   (check-param decl xenv)))
    (foldr #'build-environment-params decls env)))



(defun check-module-decls (decls env)
  "Check a module's argument and parameter DECLS in ENV.

Return an environment containing these declarations."
  (destructuring-bind (args params)
      (split-args-params decls)
    (let* ((argenv (check-module-args args env)))
      (check-module-params params argenv))))


(defmethod check-sexp ((fun (eql 'module)) args env)
  (destructuring-bind (modname decls &rest body)
      args
    (let ((newenv (check-module-decls decls env)))
      (check (cons 'progn body) newenv))))


(defun module-parameter-p (n env)
  "Test whether N is a module paramater in ENV.

Module parameters have type :lisp to indicate that they should
be interpolated."
  (eql (get-type n env) :lisp))


(defun synthesise-arg (n env)
  "Return the code for argument N from ENV."
  (if (module-parameter-p n env)
      ;; arg is a parameter
      (format nil "parameter ~a = ~a"
	      n
	      (get-initial-value n env))

      ;; arg is an argument
      (let ((dir (get-direction n env))
	    (width (get-width n env)))
	(format nil "~a ~a~a"
		(case dir
		  (:in    "input")
		  (:out   "output")
		  (:inout "inout"))
		(if (= width 1)
		    ""
		    (format nil "[ ~a - 1 : 0 ] " width))
		n))))


(defmethod synthesise-sexp ((fun (eql 'module)) args (as (eql :statement)) str)
  (destructuring-bind (modname decls &rest body)
      args
    (destructuring-bind (args params)
	(split-args-params decls)
      (let* ((argenv (check-module-args args (empty-environment)))
	     (argnames (reverse (get-environment-names argenv)))
	     (argdefs (mapcar (lambda (n) (synthesise-arg n argenv))
			      argnames))
	     (paramenv (check-module-params params (empty-environment)))
	     (paramnames (reverse (get-environment-names paramenv)))
	     (paramdefs (mapcar (lambda (n) (synthesise-arg n paramenv))
				paramnames)))
	 (format str "module ~a~a(~{~a ~^,~});~&~a~&endmodule // ~a"
	      modname
	      (if params
		  (format nil " # (~{~a ~^, ~})" paramdefs)
		  "")
	      argdefs
	      (synthesise (cons 'progn body) :statement)
	      modname)))))


;; Assignments

(defun ensure-subtype (ty1 ty2)
  "Ensure TY1 is a sub-type of TY2.

Signals TYPE-MISMATCH is the types are not compatible. This
can be ignored for systems not concerned with loss of precision."
  (if (not (subtypep ty1 ty2))
      (signal 'type-mismatch :expected ty2 :got ty1)))


(defun writeable-p (n env)
  "Test whether N is writeable in ENV.

To be writeable a variable must be a register, not a constant,
not a Lisp-typed value, and not an input argument."
  (and (not (get-constant n env))
       (not (eql (get-type n env) :lisp))
       (not (eql (get-direction n env) :in))))


(defun ensure-writeable (n env)
  "Ensure N is writeable in ENV.

Signals NOT-SYNTHESISABLE is an attempt is made to update a
constant or an input parameter."
  (unless (writeable-p n env)
    (error 'not-synthesisable :fragment `(setf ,n))))


(defmethod check-sexp ((fun (eql 'setf)) args env)
  (destructuring-bind (var val &key (sync nil))
      args
    (ensure-writeable var env)

    (let ((nty (check var env))
	  (vty (check val env)))
      (ensure-subtype vty nty)
      vty)))


(defmethod synthesise-sexp ((fun (eql 'setf)) args (as (eql :statement)) str)
  (destructuring-bind (var val &key (sync nil))
      args
    (format str "~a ~a ~a;"
	    (synthesise var :lvalue)
	    (if sync "=" "<=")
	    (synthesise val :rvalue))))


;; Triggers

(defmethod check-sexp ((fun (eql 'posedge)) args env)
  (destructuring-bind (pin)
      args
    (let ((ty (check pin env)))
      (if (subtypep ty '(fixed-width-integer 1))
	  '(fixed-width-integer 1)

	  (error 'type-mismatch :expected '(fixed-width-integer 1) :got ty)))))


(defmethod synthesise-sexp ((fun (eql 'posedge)) args (as (eql :rvalue))  str)
  (destructuring-bind (pin)
      args
    (format str "posedge ~a" (synthesise pin :rvalue))))


;; Wiring

(defmethod check-sexp ((fun (eql 'wire)) args env)
  (destructuring-bind (from to)
      args
    ()
    )
  )


(defmethod synthesise-sexp ((fun (eql 'wire)) args as str)
  (destructuring-bind (from to)
      args
    (format str "assign ~a = ~a"
	    (synthesise from :lvalue nil)
	    (synthesise to :rvalue nil))))
