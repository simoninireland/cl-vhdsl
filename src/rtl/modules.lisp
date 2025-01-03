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


(deftype module (&optional params args)
  "The type of modules.

PARAMS and ARGS are lists of parameter and argument declarations
respectively.")


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
  (let ((i (position ':key decls)))
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
expression in the current Lisp environment. This means that parameter
values can't be defined in terms of other parameter values."
  (if (listp decl)
      ;; standard declaration
      (destructuring-bind (n v)
	  decl
	(let ((ty (typecheck v env))
	      (val (eval v)))        ;; this might need to change to
				     ;; make sure the environment makes
				     ;; sense
	  (extend-environment n `((:initial-value ,val)
				  (:type ,ty)
				  (:parameter t))
			      env)))

      ;; naked paramater
      (extend-environment decl `((:initial-value 0)
				 (:type (fixed-width-unsigned 1))
				 (:parameter t))
			  env)))


(defun typecheck-module-params (decls env)
  "Type-check the module parameter declarations DECLS to extend ENV."
  (foldr #'typecheck-module-param decls env))


(defun typecheck-module-arg (env decl)
  "Type-check a module argument declaration DECL in ENV."
  (destructuring-bind (n &key (width 1) type (direction :in))
      decl
    (ensure-direction direction)

    (let ((w (typecheck width env)))
      (if type
	   ;; make sure the argument is wide enough to accommodate
	   ;; the type
	   (ensure-width-can-store w type env)
	   (setq width (bitwidth w env)))

	  ;; no type, use width for a default unsigned
	  (setq type `(fixed-width-unsigned ,(bitwidth w env))))

    (extend-environment n `((:type ,type)
			    (:width ,width)
			    (:direction ,direction))
			env)))


(defun typecheck-module-args (decls env)
  "Type-check the module argument declarations DECLS to extend ENV."
  (foldr #'typecheck-module-arg decls env))


(defmethod typecheck-sexp ((fun (eql 'module)) args env)
  (destructuring-bind (modname decls &rest body)
      args
    (destructuring-bind (args params)
	(split-args-params decls)
      (let* ((extparams (typecheck-module-params params env))
	     (ext (typecheck-module-args args extparams)))
	(typecheck (cons 'progn body) ext)


	`(module ,params ,args)))))


(defmethod float-let-blocks-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    (destructuring-bind (newbody newdecls)
	(float-let-blocks `(progn ,@body))
      `((module ,modname ,decls
		(let ,newdecls
		  ,@ (cdr newbody)))
	()))))


(defmethod simplify-progn-sexp ((fun (eql 'module)) args)
  (destructuring-bind (modname decls &rest body)
      args
    `(module ,modname ,decls ,@ (mapcar #'simplify-progn body))))


(defun synthesise-param (decl)
  "Return the code for parameter DECL."
  (if (listp decl)
      ;; parameter with an initial value
      (destructuring-bind (n v)
	  decl
	(format *synthesis-stream* "parameter ~(~a~) = ~a"
		n
		v))

      ;; naked parameter
      (format *synthesis-stream* "parameter ~(~a~)"
	      decl)))


(defun synthesise-arg (decl)
  "Return the code for argument DECL."
  (destructuring-bind (n &key direction width)
      decl
    (format *synthesis-stream* "~a ~a~(~a~)"
	    (case direction
	      (:in    "input")
	      (:out   "output")
	      (:inout "inout"))
	    (if (and (integerp width)
		     (= width 1))
		""
		(format nil "[ ~(~a~) - 1 : 0 ] " width))
	    n)))


(defmethod synthesise-sexp ((fun (eql 'module)) args (as (eql :toplevel)))
  (destructuring-bind (modname decls &rest body)
      args
    (destructuring-bind (args params)
	(split-args-params decls)

      (format *synthesis-stream* "module ")
      (synthesise modname :rvalue)

      ;; parameters
      (if params
	  (as-list params :argument :before " #(" :after ")"
				    :indented t :newlines t
				    :process (lambda (form as)
					       (synthesise-param form))))

      ;; arguments
      (as-list args :argument :before "(" :after ");"
			      :indented t :newlines t
			      :process (lambda (form as)
					 (synthesise-arg form))))

    ;; body
    (with-indentation
      (as-body body :module))
    (format *synthesis-stream* "endmodule // ~(~a~)" modname)))
