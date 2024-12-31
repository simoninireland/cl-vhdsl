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
;; - :constant -- T is the binding is a constant
;; - :direction -- direction of dataflow for module arguments
;;
;; Default and consistency checks are applied.

(defun width-can-store-p (w ty env)
  "Test whether W bits can accommodate the values of TY in ENV."
  (>= w (bitwidth ty env)))


(defun ensure-width-can-store (w ty env)
  "Ensure that W bits can accommodate the values of TY in ENV."
  (unless (width-can-store-p w ty env)
    (error 'type-mismatch :expected ty :got w)))


;; the lambda list is the "wrong way round" from "normal" to allow
;; this function to be folded across a list of declarations
(defun typecheck-decl (env decl)
  "Typecheck DECL in ENV, returning ENV extended by DECL."
  (if (listp decl)
      ;; full declaration
      (destructuring-bind (n v &key width type constant)
	  decl
	(let ((ty (typecheck v env)))
	  (when type
	    ;; if a type is provided, make sure the initial
	    ;; value fits in it and then use that as the type
	    ;; for the binding
	    (ensure-subtype ty type)
	    (setq ty type))

	  (if width
	      (progn
		;; if a width is provided, make sure it's enough to
		;; accommodate the type
		(ensure-width-can-store width ty env)

		;; widen the type to match the width
		(setq ty (widen-fixed-width ty width)))

	      ;; if none was provided, the width is that of the type
	      (setq width (bitwidth ty env)))

	  (extend-environment n `((:initial-value ,v)
				  (:type ,ty)
				  (:width ,width)
				  (:constant ,constant))
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
      (mapn (rcurry #'typecheck ext) body ))))


(defun synthesise-register (decl as)
  "Synthesise a register declaration within a LET block.

The register has name N and initial value V, with the optional
WIDTH defaulting to the system's global width."
  (destructuring-bind  (n v &key (width *default-register-width*))
      decl
    (format *synthesis-stream* "reg [ ")
    (synthesise width :rvalue)
    (format *synthesis-stream* " - 1 : 0 ] ")
    (synthesise n :rvalue)
    (format *synthesis-stream* " = ")
    (synthesise v :rvalue)
    (format *synthesis-stream* ";")))


(defmethod synthesise-sexp ((fun (eql 'let)) args (as (eql :statement)))
  (let ((decls (car args))
	(body (cdr args)))

    ;; synthesise the registers
    (as-body decls :declaration :process #'synthesise-register)
    (format *synthesis-stream* "~%")

    ;; synthesise the body
    (as-body body :statement)))
