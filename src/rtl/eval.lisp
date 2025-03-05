;; Evaluating RTLisp as Lisp
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


(defun make-static-environment-alist (env)
  "Return a list of name/value pairs of the static elements of ENV.

The pairs can be used in LET blocks, or as an alist."
  (flet ((make-decl (n env)
	   `(,n ,(get-environment-property n :initial-value env))))

    (map-environment #'make-decl
		     (filter-environment (lambda (n env)
					   (eql (get-representation n env)
						:parameter))
					 env))))


(defun close-form-in-environment (form env)
  "Close the RTLisp FORM as a Lisp term in the environment ENV.

Each declaration in ENV is converted into a LET declaration with the
same name and initial value. FORM is then Lispified and placed as the
body of this LET form."
  (let ((lispform (lispify form env)))
    (if (null env)
	lispform

	;; expand the static enviroment and close over it
	(let* ((ext (make-static-environment-alist env))
	       (ns (alist-keys ext)))
	  `(let ,ext
	     (declare (ignorable ,@ns))
	     ,lispform)))))


(defun static-constant-p (form env)
  "Test whether FORM is a static constant in ENV.

Static constants include literal constants and variables that
are module parameters or declared as constant."
  (or (integerp form)
      (and (symbolp form)
	   (variable-defined-p form env)
	   (member (get-representation form env) '(:parameter
						   :constant)))))


(defun close-form-in-static-environment (form env)
  "Close the RTLisp FORM as Lisp in the static part of environment ENV.

ENV is filtered to include only the static elements of the
environment, consisting of package parameters and declared constants.
In other words, the form is placed into an environment that is known
at compile time."
  (let ((staticenv (filter-environment #'static-constant-p env)))
    (close-form-in-environment form staticenv)))


(defun eval-in-static-environment (form env)
  "Evaluate RTLisp FORM as Lisp in the static part of environment ENV.

ENV is filtered to include only the static elements of the
environment, consisting of package parameters. In other words,
the evaluation takes place in an environment that is known at
compile time.

The resulting form is evaluated as Lisp, and so will signal
standard Lisp conditions."
  (eval (close-form-in-static-environment form env)))


(defun ensure-static (form env)
  "Evaluate FORM in the static environment of ENV, returning its value.

A NOT-STATIC error condition is signalled if FORM does not
evaluate to a constant."
  (with-current-form form
    (handler-bind
	((error (lambda (c)
		  (error 'not-static :underlying-condition c
				     :hint "Make sure expression is a constant known at compile-time"))))
      (eval-in-static-environment form env))))


(defun eval-if-static (form env)
  "Evauate FORM in the static environment of ENV, returning its value.

If the form isn't statically constant, return NIL."
  (handler-case
      (eval-in-static-environment form env)
    (error () nil)))
