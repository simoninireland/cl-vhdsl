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


;; ---------- Lisp evaluation in RTLisp-derived environments ----------

(defun make-environment-alist (env)
  "Return a list of name/value pairs of the elements of ENV.

The pairs can be used in LET blocks, or as an alist."
  (flet ((make-decl (n env)
	   `(,n ,(get-environment-property n :initial-value env))))

    (map-environment #'make-decl env)))


(defun close-form-in-environment (form env)
  "Close FORM as a Lisp term in the environment ENV.

Each declaration in ENV is converted into a LET declaration with the
same name and initial value. FORM is then placed as the body of this
LET form.

Note that FORM should be Lisp, not RTLisp: to evaluate RTLisp forms
first use LISPIFY to generate proper Lisp for evaluation."
  (if (null env)
      form

      ;; expand the static enviroment and close over it
      (let* ((ext (make-environment-alist env))
	     (ns (alist-keys ext)))
	`(let ,ext
	   (declare (ignorable ,@ns))  ;; don't warn about un-used variables
	   ,form))))


(defun constant-p (form env)
  "Test whether FORM is a constant.

Constants include literals and constant symbols, but do
not includer module parameters: for that use STATIC-CONSTANT-P)."
  (or (integerp form)
      (and (symbolp form)
	   (variable-declared-p form env)
	   (eql (get-representation form env) :constant))))


(defun static-constant-p (form env)
  "Test whether FORM is a static constant in ENV.

Static constants include constants as identified by
CONSTANT-P and module parameters."
  (or (integerp form)
      (and (symbolp form)
	   (variable-declared-p form env)
	   (member (get-representation form env) '(:parameter
						   :constant)))))


(defun close-form-in-constant-environment (form env)
  "Close FORM as Lisp in the constant part of environment ENV.

ENV is filtered to include only the constant elements of the
environment, as identified by CONSTANT-P. In other words, the
form is placed in an environment with no non-constant information."
  (let ((constantnv (filter-environment #'constant-p env)))
    (close-form-in-environment form constantenv)))


(defun close-form-in-static-environment (form env)
  "Close FORM as Lisp in the static part of environment ENV.

ENV is filtered to include only the static elements of the
environment, as identified by STATIC-CONSTANT-P. In other words, the
form is placed into an environment that is known at compile time."
  (let ((staticenv (filter-environment #'static-constant-p env)))
    (close-form-in-environment form staticenv)))


(defun eval-lisp-in-static-environment (form env)
  "Evaluate Lisp FORM in the static part of environment ENV.

ENV is filtered to include only the static elements of the
environment, consisting of package parameters. In other words,
the evaluation takes place in an environment that is known at
compile time.

FORM should be in Lisp: to evaluate RTLisp forms use
EVAL-IN-STATIC-ENVIRONMENT."
  (eval (close-form-in-static-environment form env)))


;; ---------- RTLisp evaluation ----------

;; The forms passed to these functions should be RTLisp, which is
;; Lispified before closure and evaluation

(defun eval-in-static-environment (form env)
  "Evaluate RTLisp FORM as Lisp in the static part of environment ENV.

ENV is filtered to include only the static elements of the
environment, consisting of package parameters. In other words,
the evaluation takes place in an environment that is known at
compile time.

The resulting form is evaluated as Lisp, and so will signal
standard Lisp conditions. To run Lisp (rather than RTLisp) use
EVAL-LISP-IN-STATIC-ENVIRONMENT."
  (eval-lisp-in-static-environment (lispify form env) env))


(defun ensure-static (form env)
  "Evaluate RTLisp FORM in the static environment of ENV, returning its value.

A NOT-STATIC error condition is signalled if FORM does not
evaluate to a constant."
  (with-current-form form
    (handler-bind
	((error (lambda (c)
		  (error 'not-static :underlying-condition c
				     :hint "Make sure expression is a constant known at compile-time"))))
      (eval-in-static-environment form env))))


(defun eval-if-static (form env)
  "Evauate RTLisp FORM in the static environment of ENV, returning its value.

If the form isn't statically constant, return NIL."
  (handler-case
      (eval-in-static-environment form env)
    (error () nil)))


(defun eval-if-constant-or-return (form env)
  "Evaluate RTLisp FORM in the constant environment of ENV, returning its value.

If FORM is not a constant, return FORM unchanged. Thi is essentially a function
to simplify simplee expressions."
   (handler-case
      (eval-in-constant-environment form env)
    (error () form)))


(defun static-p (form env)
  "Test whether FORM is statically known in ENV.

This just evaluates FORM and throws away the result."
  (handler-case
      (progn
	(eval-in-static-environment form env)
	t)
    (error () nil)))
