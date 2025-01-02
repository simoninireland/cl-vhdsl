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


(defun close-form-in-environment (form env)
  "Close FORM as a Lisp term in the environment ENV.

Each declaration in ENV is converted into a LET declaration
with the same name and initial value. FORM is then placed
as the body of this LET form."
  (labels ((make-decl (n env)
	     `(,n ,(get-environment-property n :initial-value env)))

	   (make-env (env)
	     (map-environment #'make-decl env)))

    (if (null env)
	form
	`(let ,(make-env env)
	   ,form))))


(defun close-form-in-static-environment (form env)
  "Close  FORM as Lisp in the static part of environment ENV.

ENV is filtered to include only the static elements of the
environment, consisting of package parameters. In other words, the
form is placed into an environment that is known at compile time."
  (flet ((parameter-p (n env)
	   (get-environment-property n :parameter env)))

    (let ((staticenv (filter-environment #'parameter-p env)))
      (close-form-in-environment form staticenv))))


(defun eval-in-static-environment (form env)
  "Evaluate FORM as Lisp in the static part of environment ENV.

ENV is filtered to include only the static elements of the
environment, consisting of package parameters. In other words,
the evaluation takes place in an environment that is known at
compile time.

The resulting form is evaluated as Lisp, and so will signal
standard Lisp conditions."
  (eval (close-form-in-static-environment form env)))
