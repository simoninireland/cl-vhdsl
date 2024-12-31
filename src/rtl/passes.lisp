;; The compiler passes
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


;; ---------- Type and width checking ----------

(defgeneric typecheck (form env)
  (:documentation "Type-check FORM in ENV.")
  (:method ((form list) env)
    (let ((fun (car form))
	  (args (cdr form)))
      (handler-bind ((error #'(lambda (cond)
				;;(error 'not-synthesisable :fragment form)
				(error cond)
				)))
	(typecheck-sexp fun args env)))))


(defgeneric typecheck-sexp (fun args env)
  (:documentation "Type-check the application of FUN to ARGS in ENV."))


;; ---------- Synthesis ----------

(defgeneric synthesise (form as)
  (:documentation "Synthesise the Verilog for FORM.

The form may have a specified role of position indicated by AS.
This may be used to specialise synthesis methods according to
syntactic classes and the like.

A NOT-SYNTHESISABLE error will be signalled for all underlying
conditions.")
  (:method ((form list) as)
    (let ((fun (car form))
	  (args (cdr form)))
      (handler-bind ((error #'(lambda (cond)
				;;(error 'not-synthesisable :fragment form)
				(error cond)
				)))
	(synthesise-sexp fun args as)
	t))))


(defgeneric synthesise-sexp (fun args as)
  (:documentation "Write the synthesised Verilog of FUN called with ARGS.

The synthesised code may depend on the role or position AS,
which can be used to specialise the method."))
