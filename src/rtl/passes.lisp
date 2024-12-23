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


;; ---------- Synthesis ----------

(defgeneric synthesise (form as &optional str)
  (:documentation "Synthesise the Verilog for FORM to stream STR.

The form may have a specified role of position indicated by AS.
This may be used to specialise synthesis methods according to
syntactic classes and the like.

Methods should return a list of lines of synthesised code. Nested
lists indicate containment, which may be used in pretty-printing.
If only a single line is being sythesised, it needn't be wrapped into
a list.

A NOT-SYNTHESISABLE error will be signalled for all underlying
conditions.")
  (:method ((form list) as &optional str)
    (let ((fun (car form))
	  (args (cdr form)))
      (handler-bind ((error #'(lambda (cond)
				(error 'not-synthesisable :fragment form))))
	(synthesise-sexp fun args as str)))))


(defgeneric synthesise-sexp (fun args as str)
  (:documentation "Write the synthesised Verilog of FUN called with ARGS on STR.

The synthesised code may depend on the role or position AS,
which can be used to specialise the method."))
