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


;; A binder defines an environment, and we expect the following
;; keys to be provided where appropriate:
;;
;; - :width -- the width of a register in bits
;; - :type -- the type being held by the binding


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
    (synthesise v :rvalue))
  (format *synthesis-stream* ";"))


(defmethod synthesise-sexp ((fun (eql 'let)) args (as (eql :statement)))
  (let ((decls (car args))
	(body (cdr args)))

    ;; synthesise the registers
    (as-body decls :declaration :process #'synthesise-register)
    (format *synthesis-stream* "~%")

    ;; synthesise the body
    (as-body body :statement)))
