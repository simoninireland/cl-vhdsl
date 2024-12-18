;; Conditions for synthesis
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

(define-condition not-synthesisable ()
  ((fragment
    :documentation "The code that could not be synthesised."
    :initarg :fragment
    :reader fragment))
  (:report (lambda (c str)
	     (format str "Could not synthesise code:~%~a"
		     (fragment c))))
  (:documentation "Condition signalled when code can't be synthesised.

This usually indicates that the Lisp fragment provided is un-parsable because
it contains non-synthesisable syntax, or because of a problem with the
synthesiser's code generator."))


;; ---------- Checking ----------

(define-condition unknown-variable ()
  ((var
    :documentation "The variable(s)."
    :initarg :variable
    :initarg :variables
    :reader variables))
  (:report (lambda (c str)
	     (format str "Unknown variable(s) ~a"
		     (variables c))))
  (:documentation "Condition signalled when one or more undeclared variables are encountered.

This is caused either by an undeclared variable or by use of a variable
that should be declared in the architectural environment, such as a register."))


(define-condition value-mismatch ()
  ((expected
    :documentation "The values allowed."
    :initarg :expected
    :reader expected-values)
   (received
    :documentation "The value received."
    :initarg :got
    :reader received-value))
  (:report (lambda (c str)
	     (format str "Expected a value that is one of ~s, got ~s"
		     (expected-values c)
		     (received-value c))))
  (:documentation "Condition signalled when a mis-matched value is received."))


(define-condition direction-mismatch ()
  ((expected
    :documentation "The direction(s) allowed."
    :initarg :expected
    :reader expected-values)
   (received
    :documentation "The direction received."
    :initarg :got
    :reader received-value))
  (:report (lambda (c str)
	     (format str "Expected a direction that is one of ~s, got ~s"
		     (expected-values c)
		     (received-value c))))
  (:documentation "Condition signalled when a mis-matched direction is received.

This is usually caused by assigning to a module argument denoteed :IN."))


(define-condition type-mismatch ()
  ((expected
    :documentation "The expected type."
    :initarg :expected
    :reader expected-type)
   (received
    :documentation "The received type."
    :initarg :got
    :reader received-type))
  (:report (lambda (c str)
	     (format str "Expected a value of type ~a, got one of type ~a"
		     (expected-type c)
		     (received-type c))))
  (:documentation "Condition signalled when types don't match.

The most common case is making an assignment of an expression to a
variable that is too narrow to accommodate all its possible values.
This is usually signalled as a warning, as there is a
sometimes-acceptable default action to risk the loss of precision
caused by the assignment."))
