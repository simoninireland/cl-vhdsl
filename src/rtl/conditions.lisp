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
(declaim (optimize debug))


;; ---------- Base condition ----------

(define-condition rtl-condition ()
  ((fragment
    :documentation "The code giving rise to the condition."
    :initarg :fragment
    :reader fragment)
   (hint
    :documentation "A hint as to how to fix the condition."
    :initarg :hint
    :initform nil
    :reader hint))
  (:documentation "Base class for RTLisp conditions.

The fragment is the code that gave rise to the condition. A
hint can be given to suggest how to fix the issue."))


(defvar *MAXIMUM-CODE-FRAGMENT-LENGTH* 40
  "Length of code fragment to be reported in conditions.

This only changes the printed length: the entire fragment is retained.")


(defun format-fragment (cond)
  "Format the fragment of COND  as a string.

The string is cut-off after a length given by
*MAXIMUM-CODE-FRAGMENT-LENGTH*."
  (shorten *MAXIMUM-CODE-FRAGMENT-LENGTH*
	   (format nil "~a" (fragment cond))
	   :ellipsis "..."))


(defun format-hint (cond)
  "If the hint of COND is non-nil, format it for display"
  (if-let ((hint (hint cond)))
      (format nil " (Hint: ~a)" hint)
    ""))


;; ---------- Synthesis ----------

(define-condition not-synthesisable (rtl-condition)
  ()
  (:report (lambda (c str)
	     (format str "Could not synthesise code~a: ~a"
		     (format-hint c)
		     (format-fragment c))))
  (:documentation "Condition signalled when code can't be synthesised.

This usually indicates that the Lisp fragment provided is un-parsable because
it contains non-synthesisable syntax, or because of a problem with the
synthesiser's code generator."))


;; ---------- Checking ----------

(define-condition unknown-variable (rtl-condition)
  ((var
    :documentation "The variable(s)."
    :initarg :variable
    :initarg :variables
    :reader variables))
  (:report (lambda (c str)
	     (format str "Unknown variable(s): ~a"
		     (variables c))))
  (:documentation "Condition signalled when one or more undeclared variables are encountered.

This is caused either by an undeclared variable or by use of a variable
that should be declared in the architectural environment, such as a register."))


(define-condition value-mismatch (rtl-condition)
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


(define-condition direction-mismatch (rtl-condition)
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


(define-condition type-mismatch (rtl-condition)
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
