;; Conditions for synthesis
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


;; ---------- Base condition ----------

(define-condition rtl-condition (vhdsl-condition)
  ((fragment
    :documentation "The code giving rise to the condition.

This is extracted automatically from the current function
form queue, or can be provided explicitly using the :FRAGMENT key."
    :initform (current-form)
    :initarg :fragment
    :reader fragment))
  (:documentation "Base class for RTLisp conditions.

The fragment is the code that gave rise to the condition, and is
used to add contxt to the report."))


(defvar *maximum-code-fragment-length* 40
  "Length of code fragment to be reported in conditions.

This only changes the printed length: the entire fragment is retained.")


(defmethod format-condition-context (detail (c rtl-condition) str)
  "Format the hint and code context of C on STR

DETAIL should be the detailed description of the condition. This
will be followed by a hint (if available) and the code context
(if available)."
  (call-next-method)

  ;; add context if known
  (if-let ((code (fragment c)))
    (format str "~%Context: ~a" (shorten *maximum-code-fragment-length*
					 (format nil "~a" code)
					 :ellipsis "..."))))


;; ---------- Synthesis ----------

(define-condition not-synthesisable (rtl-condition)
  ()
  (:report (lambda (c str)
	     (format-condition-context "Could not synthesise code"
				       c str)))
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
	     (format-condition-context (format nil "Unknown variable(s) ~a"
					       (variables c))
				       c str)))
  (:documentation "Condition signalled when one or more undeclared variables are encountered.

This is caused either by an undeclared variable or by use of a variable
that should be declared in the architectural environment, such as a register."))


(define-condition bad-variable (rtl-condition)
  ((var
    :documentation "The variable."
    :initarg :variable
    :reader variables))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Badly-named variable or module ~a"
					       (variables c))
				       c str)))
  (:documentation "Condition signalled when an unacceptable identifier is encountered.

This is caused by using identifier names may be legal in Lisp (with
its very permissive rules) but that can't be synthesised (because of
limitations in the underlying technology)."))


(define-condition unknown-module (rtl-condition)
  ((modname
    :documentation "The module."
    :initarg :module
    :reader module))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Unknown module ~a"
					       (module c))
				       c str)))
  (:documentation "Condition signalled when an unnown module is imported.

This means that the required module isn't available, either having not yet
been defined or not having been imported."))


(define-condition duplicate-variable (rtl-condition)
  ((var
    :documentation "The variable(s)."
    :initarg :variable
    :initarg :variables
    :reader variables))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Duplicate variable(s) ~a"
					       (variables c))
				       c str)))
  (:documentation "Condition signalled when a variable is re-defined in the same scope."))


(define-condition duplicate-module (rtl-condition)
  ((modname
    :documentation "The module."
    :initarg :module
    :reader module))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Duplicate module ~a"
					       (module c))
				       c str)))
  (:documentation "Condition signalled when a module is re-defined."))


(define-condition not-importable (rtl-condition)
  ((modname
    :documentation "The module."
    :initarg :module
    :reader module))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Module ~a can't be imported"
					       (module c))
				       c str)))
  (:documentation "Condition signalled when a module can't be imported.

This is usually caused by mismatched arguments, either an unknown argument
being provided in the import or one that's needed not being provided."))


(define-condition not-static (rtl-condition)
  ()
  (:report (lambda (c str)
	     (format-condition-context "Expression is not static" c str)))
  (:documentation "Condition signalled when an expression isn't static.

Some prts of a program need to be known at compile-time, and so can't
include terms that are only known at run-time."))


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
	     (format-condition-context (format nil "Expected a value that is one of ~s, got ~s"
					       (expected-values c)
					       (received-value c))
				       c str)))
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
	     (format-condition-context (format nil "Expected a direction that is one of ~s, got ~s"
					       (expected-values c)
					       (received-value c))
				       c str)))
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
	     (format-condition-context (format nil "Expected a value of type ~a, got one of type ~a"
					       (expected-type c)
					       (received-type c))
				       c str)))
  (:documentation "Condition signalled when types don't match.

The most common case is making an assignment of an expression to a
variable that is too narrow to accommodate all its possible values.
This is usually signalled as a warning, as there is a
sometimes-acceptable default action to risk the loss of precision
caused by the assignment."))


(define-condition bitfield-mismatch (rtl-condition)
  ((pattern
    :documentation "The bitfield pattern."
    :initarg :pattern
    :reader pattern))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Can't interpret bitfield pattern~a: ~a"
					       (format-hint c)
					       (pattern c))
				       c str)))
  (:documentation "Condition signalled when a bitfield pattern can't be parsed.

This is usually caused by non-consecutive uses of variables in the pattern."))


(define-condition shape-mismatch (rtl-condition)
  ((shape
    :documentation "The expected shape."
    :initarg :expected
    :reader shape))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Data doesn't match shape~a: ~a"
					       (format-hint c)
					       (shape c))
				       c str)))
  (:documentation "Condition signalled when data has the wrong shape.

This means that the initial contents of an array are not in the right
shape for the array. Usually this can be fixed by simply reformatting
the data, and/or making sure there's the right amount of it."))
