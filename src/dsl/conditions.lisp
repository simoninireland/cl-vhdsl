;; DSL definition conditions
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

(in-package :cl-vhdsl/dsl)


;; ---------- Base condition ----------

(define-condition dsl-condition ()
  ((dsl
    :documentation "The DSL."
    :initarg :dsl
    :reader dsl)
   (hint
    :documentation "A hint as to how to fix the condition."
    :initarg :hint
    :initform nil
    :reader hint))
  (:documentation "Base class for DSL definition conditions."))


(defun format-condition-context (detail c str)
  "Format the hint of C on STR"
  (format str "~a" detail)

  ;; add hint if present
  (if-let ((hint (hint c)))
    (format str " (~a)" hint)))


;; ---------- DSL definition ----------

(define-condition no-current-dsl (dsl-condition)
  ()
  (:report (lambda (c str)
	     (format-condition-context "No DSL defined"
				       c str)))
  (:documentation "Condition signalled when there is no DSL being defined.

This happens because no DSL is explicitly set in a DSL function or
method, and there's no IN-DSL in effect."))


(define-condition unknown-dsl-function (dsl-condition)
  ((f
    :documentation "The function."
    :initarg :function
    :reader dsl-function))
  (:report (lambda (c str)
	     (format-condition-context (format nil "No function ~a defined over DSL" (dsl-function c))
				       c str)))
  (:documentation "Condition signalled for a funciton not defined over the DSL.

This happens when methods are added to a function that hasn't been
declared over the DSL."))


(define-condition duplicate-dsl-function (dsl-condition)
  ((f
    :documentation "The function."
    :initarg :function
    :reader dsl-function))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Duplicate definiton of function ~a over DSL" (dsl-function c))
				       c str)))
  (:documentation "Condition signalled for a function re-defined over the DSL."))


(define-condition unknown-dsl-form (dsl-condition)
  ((f
    :documentation "The form."
    :initarg :form
    :reader dsl-form))
  (:report (lambda (c str)
	     (format-condition-context (format nil "No form ~a defined in DSL" (dsl-function c))
				       c str)))
  (:documentation "Condition signalled for a form not defined in the DSL.

This happens when methods are added to a form that hasn't been added
as valid to the DSL."))


(define-condition duplicate-dsl-macro (dsl-condition)
  ((f
    :documentation "The macro."
    :initarg :macro
    :reader dsl-macro))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Duplicate definiton of macro ~a over DSL" (dsl-macro c))
				       c str)))
  (:documentation "Condition signalled for a macro re-defined over the DSL."))
