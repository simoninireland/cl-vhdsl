;; Assignments
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


;; ---------- Defaults ----------

(defparameter *default-register-width* 8
  "Default width for registers.

This is used absent any specfic width specification. It will
generally reflect the word size of the desired circuit, for
example 8, 16, 32, or 64 bits.")


;; ---------- Identifiers ----------

(defvar *identifier-regexp* (create-scanner "^([A-Za-z][A-Za-z0-9_]*)|(_[A-Za-z0-9_]+)$")
  "The regexp scanner used for determining legal variable names.

Legal names contains letters, digits, and underscores, not leading
with a digit. This is a /lot/ more restrictive than Lisp's legal
names, and is intentionally quite conservative with respect to Verilog
identifier names.")


;; There are probably more reserved words
(defvar *reserved-words* '("module" "input" "output" "inout"
			   "always" "posedge" "assign"
			   "parameter" "localparam" "reg" "signed")
  "Reserved words in Verilog.

These are used to prevent poor choices of variable name: they
all match *IDENTIFIER-REGEXP* but still can't be used.")


;; ---------- Type checking ----------

(defun ensure-subtype (ty1 ty2)
  "Ensure TY1 is a sub-type of TY2.

Signals TYPE-MISMATCH is the types are not compatible. This
can be ignored for systems not concerned with loss of precision."
  (if (not (subtypep ty1 ty2))
      (signal 'type-mismatch :expected ty2 :got ty1)))


;; ---------- Type environments ----------

(defun empty-environment ()
  "Return the empty type environment."
  '())


(defun extend-environment (n props env)
  "Return a type environment extending ENV with a binding of N to PROPS.

The new binding will mask any existing bindings of N in ENV.
ENV itself is unchanged.

The properties must include:

   - :type -- the type associated with the name

and may include:

   - :width -- the width in bits assigned to the variable
   - :initial-value -- the initial value assigned to the name
   - :direction -- for arguments to modules, the direction of information
     flow, which should be :in, :out, or :inout
   - :constant -- T if the value is constant and cannot be re-assigned"
  (ensure-legal-identifier n)
  (cons (cons n props) env))


(defun legal-identifier-p (n)
  "Ensure N is a legal identifier.

This uses *IDENTIFIER-REGEXP* for legality while avoiding strings
in *RESERVED-WORDS*."
  (when (symbolp n)
    (setq n (symbol-name n)))
  (and (not (member n *reserved-words* :test #'string-equal))
       (scan *identifier-regexp* n)))


(defun ensure-legal-identifier (n)
  "Ensure that N is a legal identifier.

Signals NOT-SYNTHESISABLE as an error if not."
  (unless (legal-identifier-p n)
    (error 'not-synthesisable :fragment n)))


(defun get-environment-properties (n env)
  "Return the key/value list for N in ENV.

An UNKNOWN-VARIABLE error is signalled if N is undefined."
  (if-let ((kv (assoc n env)))
    (cdr kv)
    (error 'unknown-variable :variable n)))


(defun get-environment-names (env)
  "Return the names in ENV."
  (mapcar #'car env))


(defun get-environment-property (n prop env)
  "Return PROP for N in ENV."
  (cadr (assoc prop (get-environment-properties n env))))


(defun variable-defined (n env)
  "Test whether N is defined in ENV."
  (not (null (assoc n env))))


(defun get-type (n env)
  "Return the type of N in ENV."
  (get-environment-property n :type env))


(defun get-width (n env)
  "Return the width of N in ENV."
  (if-let ((props (get-environment-property n :width env)))
    props

    ;; default value
    *default-register-width*))


(defun get-initial-value (n env)
  "Return the initial value of N in ENV."
  (if-let ((props (get-environment-property n :initial-value env)))
    props

    ;; default value
    0))


(defun get-constant (n env)
  "Return whether N is constant in ENV."
  (if-let ((const (get-environment-property n :constant env)))
    const

    ;; default is not constant
    nil))


(defun get-direction (n env)
  "Return the direction of N in ENV."
  (get-environment-property n :direction env))
