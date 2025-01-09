;; Environments
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
with a digit, and not consisting solely of an underscore. This is a
/lot/ more restrictive than Lisp's legal names, and is intentionally
quite conservative with respect to Verilog identifier names.")


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

PROPS should be an alist of properties.

N must be a legal identifier according to LEGAL-IDENTIFIER-P.
The new binding will mask any existing bindings of N in ENV.
ENV itself is unchanged.

The properties may include:

   - :width -- the width in bits assigned to the variable
   - :initial-value -- the initial value assigned to the name
   - :type -- the type to be stored in the variable
   - :as -- representation used for this variable
   - :direction -- for arguments to modules, the direction of information
     flow, which should be :in, :out, or :inout
   - :parameter -- T if the value is a module parameter"
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
    (error 'not-synthesisable :fragment n
			      :hint "Change the identifier name to a legal one")))


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
  (if-let ((p (assoc prop (get-environment-properties n env))))
    (cadr p)))


(defun variable-defined (n env)
  "Test whether N is defined in ENV."
  (not (null (assoc n env))))


(defun get-type (n env)
  "Return the type of N in ENV."
  (get-environment-property n :type env))


(defun get-representation (n env)
  "Return the representation of N in ENV."
  (get-environment-property n :as env))


(defun get-width (n env)
  "Return the width of N in ENV."
  (or (get-environment-property n :width env)

      ;; default value
      *default-register-width*))


(defun get-initial-value (n env)
  "Return the initial value of N in ENV."
  (or (get-environment-property n :initial-value env)

      ;; default value
      0))


(defun get-constant (n env)
  "Return whether N is constant in ENV."
  (if-let ((rep (get-representation n env)))
    (eql rep :constant)))


(defun get-direction (n env)
  "Return the direction of N in ENV."
  (get-environment-property n :direction env))


(defun filter-environment (pred env)
  "Return an environment containing all the entries of ENV matching PRED.

PRED should be a predicate taking a name and en environment."
  (flet ((filter-p (decl)
	   (let ((n (car decl)))
	     (not (funcall pred n env)))))

    (remove-if #'filter-p env)))


(defun map-environment (f env)
  "Map F across each declaration in ENV.

F should be a function taking a name and an environment. The
result is a list of the values returned from F."
  (flet ((map-decl (decl)
	   (let ((n (car decl)))
	     (funcall f n env))))

    (mapcar #'map-decl env)))
