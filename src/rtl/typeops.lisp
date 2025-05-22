;; Type operations
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


;; ---------- Type checking ----------

(defun ensure-subtype (ty1 ty2)
  "Ensure TY1 is a sub-type of TY2 in the current enironment.

Signals TYPE-MISMATCH is the types are not compatible. This
can be ignored for systems not concerned with loss of precision."
  (let ((ety1 (expand-type-parameters ty1))
	(ety2 (expand-type-parameters ty2)))
    (when (not (subtypep ety1 ety2))
      (signal 'type-mismatch :expected ty2 :got ty1))))


;; ---------- Bit widths ----------

(defgeneric bitwidth (v)
  (:documentation "Return the bits required to represent V.

A list V is assumed to be a type specifier, which is passed to
BITWIDTH-TYPE.")
  ;; TODO: I'm not entirely sure about this as a design
  (:method ((v list))
    (let ((tys (safe-car-cdr v)))
      (bitwidth-type (car tys) (cadr tys)))))


(defgeneric bitwidth-type (tytag tyargs)
  (:documentation "Return the width need for values of a type.

The type is tagged TYTAG with arguments TYARGS."))


;; ---------- Least upper-bounds ----------

(defun lub (ty1 ty2)
  "Return the least upper-bound of types TY1 and TY2 in the current environment.

By default the upper bound is T, and if either is null the
upper bound is the other. For other combinations the type tag
is extracted and used in a call to LUB-TYPE.

Type parameters are not expanded by default."
  (cond ((null ty1)
	 ty2)
	((null ty2)
	 ty1)
	(t
	 (let ((ty1s (safe-car-cdr ty1))
	       (ty2s (safe-car-cdr ty2)))
	   (lub-type (car ty1s) (cadr ty1s)
		     (car ty2s) (cadr ty2s))))))


(defgeneric lub-type (ty1tag ty1args ty2tag ty2args)
  (:documentation "Return the least upper-bound of two types in tyhe current environment.

The types are tagged TY1TAG and TY2TAG, with specialising arguments
TY1ARGS and TY2ARGS respectively (both of which can be nil).

The default LUB of two types is T.")
  (:method (ty1tag ty1args ty2tag ty2args)
    t))


;; ---------- Type parameter expansion ----------

(defgeneric expand-type-parameters (ty)
  (:documentation "Expand any parameters in the type TY in the current environment.

This expands constants that appear within type declarations,
which always need to be statically constant.")
  (:method (ty)
    ty)
  (:method ((ty list))
    (expand-type-parameters-type (car ty) (cdr ty))))


(defgeneric expand-type-parameters-type (ty args)
  (:documentation "Expand any parameters in the type tag TY applied to ARGS."))


;; ---------- Type constraints in environments ----------

(defun add-type-constraint (n ty)
  "Constraint N to have type TY.

N must be in scope, but need not be in the shallowest frame of ENV."
  (let ((constraints (variable-property n :type-constraints :default nil)))
    (set-variable-property n :type-constraints (cons ty constraints))))
