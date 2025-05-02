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


;; ---------- Bit widths ----------

(defgeneric bitwidth (v env)
  (:documentation "Return the bits required to represent V in ENV.

A list V is assumed to be a type specifier, which is passed to
BITWIDTH-TYPE.")
  ;; TODO: I'm not entirely sure about this as a design
  (:method ((v list) env)
    (let ((tys (safe-car-cdr v)))
      (bitwidth-type (car tys) (cadr tys) env))))


(defgeneric bitwidth-type (tytag tyargs env)
  (:documentation "Return the width need for values of a type in ENV.

The type is tagged TYTAG with arguments TYARGS."))


;; ---------- Least upper-bounds ----------

(defun lub (ty1 ty2 env)
  "Return the least upper-bound of types TY1 and TY2 in ENV.

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
		     (car ty2s) (cadr ty2s)
		     env)))))


(defgeneric lub-type (ty1tag ty1args ty2tag ty2args env)
  (:documentation "Return the least upper-bound of two types in ENV.

The types are tagged TY1TAG and TY2TAG, with specialising arguments
TY1ARGS and TY2ARGS respectively (both of which can be nil).

The default LUB of two types is T.")
  (:method (ty1tag ty1args ty2tag ty2args env)
    t))


;; ---------- Type parameter expansion ----------

(defgeneric expand-type-parameters (ty env)
  (:documentation "Expand any parameters in the type TY in ENV.

This expands constants that appear within type declarations,
which always need to be statically constant.")
  (:method (ty env)
    ty)
  (:method ((ty list) env)
    (expand-type-parameters-type (car ty) (cdr ty) env)))


(defgeneric expand-type-parameters-type (ty args env)
  (:documentation "Expand any parameters in the type tag TY applied to ARGS in ENV."))


;; ---------- Type constraints in environments ----------

(defun add-type-constraint (n ty env)
  "Constraint N to have type TY in ENV.

N must be in scope, but need not be in the shallowest frame of ENV."
  (let ((constraints (get-environment-property n :type-constraints env :default nil)))
    (set-environment-property n :type-constraints (cons ty constraints) env)))
