;; Assignments
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


(defun writeable-p (n env)
  "Test whether N is writeable in ENV.

To be writeable a variable must be a register or wire, not a constant,
and not an input argument."
  (and (not (get-constant n env))
       (not (eql (get-representation n env) :constant))
       (not (eql (get-direction n env) :in))))


(defun ensure-writeable (n env)
  "Ensure N is writeable in ENV.

Signals NOT-SYNTHESISABLE if an attempt is made to update a
constant or an input parameter."
  (unless (writeable-p n env)
    (error 'not-synthesisable :hint "Ensure target is writeable")))


;; ---------- setq ----------

(defmethod typecheck-sexp ((fun (eql 'setq)) args env)
  (destructuring-bind (n v &key sync)
      args
    ;; catch the common mistake of using SETQ when we need SETF
    (unless (symbolp n)
      (error 'not-synthesisable :hint "Do you need SETF instead of SETQ?"))

    (let ((tyvar (typecheck n env))
	  (tyval (typecheck v env)))
      (ensure-subtype tyval tyvar)
      (ensure-writeable n env)

      tyval)))


(defmethod synthesise-sexp ((fun (eql 'setq)) args (context (eql :inblock)))
  (destructuring-bind (var val &key (sync nil))
      args
    (synthesise var :inassignment)
    (if sync
	(as-literal " = ")
	(as-literal " <= "))
    (synthesise val :inexpression)
    (format *synthesis-stream* ";")))


(defmethod synthesise-sexp ((fun (eql 'setq)) args (context (eql :inmodule)))
  (destructuring-bind (var val)
      args
    (as-literal "assign ")
    (synthesise var :inassignment)
    (as-literal " = ")
    (synthesise val :inexpression)
    (as-literal ";")))



;; ---------- setf (generalised places) ----------

(defgeneric generalised-place-p (form env)
  (:documentation "Test whether FORM is a generalised place in ENV.

Generalised places can appear as the target of SETF forms. (In other
languages they are sometimes referred to as /lvalues/.) This is
separate, but related to, their type: a generalised place has a type,
but is also SETF-able.

By default forms are /not/ generalised places.")
  (:method ((form integer) env)
    nil)
  (:method ((form symbol) env)
    (not (get-constant form env)))
  (:method ((form list) env)
    (destructuring-bind (fun &rest args)
	form
      (generalised-place-sexp-p fun args env))))


;; No need to dive into the args in most cases, just the selector

;; There might be a better approach to tis using reference types
;; to differentiate between l- and r-value instances

(defgeneric generalised-place-sexp-p (fun args env)
  (:documentation "Test whether FUN applied to ARGS identifies a generalised place in ENV.

The default is for a form /not/ to be a generalised place.")
  (:method (fun args env)
    nil))


(defun ensure-generalised-place (form env)
  "Ensure FORM is a generalised place in ENV."
  (unless (generalised-place-p form env)
    (error 'not-synthesisable :hint "Make sure the code identifies a generalised, SETF-able, place")))


(defmethod typecheck-sexp ((fun (eql 'setf)) args env)
  (destructuring-bind (var val &key sync)
      args
    (if (listp var)
	(typecheck-sexp-setf (car var) val (cdr var) env :sync sync)

	;; a SETF to a simple variable is a SETQ
	(typecheck `(setq ,var ,val :sync ,sync) env))))


(defmethod typecheck-sexp-setf ((selector symbol) val selectorargs env &key sync)
  (let* ((place `(,selector ,@selectorargs))
	 (tyvar (typecheck place env))
	 (tyval (typecheck val env)))
    (ensure-subtype tyval tyvar)
    (ensure-generalised-place place env)

    tyvar))


(defmethod synthesise-sexp ((fun (eql 'setf)) args (context (eql :inblock)))
  (destructuring-bind (var val &key (sync nil))
      args
    (synthesise var :inassignment)
    (if sync
	(as-literal " = ")
	(as-literal " <= "))
    (synthesise val :inexpression)
    (as-literal ";")))


(defmethod synthesise-sexp ((fun (eql 'setf)) args (context (eql :inmodule)))
  (destructuring-bind (var val)
      args
    (as-literal "assign ")
    (synthesise var :inassignment)
    (as-literal " = ")
    (synthesise val :inexpression)
    (as-literal ";")))
