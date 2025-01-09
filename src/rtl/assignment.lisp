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
(declaim (optimize debug))


(defun writeable-p (n env)
  "Test whether N is writeable in ENV.

To be writeable a variable must be a register, not a constant,
not a Lisp-typed value, and not an input argument."
  (and (not (get-constant n env))
       (not (eql (get-type n env) :lisp))
       (not (eql (get-direction n env) :in))))


(defun ensure-writeable (n env)
  "Ensure N is writeable in ENV.

Signals NOT-SYNTHESISABLE is an attempt is made to update a
constant or an input parameter."
  (unless (writeable-p n env)
    (error 'not-synthesisable :fragment `(setf ,n))))


(defmethod typecheck-sexp ((fun (eql 'setf)) args env)
  (destructuring-bind (var val &key sync)
      args
    (if (listp var)
	(typecheck-sexp-setf (car var) val (cdr var) env :sync sync)
	(typecheck-sexp-setf var val nil env :sync sync))))


(defmethod typecheck-sexp-setf ((selector symbol) val selectorargs env &key sync)
  (let ((tyvar (typecheck selector env))
	(tyval (typecheck val env)))
      (ensure-subtype tyval tyvar)
      (ensure-writeable selector env)

      tyval))


(defmethod typecheck-sexp-setf ((selector (eql 'bits)) val selectorargs env &key sync)
  (destructuring-bind (var start end)
      selectorargs
    (let ((tyvar (typecheck var env))
	  (tyval (typecheck val env))
	  (l (eval-in-static-environment `(+ 1 (- ,start ,end)) env)))
      (ensure-writeable var env)
      (ensure-width-can-store l tyval env)
      (if (> l (bitwidth tyvar env))
	  (error 'not-synthesisable :fragment `(setf (,selector ,@selectorargs) ,val)))

      tyval)))


(defmethod synthesise-sexp ((fun (eql 'setf)) args (context (eql :inblock)))
  (destructuring-bind (var val &key (sync nil))
      args
    (synthesise var :inassignment)
    (if sync
	(format *synthesis-stream* " = ")
	(format *synthesis-stream* " <= "))
    (synthesise val :inexpression)
    (format *synthesis-stream* ";")))


(defmethod synthesise-sexp ((fun (eql 'setf)) args (context (eql :inmodule)))
  (destructuring-bind (var val)
      args
    (format *synthesis-stream* "assign ")
    (synthesise var :inassignment)
    (format *synthesis-stream* " = ")
    (synthesise val :inexpression)
    (format *synthesis-stream* ";")))


;; Triggers

(defmethod typecheck-sexp ((fun (eql 'posedge)) args env)
  '(fixed-width-unsigned 1))


(defmethod synthesise-sexp ((fun (eql 'posedge)) args (context (eql :inexpression)))
  (destructuring-bind (pin)
      args
    (format *synthesis-stream* "posedge(")
    (synthesise pin :inexpression)
    (format *synthesis-stream* ")")))


(defmethod typecheck-sexp ((fun (eql 'negedge)) args env)
  '(fixed-width-unsigned 1))


(defmethod synthesise-sexp ((fun (eql 'negedge)) args (context (eql :inexpression)))
  (destructuring-bind (pin)
      args
    (format *synthesis-stream* "negedge(")
    (synthesise pin :inexpression)
    (format *synthesis-stream* ")")))
