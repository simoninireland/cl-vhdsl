;; Synthesisable control flow
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


;; ---------- PROGN ----------

(defmethod typecheck-sexp ((fun (eql 'progn)) args env)
  (mapn (rcurry #'typecheck env) args))


(defmethod simplify-progn-sexp ((fun (eql 'progn)) args)
  (destructuring-bind (&rest body)
      args
    (let ((newbody (mapcar #'simplify-progn args)))
      (cond ((= (length newbody) 0)
	     nil)

	    ((= (length newbody) 1)
	     (car newbody))

	    (t
	     `(progn ,@newbody))))))


(defmethod synthesise-sexp ((fun (eql 'progn)) args (context (eql :inblock)))
  (dolist (form args)
    (synthesise form :inblock)))


;; ---------- Triggered blocks ----------

(defmethod typecheck-sexp ((fun (eql '@)) args env)
  (destructuring-bind (sensitivities &rest body)
      args
    ;; check all sensitivities are single bits
    (dolist (s sensitivities)
      (ensure-subtype (typecheck s env) '(fixed-width-unsigned 1)))

    ;; check the body in the outer environment
    (mapn (rcurry #'typecheck env) body)))


(defmethod synthesise-sexp ((fun (eql '@)) args (context (eql :inblock)))
  (destructuring-bind ((&rest sensitivities) &rest body)
      args
    (as-literal "always @(")
    (as-list sensitivities :inexpression)
    (as-literal ")" :newline t)
    (as-body body :inblock
	     :before "begin" :after "end")
    (as-literal "" :newline t)))
(defmethod synthesise-sexp ((fun (eql '@)) args (context (eql :inmodule)))
  (synthesise-sexp fun args :inblock))


;; ---------- Triggers ----------

(defmethod typecheck-sexp ((fun (eql 'posedge)) args env)
  (destructuring-bind (pin)
      args
    (let ((ty (typecheck pin env)))
      (ensure-subtype ty '(fixed-width-unsigned 1))
      ty)))


(defmethod synthesise-sexp ((fun (eql 'posedge)) args (context (eql :inexpression)))
  (destructuring-bind (pin)
      args
    (as-literal"posedge(")
    (synthesise pin :inexpression)
    (as-literal ")")))


(defmethod typecheck-sexp ((fun (eql 'negedge)) args env)
  (destructuring-bind (pin)
      args
    (let ((ty (typecheck pin env)))
      (ensure-subtype ty '(fixed-width-unsigned 1))
      ty)))


(defmethod synthesise-sexp ((fun (eql 'negedge)) args (context (eql :inexpression)))
  (destructuring-bind (pin)
      args
    (as-literal "negedge(")
    (synthesise pin :inexpression)
    (as-literal")")))
