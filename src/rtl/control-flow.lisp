;; Synthesisable control flow
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


;; ---------- Helper ----------

(defun simplify-progn-body (body)
  "Simplify the body of a PROGN or implied PROGN block."
  (foldr (lambda (l arg)
	   (if (and (listp arg)
		    (eql (car arg) 'progn))
	       (append l (cdr arg))
	       (append l (list arg))))
	 body
	 '()))


;; ---------- PROGN ----------

(defmethod typecheck-sexp ((fun (eql 'progn)) args env)
  (mapn (rcurry #'typecheck env) args))


(defmethod simplify-progn-sexp ((fun (eql 'progn)) args)
  (destructuring-bind (&rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(progn ,@(simplify-progn-body newbody)))))


(defmethod synthesise-sexp ((fun (eql 'progn)) args (context (eql :inblock)))
  (as-block args :inblock))

(defmethod synthesise-sexp ((fun (eql 'progn)) args (context (eql :inmodule)))
  (synthesise-sexp fun args :inblock))


;; ---------- Triggered blocks ----------

(defmethod typecheck-sexp ((fun (eql '@)) args env)
  (destructuring-bind (sensitivities &rest body)
      args
    ;; check all sensitivities are single bits
    (dolist (s sensitivities)
      (ensure-subtype (typecheck s env) 'fixed-width-unsigned))

    ;; check the body in the outer environment
    (mapn (rcurry #'typecheck env) body)))


(defmethod simplify-progn-sexp ((fun (eql '@)) args)
  (destructuring-bind ((&rest sensitivities) &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(@ ,sensitivities ,@(simplify-progn-body newbody)))))


(defmethod synthesise-sexp ((fun (eql '@)) args (context (eql :inblock)))
  (destructuring-bind ((&rest sensitivities) &rest body)
      args
    (as-list sensitivities :inexpression
	     :before "always @(" :after ")")
    (as-newline)

    (as-block body :inblock
	      :before "begin" :after "end" :always t)
    (as-blank-line)))

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
