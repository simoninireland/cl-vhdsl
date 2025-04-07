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


;; ---------- PROGN ----------

(defmethod typecheck-sexp ((fun (eql 'progn)) args env)
  (mapn (rcurry #'typecheck env) args))


(defun simplify-progn-body (body)
  "Simplify the body of a PROGN or implied PROGN block."
  (foldr (lambda (l arg)
	   (if (and (listp arg)
		    (eql (car arg) 'progn))
	       (append l (cdr arg))
	       (append l (list arg))))
	 body
	 '()))


(defmethod simplify-progn-sexp ((fun (eql 'progn)) args)
  (destructuring-bind (&rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(progn ,@(simplify-progn-body newbody)))))


(defmethod synthesise-sexp ((fun (eql 'progn)) args (context (eql :inblock)))
  (as-block args :inblock :indent nil))

(defmethod synthesise-sexp ((fun (eql 'progn)) args (context (eql :inmodule)))
  (synthesise-sexp fun args :inblock))


;; ---------- Triggered blocks ----------

(defun combinatorial-trigger-p (form)
  "Test whether FORM is a combinatorial trigger.

Combinatorial triggers are sensitive to all the wires in the
block, and are represented by the symbol *."
  (and (listp form)
       (= (length form) 1)
       (eql (car form) '*)))


(defmethod typecheck-sexp ((fun (eql '@)) args env)
  (destructuring-bind (sensitivities &rest body)
      args
    ;; We accept single variables or lists of variables as senasitivity,
    ;; but there's an ambiguity over posedge and negedge operators, so
    ;; we explicity check the car of any list to see whether it's
    ;; "really" an atom
    ;;
    ;; These checks actually need to be slightly different, to make
    ;; sure we identify something with wires and not just a value.
    ;; That's not quite "typechecking" in the sense we use it.
    (if (listp sensitivities)
	(cond ((combinatorial-trigger-p sensitivities)
	       ;; sensitive to everything
	       nil)

	      ((edge-trigger-p sensitivities)
	       ;; a single instance of a trigger operator
	       (typecheck sensitivities env))

	      (t
	       ;; a list of sensitivities
	       (dolist (s sensitivities)
		 (typecheck s env))))

	;; an atom
	(typecheck sensitivities env))

    ;; check the body in the outer environment
    (mapn (rcurry #'typecheck env) body)))


(defmethod simplify-progn-sexp ((fun (eql '@)) args)
  (destructuring-bind ((&rest sensitivities) &rest body)
      args
    (let ((newbody (mapcar #'simplify-progn body)))
      `(@ ,sensitivities ,@(simplify-progn-body newbody)))))


(defmethod synthesise-sexp ((fun (eql '@)) args (context (eql :inblock)))
  (declare (optimize debug))
  (destructuring-bind (sensitivities &rest body)
      args
    (if (combinatorial-trigger-p sensitivities)
	;; luteral expansion for combinatoreial blocks
	(as-literal "always @(*)")

	;; expand specified wires
	(as-list (if (listp sensitivities)
		     (if (edge-trigger-p sensitivities)
			 ;; an edge trigger, synthesise as an operator
			 (list sensitivities)

			 ;; a list of triggers, synthesise as a list
			 sensitivities)

		     ;; a single trigger, synthesise as a list
		     (list sensitivities))
		 :inexpression
		 :before "always @(" :after ")"))
    (as-newline)

    (as-block body :inblock
	      :before "begin" :after "end" :always t)
    (as-blank-line)))

(defmethod synthesise-sexp ((fun (eql '@)) args (context (eql :inmodule)))
  (synthesise-sexp fun args :inblock))


;; ---------- Triggers ----------

(defun edge-trigger-p (form)
  "Test whether FORM is an edge trigger expression."
  (and (listp form)
       (member (car form) '(posedge negedge))))


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
