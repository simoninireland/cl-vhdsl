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

(defmethod synthesise-sexp ((fun (eql '@)) args (context (eql :inblock)))
  (let ((test (car args))
	(body (cdr args)))
    (as-literal "always @(")
    (synthesise test :inexpression)
    (as-literal ")" :newline t)
    (as-body body :inblock
	     :before "begin" :after "end")
    (as-literal "" :newline t)))
(defmethod synthesise-sexp ((fun (eql '@)) args (context (eql :inmodule)))
  (synthesise-sexp fun args :inblock))
