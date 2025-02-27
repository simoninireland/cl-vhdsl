;; Constructing words from bitfields
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


;; ---------- make-bitfields ----------

(defun typecheck-pattern (pat env)
  "Typecheck bitfield pattern PAT in ENV."
  (let ((typ (typecheck pat env)))
    (ensure-fixed-width typ)
    typ))


(defmethod typecheck-sexp ((fun (eql 'make-bitfields)) args env)
  (destructuring-bind (&rest pats)
      args
    (let ((tys (mapcar (rcurry #'typecheck-pattern env) pats)))
      ;; work out the bit width of the result
      (let ((w (foldr #'+
		      (mapcar (rcurry #'bitwidth env) tys)
		      0)))
	`(fixed-width-unsigned ,w)))))


(defmethod synthesise-sexp ((fun (eql 'make-bitfields)) args (context (eql :inexpression)))
  (as-list args :inexpression
	   :before "{"
	   :after "}"))


;; ---------- repeat-bits ----------

(defmethod typecheck-sexp ((fun (eql 'repeat-bits)) args env)
  (destructuring-bind (reps bs)
      args
    (let ((tyr (typecheck reps env))
	  (tyb (typecheck bs env)))
      (ensure-fixed-width tyr)
      (ensure-fixed-width tyb)

      ;; evaluate the repetitions, whcih must be statically determined
      (let ((w (bitwidth tyb env))
	    (r (ensure-static reps env)))
	`(fixed-width-unsigned ,(* w r))))))


(defmethod synthesise-sexp ((fun (eql 'repeat-bits)) args (context (eql :inexpression)))
  (destructuring-bind (reps bs)
      args
    (as-literal "{")
    (synthesise reps :inexpression)
    (as-literal "{")
    (synthesise bs :inexpression)
    (as-literal "}}")))
