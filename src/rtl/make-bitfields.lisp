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


;; ---------- extend-bits ----------

(defmethod typecheck-sexp ((fun (eql 'extend-bits)) args env)
  (destructuring-bind (bs width)
      args
    (let ((tyw (typecheck width env))
	  (tyb (typecheck bs env)))
      (ensure-fixed-width tyw)
      (ensure-fixed-width tyb)

      ;; evaluate the width, which must be statically determined
      (let ((w (ensure-static width env)))
	`(fixed-width-unsigned ,w)))))

(defun synthesise-fixed-width-constant (c width &optional (base 2))
  "Synthesise C as a constant with the given WIDTH.

The BASE used can be 2, 8, 10, or 16."
  (synthesise width :inexpression)
  (as-literal "'")
  (as-literal (ecase base
		(2  "b")
		(8  "o")
		(10 "d")
		(16 "x")))
  (let ((*print-base* base))
    (synthesise c :inexpression)))


(defmethod synthesise-sexp ((fun (eql 'extend-bits)) args (context (eql :inexpression)))
  (destructuring-bind (bs width)
      args
    (as-literal "{")
    (synthesise width :inexpression)
    (as-literal "{")
    (if (static-constant-p bs nil)
	;; value is a static constant, output it
	(let ((w (bitwidth (ensure-static bs nil) nil)))
	  (synthesise-fixed-width-constant bs w))

	;; value is an expression, synthesise it
	(progn
	  (synthesise bs :inexpression)))
    (as-literal "}}")))
