;; Bitwise access to variables
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


;; ---------- Single-bit access ----------

(defmethod typecheck-sexp ((fun (eql 'bit)) args env)
  (destructuring-bind (var bit)
      args
    (ensure-subtype (typecheck var env) 'fixed-width-unsigned)
    (ensure-subtype (typecheck bit env) '(fixed-width-unsigned 1))
    '(fixed-width-unsigned 1)))


(defmethod generalised-place-sexp-p ((fun (eql 'bit)) args env)
  t)


(defmethod synthesise-sexp ((fun (eql 'bit)) args context)
  (destructuring-bind (var bit)
      args
    (synthesise var :indeclaration)
    (as-literal "[ ")
    (synthesise bit :inexpression)
    (as-literal " ]")))


(defmethod lispify-sexp ((fun (eql 'bit)) args env)
  (destructuring-bind (var bit)
      args
    `(logand (ash ,(lispify var) (- ,bit))
	     1)))


;; ---------- Multi-bit access ----------

(defun compute-end-bit (start end width)
  "Compute the end bit given START, END, and WIDTH."
  (if (null end)
      (if width
	  ;; no end set, extract from width if present
	  (progn
	    (setq end (1+ (- start width)))
	    (if (< end 0)
		(error 'width-mismatch :expected 0
				       :got end
				       :hint "Width greater than the number of remaining bits")
		end))

	  ;; default is to the end of the pattern
	  0)

      (if width
	  ;; if both are set, width and end must agree
	  (if (/= width (1+ (- start end)))
	      (error 'width-mismatch :expected (1+ (- start end))
				     :gopt width
				     :hint "Explicit width does not agree with start and end positions")
	      end)

	  ;; otherwise just use the given end
	  end)))


(defmethod generalised-place-sexp-p ((fun (eql 'bits)) args env)
  t)


(defmethod typecheck-sexp ((fun (eql 'bits)) args env)
  (destructuring-bind (var start &key end width)
      args
    (let ((tyvar (typecheck var env)))
      (setq end (compute-end-bit start end width))

      (let ((l (1+ (- start end)))
	    (vw (bitwidth tyvar env)))
	(when (> l vw)
	  (error 'width-mismatch :expected vw
				 :got l
				 :hint "Width greater than base variable"))

	;; width is the nunmber of bits extracted
	`(fixed-width-unsigned ,l)))))


(defmethod synthesise-sexp ((fun (eql 'bits)) args (context (eql :inexpression)))
  (destructuring-bind (var start &key end width)
      args
    (setq end (compute-end-bit start end width))

    (synthesise var :inassignment)
    (as-literal "[ ")
    (synthesise start :inexpression)
    (as-literal " : ")
    (synthesise end :inexpression)
    (as-literal " ]")))


(defmethod lispify-sexp ((fun (eql 'bits)) args env)
  (destructuring-bind (var start &key end width)
      args
    (let ((l (eval-in-static-environment `(+ 1 (- ,start ,end)) env)))
      `(logand (ash ,(lispify var) (- ,end)) (1- (ash 1 ,l))))))
