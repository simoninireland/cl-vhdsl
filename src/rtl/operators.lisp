;; Synthesisable operators
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


(defun typecheck-addition (args env)
  "Type-check an addition or subtraction of ARGS in ENV.

The width of the resulting number is the width of the widest argument
plus the number of other arguments."
  (let ((tys (mapcar (rcurry #'typecheck env) args)))
    (dolist (ty tys)
      (ensure-fixed-width ty))

    (let* ((ws (mapcar (rcurry #'bitwidth env) tys))
	   (minus (some #'fixed-width-signed-p tys))
	   (w (+ (apply #'max ws)
		 (1- (length args)))))
      (if minus
	  `(fixed-width-signed ,w)
	  `(fixed-width-unsigned ,w)))))


(defmethod typecheck-sexp ((fun (eql '+)) args env)
  (typecheck-addition args env))


(defmethod synthesise-sexp ((fun (eql '+)) args (as (eql :rvalue)))
  (as-infix '+ args as))


(defmethod lispify-sexp ((fun (eql '+)) args env)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg env))
		      args)))
    `(+ ,@vals)))


(defmethod typecheck-sexp ((fun (eql '-)) args env)
  ;; we force subtractions to be signed
  (let ((ty (typecheck-addition args env)))
    `(fixed-width-signed ,(bitwidth ty env))))


(defmethod synthesise-sexp ((fun (eql '-)) args (as (eql :rvalue)))
  (if (= (length args) 1)
      ;; unary minus
      (progn
	(format *synthesis-stream* "(-~a)" (car args)))

      ;; application
      (as-infix '- args as)))


(defmethod synthesise-sexp ((fun (eql '*)) args (as (eql :rvalue)))
  (as-infix '* args as))


(defmethod lispify-sexp ((fun (eql '-)) args env)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg env))
		      args)))
    `(- ,@vals)))


;; Verilog provides left and right shift operators; Common Lisp uses ash
;; and switches depending on the sign of the shift (negative for right).
;; That behaviour seems impossible to synthesise without using an extra
;; register, so we provide two different operators instead. (This will
;; change if I can figure out a way to synthesise ash.)
;;
;; The destructuring is to ensure there are only two arguments.
;;
;; The result is always unsigned.

(defmethod typecheck-sexp ((fun (eql '<<)) args env)
  (destructuring-bind (val offset)
      args
    (let ((tyval (typecheck val env))
	  (tyoffset (typecheck offset env)))
      (ensure-fixed-width tyval)
      (ensure-fixed-width tyoffset)

      ;; the width is the width of the value plus the
      ;; maximum number that can be in the offset
      `(fixed-width-unsigned ,(+ (bitwidth tyval env)
				 (1- (round (expt 2 (bitwidth tyoffset env)))))))))


(defmethod synthesise-sexp ((fun (eql '<<)) args (as (eql :rvalue)))
  (destructuring-bind (val offset)
      args
    (as-infix '<< args as)))


(defmethod lispify-sexp ((fun (eql '<<)) args env)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg env))
		      args)))
    `(ash ,@vals)))


(defmethod typecheck-sexp ((fun (eql '>>)) args env)
  (destructuring-bind (val offset)
      args
    (let ((tyval (typecheck val env))
	  (tyoffset (typecheck offset env)))
      (ensure-fixed-width tyval)
      (ensure-fixed-width tyoffset)

      ;; the width is the width of the value minus the
      ;; maximum number that can be in the offset
      `(fixed-width-unsigned ,(- (bitwidth tyval env)
				 (1- (round (expt 2 (bitwidth tyoffset env)))))))))


(defmethod synthesise-sexp ((fun (eql '>>)) args (as (eql :rvalue)))
  (destructuring-bind (val offset)
      args
    (as-infix '>> args as)))


(defmethod lispify-sexp ((fun (eql '>>)) args env)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg env))
		      args)))
    `(ash ,(car vals) (- ,(cadr vals)))))
