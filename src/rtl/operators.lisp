;; Synthesisable operators
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

(defun ensure-number-of-arguments (fun args n)
  "Ensure that ARGS has exactly N arguments.

A NOT-SYNTHESISABLE error is raised if the arguments are wrong."
   (if (/= (length args) n)
      (error 'not-synthesisable :hint (format nil "Operator needs exactly ~a arguments" n))))


;; ---------- Maths ----------

(defun typecheck-addition (args env)
  "Type-check an addition or subtraction of ARGS in ENV.

The width of the resulting number is the width of the widest argument
plus the number of other arguments."
  (let ((tys (mapcar (rcurry #'typecheck env) args)))
    (dolist (ty tys)
      (ensure-fixed-width ty))

    (let* ((ws (mapcar (rcurry #'bitwidth env) tys))
	   (minus (some #'signed-byte-p tys))
	   (w `(+ (max ,@ws)
		  (1- ,(length args)))))

      (if minus
	  `(signed-byte ,w)
	  `(unsigned-byte ,w)))))


(defun fold-constant-expressions-addition (fun args)
  "Fold expressions in addition operator FUN applied to ARGS."
  (labels ((fold-constants (total l)
	     (if (null l)
		 (list total '())
		 (let ((v (car l)))
		   (if (integerp v)
		       ;; fold-in the constant, reduce the rest
		       (let ((s (apply fun (list v total))))
			 (fold-constants s (cdr l)))

		       ;; reduce the rest with the non-constant on the front
		       (destructuring-bind (ct cl)
			   (fold-constants total (cdr l))
			 (list ct (cons v cl))))))))

    (destructuring-bind (total remaining)
	(fold-constants 0 args)
      (if (null remaining)
	  total
	  (if (= total 0)
	      `(,fun ,@remaining)
	      `(,fun ,total ,@remaining ))))))


(defmethod typecheck-sexp ((fun (eql '+)) args env)
  (typecheck-addition args env))


(defmethod fold-constant-expressions-sexp ((fun (eql '+)) args)
  (fold-constant-expressions-addition '+args))


(defmethod synthesise-sexp ((fun (eql '+)) args env (context (eql :inexpression)))
  (as-infix '+ args env))


(defmethod lispify-sexp ((fun (eql '+)) args env)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg env))
		      args)))
    `(+ ,@vals)))


(defmethod typecheck-sexp ((fun (eql '-)) args env)
  (if (= (length args) 1)
      ;; unary negation
      (let ((ty (typecheck (car args) env)))
	`(signed-byte ,(1+ (bitwidth ty env))))

      ;; general substraction
      ;; we force subtractions to be signed
      (let ((ty (typecheck-addition args env)))
	`(signed-byte ,(bitwidth ty env)))))


(defmethod fold-constant-expressions-sexp ((fun (eql '-)) args)
  (fold-constant-expressions-addition '- args))


(defmethod synthesise-sexp ((fun (eql '-)) args env (context (eql :inexpression)))
  (if (= (length args) 1)
      ;; unary minus
      (as-literal (format nil "(-~a)" (car args)))

      ;; application
      (as-infix '- args env)))


(defmethod synthesise-sexp ((fun (eql '*)) args env (context (eql :inexpression)))
  (as-infix '* args env))


(defmethod lispify-sexp ((fun (eql '-)) args env)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg env))
		      args)))
    `(- ,@vals)))


;; ---------- Shifts ----------

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
  (ensure-number-of-arguments fun args 2)

  (destructuring-bind (val offset)
      args
    (let ((tyval (typecheck val env))
	  (tyoffset (typecheck offset env)))
      (ensure-fixed-width tyval)
      (ensure-fixed-width tyoffset)

      ;; the width is the width of the value plus the
      ;; maximum number that can be in the offset
      `(unsigned-byte ,(+ (bitwidth tyval env)
			  (1- (ash 1 (bitwidth tyoffset env))))))))


(defmethod fold-constant-expressions-sexp ((fun (eql '<<)) args)
  (destructuring-bind (val offset)
      args
    (if (and (integerp val)
	     (integerp offset))
	(ash val offset)
	`('<< ,val ,offset))))


(defmethod synthesise-sexp ((fun (eql '<<)) args env (context (eql :inexpression)))
  (destructuring-bind (val offset)
      args
    (as-infix '<< args env)))


(defmethod lispify-sexp ((fun (eql '<<)) args env)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg env))
		      args)))
    `(ash ,@vals)))


;; We should probably do sign extension here, like Lisp does

(defmethod typecheck-sexp ((fun (eql '>>)) args env)
  (ensure-number-of-arguments fun args 2)

  (destructuring-bind (val offset)
      args
    (let ((tyval (typecheck val env))
	  (tyoffset (typecheck offset env)))
      (ensure-fixed-width tyval)
      (ensure-fixed-width tyoffset)

      ;; use the width of the value as the maximum width, since
      ;; shifting right can only make it smaller
      `(unsigned-byte ,(bitwidth tyval env)))))


(defmethod fold-constant-expressions-sexp ((fun (eql '>>)) args)
  (destructuring-bind (val offset)
      args
    (if (and (integerp val)
	     (integerp offset))
	(ash val (- offset))
	`('<< ,val ,offset))))


(defmethod synthesise-sexp ((fun (eql '>>)) args env (context (eql :inexpression)))
  (destructuring-bind (val offset)
      args
    (as-infix '>> args env)))


(defmethod lispify-sexp ((fun (eql '>>)) args env)
  (let ((vals (mapcar (lambda (arg)
			(lispify arg env))
		      args)))
    `(ash ,(car vals) (- ,(cadr vals)))))


;; ---------- Bitwise operators ----------

(defun typecheck-bitwise-operator (args env)
  "Typecheck the arguments ARGS to a logical operator in ENV."
  (let ((ty (foldr (lambda (ty1 arg)
		     (lub ty1 (typecheck arg env) env))
		   args nil)))
    (ensure-subtype ty 'unsigned-byte env)

    ty))


(defmethod typecheck-sexp ((fun (eql 'logand)) args env)
  (typecheck-bitwise-operator args env))


(defmethod fold-constant-expressions-sexp ((fun (eql 'logand)) args)
  (fold-constant-expressions-addition 'logand args))


(defmethod synthesise-sexp ((fun (eql 'logand)) args env (context (eql :inexpression)))
  (as-infix '& args env))


(defmethod typecheck-sexp ((fun (eql 'logior)) args env)
  (typecheck-bitwise-operator args env))


(defmethod fold-constant-expressions-sexp ((fun (eql 'logior)) args)
  (fold-constant-expressions-addition 'logior args))


(defmethod synthesise-sexp ((fun (eql 'logior)) args env (context (eql :inexpression)))
  (as-infix '|\|| args env))


(defmethod typecheck-sexp ((fun (eql 'logxor)) args env)
  (typecheck-bitwise-operator args env))


(defmethod fold-constant-expressions-sexp ((fun (eql 'logxor)) args)
  (fold-constant-expressions-addition 'logxor args))


(defmethod synthesise-sexp ((fun (eql 'logxor)) args env (context (eql :inexpression)))
  (as-infix '^ args env))


;; ---------- Logical ----------

(defun typecheck-logical-operator (args env)
  "Typecheck the arguments ARGS to a logical operator in ENV.

All arguments must be booleans."
  (mapc (lambda (arg)
	  (let ((ty (typecheck arg env)))
	    (ensure-boolean ty env)))
	args)
  '(unsigned-byte 1))


(defmethod typecheck-sexp ((fun (eql 'and)) args env)
  (typecheck-logical-operator args env))


(defmethod synthesise-sexp ((fun (eql 'and)) args env (context (eql :inexpression)))
  (as-infix '&& args env))


(defmethod typecheck-sexp ((fun (eql 'or)) args env)
  (typecheck-logical-operator args env))


(defmethod synthesise-sexp ((fun (eql 'or)) args env (context (eql :inexpression)))
  (as-infix '|\|\|| args env))


(defmethod typecheck-sexp ((fun (eql 'not)) args env)
  (ensure-number-of-arguments 'not args 1)
  (ensure-boolean (car args) env)
  '(unsigned-byte 1))


(defmethod synthesise-sexp ((fun (eql 'not)) args env (context (eql :inexpression)))
  (as-literal "(")
  (as-literal "!")
  (synthesise (car args) env :inexpression)
  (as-literal ")"))
