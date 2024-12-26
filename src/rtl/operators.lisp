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


(defmethod synthesise-sexp ((fun (eql '+)) args (as (eql :rvalue)))
  (format *synthesis-stream* "(")
  (dolist (i (iota (length args)))
    (synthesise (elt args i) :rvalue)
    (if (< i (1- (length args)))
	(format *synthesis-stream* " + ")))
  (format *synthesis-stream* ")"))


(defmethod synthesise-sexp ((fun (eql '-)) args (as (eql :rvalue)))
  (if (= (length args) 1)
      ;; unary minus
      (progn
	(format *synthesis-stream* "(-~a)" (car args)))

      ;; application
      (progn
	(format *synthesis-stream* "(")
	(dolist (i (iota (length args)))
	  (synthesise (elt args i) :rvalue)
	  (if (< i (1- (length args)))
	      (format *synthesis-stream* " - ")))
	(format *synthesis-stream* ")"))))


(defmethod synthesise-sexp ((fun (eql '*)) args (as (eql :rvalue)))
  (format *synthesis-stream* "(~{~a ~^* ~})" (mapcar (lambda (form)
						       (synthesise form as))
						     args)))


;; Verilog provides left and right shift operators; Common Lisp uses ash
;; and switches depending on the sign of the shift (negative for right).
;; That behaviour seems impossible to synthesise without using an extra
;; register, so we provide two different operators instead. (This will
;; change if I can figure out a way to synthesise ash.)

(defmethod synthesise-sexp ((fun (eql '<<)) args (as (eql :rvalue)))
  (destructuring-bind (val offset)
      args
    (format *synthesis-stream* "(~a << ~a)"
	    (synthesise val as)
	    (synthesise offset as))))


(defmethod synthesise-sexp ((fun (eql '>>)) args (as (eql :rvalue)))
  (destructuring-bind (val offset)
      args
    (format *synthesis-stream* "(~a >> ~a)"
	    (synthesise val as)
	    (synthesise offset as))))
