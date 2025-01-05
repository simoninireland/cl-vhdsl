;; Fixed-width types
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

;; ---------- Fixed-width integers ----------

(deftype fixed-width-unsigned (&optional width)
  "A fixed-width unsigned integer using WIDTH bits.

WIDTH may also be the symbol * if the exact width is unknown.
If omitted, WIDTH is assumed to be *."
  (if (eql width '*)
      '(integer 0 *)
      `(integer 0 ,(1- (ash 1 width)))))


(deftype fixed-width-signed (&optional width)
  "A fixed-width signed integer using WIDTH bits.

WIDTH may also be the symbol * if the exact width is unknown.
If omitted, WIDTH is assumed to be *."
  (if (eql width '*)
      '(integer * *)
      `(integer ,(- (ash 1 (1- width)))
		,(1- (ash 1 (1- width))))))


(defun fixed-width-p (ty)
  "Test whether TY is a fixed-width type."
  (and (not (null ty))
       (or (subtypep ty '(fixed-width-signed *))
	   (subtypep ty '(fixed-width-unsigned *)))))


(defun fixed-width-signed-p (ty)
  "Test whether TY is a signed fixed-width type."
  (and (not (null ty))
       (subtypep ty '(fixed-width-signed *))
       (not (subtypep ty '(fixed-width-unsigned *)))))


(defun ensure-fixed-width (ty)
  "Ensure that TY is a fixed-width integer."
  (unless (fixed-width-p ty)
    (error 'type-mismatch :expected '(fixed-width-unsigned
				      fixed-width-signed)
			  :got ty)))


;; ---------- Widening and least upper-bound ----------

(defun widen-fixed-width (ty w)
  "Widen TY to use W bits.

TY is returned if it is already at least as wide as W."
  (ensure-fixed-width ty)
  (let ((tytag (car ty)))
    (if (= (length ty) 0)
	;; widest already
	`(tytag)

	(let ((tyw (cadr ty)))
	  (if (eql tyw '*)
	      ;; widest already
	      `(,tytag *)

	      ;; new width should be the larger of the two
	      `(,tytag ,(max tyw w)))))))



(defun lub (ty1 ty2 env)
  "Return the least upper-bound of two types TY1 and TY2."
  (cond ((and (subtypep ty1 '(fixed-width-unsigned))
	      (subtypep ty2 '(fixed-width-unsigned)))
	 `(fixed-width-unsigned ,(max (bitwidth ty1 env)
				      (bitwidth ty2 env))))

	((and (subtypep ty1 '(fixed-width-signed))
	      (subtypep ty2 '(fixed-width-signed)))
	 `(fixed-width-signed ,(max (bitwidth ty1 env)
				    (bitwidth ty2 env))))

	((and (subtypep ty1 '(fixed-width-unsigned))
	      (subtypep ty2 '(fixed-width-signed)))
	 `(fixed-width-signed ,(max (1+ (bitwidth ty1 env))
				    (bitwidth ty2 env))))

	((and (subtypep ty1 '(fixed-width-signed))
	      (subtypep ty2 '(fixed-width-unsigned)))
	 (lub-fixed-width ty1 ty1 env))))


;; ---------- Width calculations ----------

(defgeneric bitwidth (val env)
  (:documentation "Return the number of bits needed to represent VAL in ENV."))


(defmethod bitwidth ((val integer) env)
  (flet ((bits-for-integer (val)
	   (multiple-value-bind (b res)
	       (ceiling (log val 2))
	     (let ((bits (if (= res 0.0)
			     ;; add a bit if val is on a
			     ;; power-of-two boundary
			     (1+ b)
			     b)))
	       bits))))
     (cond ((= val 0)
	    0)

	   ((> val 0)
	    (bits-for-integer val))

	   (t
	    (1+ (bits-for-integer (abs val)))))))


(defmethod bitwidth ((val symbol) env)
  (get-width val env))


(defmethod bitwidth ((val list) env)
  (if (member (car val) '(fixed-width-unsigned
			  fixed-width-signed))
      (if (or (= (length val) 0)
	      (eql (cadr val) '*))
	  *default-register-width*
	  (cadr val))))
