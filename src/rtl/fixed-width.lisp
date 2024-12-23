;; Fixed-width types
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

;; ---------- Fixed-width integers ----------

(deftype fixed-width-unsigned (width)
  "A fixed-width unsigned integer using WIDTH bits."
  `(integer 0 ,(round (1- (expt 2 width)))))


(deftype fixed-width-signed (width)
  "A fixed-width signed integer using WIDTH bits."
  `(integer ,(- (round (expt 2 (1- width))))
	    ,(round (1- (expt 2 (1- width))))))


(defun bitwidth (val env)
  "Return the number of bits needed to represent VAL in ENV.

VAL can be a constant number, a type, or a symbol, the latter having
their types looked-up in ENV."
  (flet ((bits-for-integer (val)
	   (multiple-value-bind (b res)
	       (ceiling (log val 2))
	     (let ((bits (if (= res 0.0)
			     (1+ b) ; add a bit if val is on a power-of-two boundary
			     b)))
	       bits))))

    (cond ((and (listp val)
		(or (eq (car val) 'fixed-width-unsigned)
		    (eq (car val) 'fixed-width-signed)))
	   (cadr val))

	  ((typep val 'integer)
	   (cond ((= val 0)
		  0)

		 ((> val 0)
		  (bits-for-integer val))

		 (t
		  (1+ (bits-for-integer (abs val))))))

	  ((symbolp val)
	   (get-width val env))

	  (t
	   (error 'not-synthesisable :fragment val)))))
