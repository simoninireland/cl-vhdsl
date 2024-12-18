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

(deftype fixed-width-integer (width)
  "A fixed-width integer using WIDTH bits."
  `(integer 0 ,(round (1- (expt 2 width)))))


(defun bitwidth (val env)
  "Return the number of bits needed to represent VAL in ENV.

VAL can be a constant number, a type, or a symbol, the latter having
their types looked-up in ENV."
  (cond ((and (listp val)
	      (eq (car val) 'fixed-width-integer))
	 (cadr val))

	((typep val 'integer)
	 (if (= val 0)
	     0

	     ;; compute the number of bits needed to store the value
	     (multiple-value-bind (b res)
		 (ceiling (log val 2))
	       (let ((bits (if (= res 0.0)
			       (1+ b) ; add a bit if val is on a power-of-two boundary
			       b)))
		 bits))))

	((symbolp val)
	 (get-width val env))

	(t
	 (error 'not-synthesisable :fragment val))))
