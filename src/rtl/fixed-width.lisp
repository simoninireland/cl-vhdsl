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

(defun fixed-width-p (ty)
  "Test whether TY is a fixed-width type."
  (and (not (null ty))
       (or (subtypep ty 'signed-byte)
	   (subtypep ty 'unsigned-byte))))


(defun signed-byte-p (ty)
  "Test whether TY is a signed fixed-width type."
  (and (not (null ty))
       (subtypep ty 'signed-byte)
       (not (subtypep ty 'unsigned-byte))))


(defun ensure-fixed-width (ty)
  "Ensure that TY is a fixed-width integer."
  (unless (fixed-width-p ty)
    (error 'type-mismatch :expected '(unsigned-byte
				      signed-byte)
			  :got ty)))


(defmethod expand-type-parameters-type ((ty (eql 'unsigned-byte)) args env)
  (if (null args)
      ty
      (let ((bounds (car args)))
	(if (eql bounds '*)
	    '(unsigned-byte *)
	    `(unsigned-byte ,(eval-in-static-environment bounds env))))))


(defmethod expand-type-parameters-type ((ty (eql 'signed-byte)) args env)
  (if (null args)
      ty
      (let ((bounds (car args)))
	(if (eql bounds '*)
	    '(unsigned-byte *)
	    `(signed-byte ,(eval-in-static-environment bounds env))))))



;; ---------- Least upper-bound ----------

(defmethod lub-type ((ty1tag (eql 'unsigned-byte)) ty1args
		     (ty2tag (eql 'unsigned-byte)) ty2args
		     env)
  `(unsigned-byte ,(max (car ty1args) (car ty2args))))


(defmethod lub-type ((ty1tag (eql 'signed-byte)) ty1args
		     (ty2tag (eql 'signed-byte)) ty2args
		     env)
  `(signed-byte ,(max (car ty1args) (car ty2args))))


(defmethod lub-type ((ty1tag (eql 'unsigned-byte)) ty1args
		     (ty2tag (eql 'signed-byte)) ty2args
		     env)
  `(signed-byte ,(max (1+ (car ty1args)) (car ty2args))))


(defmethod lub-type ((ty1tag (eql 'signed-byte)) ty1args
		     (ty2tag (eql 'unsigned-byte)) ty2args
		     env)
  `(signed-byte ,(max (car ty1args) (1+ (car ty2args)))))



;; ---------- Widths ----------

(defun bits-for-integer (val)
  "Return the number of bits needed to represent VAL."
  (flet ((bfi (val)
	   (multiple-value-bind (b res)
	       (ceiling (log val 2))
	     (let ((bits (max (if (= res 0.0)
				  ;; add a bit if val is on a
				  ;; power-of-two boundary
				  (1+ b)
				  b)
			      1)))	; always need at least one bit
	       bits))))
    (cond ((= val 0)
	   1)

	  ((> val 0)
	   (bfi val))

	  (t
	   (1+ (bfi (abs val)))))))


(defmethod bitwidth-type ((tytag (eql 'unsigned-byte)) tyargs env)
  (car tyargs))


(defmethod bitwidth-type ((tytag (eql 'signed-byte)) tyargs env)
  (car tyargs))
