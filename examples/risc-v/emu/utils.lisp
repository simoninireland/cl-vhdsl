;; 32-bit integer-only RISC-V core helper macros and functions
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

(in-package :cl-vhdsl/examples/risc-v/emu)

(defun ensure-one-of (&rest args)
  "Ensure exactly one of ARGS is non-nil."
  (let ((non-nils (foldr (lambda (n v)
			   (if v
			       (1+ n)
			       n))
			 args 0)))

    (cond ((= non-nils 0)
	   (error "Missing required argument"))
	  ((> non-nils 1)
	   (error "Too many arguments"))
	  (t t))))


(defun bits (expr start &key width end)
  "Extract bits from EXPR starting at bit START.

The number of bits can be given by WIDTH for a number of bits
or END for the last bit. If neither is supplied then the single
START bit is returned."
  (cond ((and (null width)
	      (null end))
	 (setq width 1)
	 (setq end start))

	((null end)
	 (setq end (1+ (- start width))))

	((null width)
	 (setq width (1+ (- start end)))))

  (logand (ash expr (- end))
	  (mask width)))


(defun write-bits (val var start &key width end)
  "Set the specified bits in VAR to VAL, returning the new value."
  (cond ((and (null width)
	      (null end))
	 (setq width 1)
	 (setq end start))

	((null end)
	 (setq end (1+ (- start width))))

	((null width)
	 (setq width (1+ (- start end)))))

  (let ((masked-val (logand val (mask width)))
	(lower (if (= end 0)
		   0
		   (logand var (mask end))))
	(upper (ash var (- (1+ start)))))

    (+ (ash upper (1+ start))
       (ash masked-val (1+ (- start width)))
       lower)))


(defun copy-bit (b w)
  "Return a number that is W copies of the bit B."
  (if (= 0 b)
      0
      (1- (ash b w))))


(defun mask (w)
  "Return a mask of W 1s."
  (copy-bit 1 w))


(defun asserted-p (b)
  "Test whether bit B is 1."
  (= b 1))


(defmacro with-bitfields (fields expr &body body)
  "Bind FIELDS of EXPR in BODY.

Each element of FIELDS should be a list consisting of a symbol and
a width. These variables are created by extracting the corresponding bits
from EXPR, with the last variable coming from the lowest-order bits.
The variables are then in scope (as generalised places) in BODY."
  (with-gensyms (val)
    (labels ((field-variables (fields start)
	       (if (null fields)
		   ()
		   (let ((field (car fields)))
		     (destructuring-bind (var width)
			 field
		       (cons `(,var (bits ,val ,start :width ,width))
			     (field-variables (cdr fields) (- start width)))))))

	     (fields-width (fields)
	       (foldr (lambda (i nw)
			(+ (cadr nw) i))
		      fields 0)))

      (let ((vars (field-variables fields (1- (fields-width fields)))))
	`(let ((,val ,expr))
	   (symbol-macrolet ,vars
	     ,@body))))))


(defun interleave (&rest ls)
  "Construct a list from the interleaved values of the lists LS.

For example, (INTERLEAVE '(1 2 3) '(a b c)) will construct
(1 a 2 b 3 c). The lists must all be of equal length."
  (labels ((take-cars (ls)
	     (cond ((every #'null ls)
		    ())
		   ((some #'null ls)
		    (error "Lists not the same length"))
		   (t
		    (let ((cs (mapcar #'car ls)))
		      (append cs (take-cars (mapcar #'cdr ls))))))))

    (if (null ls)
	()
	(take-cars ls))))


(defun twos-complement (n w)
  "If N is negative under two's complement, return the negative number.

Otherwise return N unchanged. W is the width of N in bits."
  (if (asserted-p (1bit n (1- w)))
      ;; number is negative, invert all bits, add 1, and make negative
      (- (1+ (logxor (logand n (mask (1- w))) (mask (1- w)))))

      ;; number is positive, return unchanged
      n))


(defmacro make-bitfields (ty &rest fields)
  "Compose FIELDS into a value of type TY.

Each element of FIELDS should be a list consisting of an expression
and a width. The bitfield is constructed by concatenating the bits
from the expressions, with the highest-order bits coming first. The
bitfield is then coerced to be of type TY."
  (labels ((field-bits (field width)
	     "Turn FIELD into a declaration with WIDTH declaration as a mask."
	     (let ((val (car field)))
	       (let ((var (gensym)))
		 ;; mask generated from the variable name of WIDTH
		 `(,var (logand ,val (1- (ash 1 ,(car width))))))))

	   (field-widths (field)
	     "Turn FIELD into a declaration of its width in a new variable."
	     (destructuring-bind (val width)
		 field
	       (let ((var (gensym)))
		 `(,var ,width))))

	   (field-shifts (vars widths result shift-p)
	     "Turn lists of field and width variables into an expression."
	     (if (null vars)
		 nil
		 (let ((var (car vars)))
		   (let* ((width (car widths))
			  (nextexpr (field-shifts (cdr vars) (cdr widths) result t))
			  (shifter (if shift-p
				       `(ash ,result ,width)
				       result)))
		     (cons `(setq ,result (+ ,var ,shifter))
			   nextexpr))))))

    (let* ((widths (mapcar #'field-widths fields))
	   (vals (mapcar #'field-bits fields widths))
	   (val-vars (mapcar #'car vals))
	   (width-vars (mapcar #'car widths)))

      (with-gensyms (bitfield bitfield-width)
	`(let* (,@(interleave widths
			      vals)
		(,bitfield-width (+ ,@ (mapcar #'cadr widths)))
		(,bitfield 0))
	   ;; add the results into the bitfield
	   ,@(field-shifts val-vars width-vars bitfield nil)

	   ,(if (signed-byte-p ty)
		;; signed type, do the two's-complement conversion
		`(twos-complement ,bitfield ,bitfield-width)

		;; otherwise leave as-is
		bitfield))))))
