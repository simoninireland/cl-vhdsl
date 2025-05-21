;; Type asserting and casting
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


;; ---------- Type assertions ----------

(defmethod typecheck-sexp ((fun (eql 'the)) args env)
  (destructuring-bind (ty val)
      args
    (let ((tyval (typecheck val env)))
      (ensure-subtype tyval ty env)

      ;; type is the type of the value itself, once we're
      ;; assured it's a sub-type of what was expected
      tyval)))


(defmethod synthesise-sexp ((fun (eql 'the)) args env context)
  (destructuring-bind (ty val)
      args
    (synthesise val env context)))


;; ---------- Type coercion (casting) ----------

(defmethod typecheck-sexp ((fun (eql 'coerce)) args env)
  (destructuring-bind (val ty)
      args
    (let ((vty (typecheck val env)))
      (if (and (fixed-width-p ty)
	       (fixed-width-p vty))
	  ;; can coerce fixed-width types
	  (let ((tyw (bitwidth-type (car ty) (cdr ty) env))
		(vtyw (bitwidth-type (car vty) (cdr vty) env)))
	    ty)

	  ;; can't coerce anything else for now
	  (error 'coercion-mismatch :expected ty :got vty
				    :hint "Make sure the two types are coercible.")))))


(defmethod synthesise-sexp ((fun (eql 'coerce)) args env context)
  (declare (optimize debug))

  (destructuring-bind (val ty)
      args
    (let* ((vty (typecheck val env))
	   (tyw (bitwidth-type (car ty) (cdr ty) env))
	   (vtyw (bitwidth-type (car vty) (cdr vty) env)))

      (cond
	;; type are both unsigned
	((and (unsigned-byte-p vty)
	      (unsigned-byte-p ty))
	 (cond ((= vtyw tyw)
		;; types have equal width, synthesise unchanged
		(synthesise val env context))

	       ((> vtyw tyw)
		;; value is wider, shrink it
		(synthesise `(bits ,val (- ,tyw 1) :end 0)
			    env context))

	       (t
		;; value is narrower, zero-extend
		(let ((zeros (- tyw vtyw)))
		  (synthesise `(make-bitfields (extend-bits 0 ,zeros)
					       ,val)
			      env context)))))

	;; type are both signed
	((and (signed-byte-p vty)
	      (signed-byte-p ty))
	 (cond ((= vtyw tyw)
		;; types have equal width, leave unchanged
		(synthesise val env context))

	       ((> vtyw tyw)
		;; value is wider, shrink it by using the same
		;; sign bit and the low-order bits
		(let* ((signbit (1- vtyw))
		       (startbit (- tyw 2)))
		  (synthesise `(make-bitfields (bit ,val ,signbit)
					       (bits ,val ,startbit :end 0))
			      env context)))

	       (t
		;; value is narrower, sign-extend
		(let ((signbit (1- vtyw))
		      (signs (- tyw vtyw)))
		  (synthesise `(make-bitfields (extend-bits (bit ,val ,signbit) ,signs)
					       ,val)
			      env context)))))

	;; value is unsigned, needed as signed
	((and (unsigned-byte-p vty)
	      (signed-byte-p ty))
	 (cond  ((= vtyw tyw)
		 ;; types have equal width, reduce value and zero-extend
		 (let ((reduced (- tyw 2)))
		   (synthesise `(make-bitfields 0
						(bits ,val ,reduced :end 0))
			       env context)))

		((> vtyw tyw)
		 ;; value is wider, shrink it
		 (let* ((startbit (- tyw 2)))
		   (synthesise `(make-bitfields 0
						(bits ,val ,startbit :end 0))
			       env context)))

		(t
		 ;; value is narrower, zero-extend
		 (let ((signs (- tyw vtyw)))
		   (synthesise `(make-bitfields (extend-bits 0 ,signs)
						,val)
			       env context)))))

	;; value is signed, needed as unsigned
	((and (signed-byte-p vty)
	      (unsigned-byte-p ty))
	 (cond ((= vtyw tyw)
		;; types have equal width, reduce value and zero-extend
		(let ((reduced (- tyw 2)))
		  (synthesise `(make-bitfields 0
					       (bits (if (< ,val 0)
							 (- ,val)
							 ,val)
						     ,reduced :end 0))

			      env context)))

	       ((> vtyw tyw)
		;; value is wider, shrink it
		(let ((reduced (- tyw 2)))
		  (synthesise `(make-bitfields 0
					       (bits (if (< ,val 0)
							 (- ,val)
							 ,val)
						     ,reduced :end 0))
			      env context)))

	       (t
		;; value is narrower, zero-extend
		(let ((signs (1+ (- tyw vtyw)))
		      (reduced (- vtyw 2)))
		  (synthesise `(make-bitfields (extend-bits 0 ,signs)
					       (bits (if (< ,val 0)
							 (- ,val)
							 ,val)
						     ,reduced :end 0))
			      env context)))))))))
