;; bitist.lisp: Lists of bits, individually manipulable
;;
;; Copyright (C) 2023 Simon Dobson
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

(in-package :cl-vhdsl)

;; We treat binary bitfields as lists by placing the low-order bit at the
;; start of the list. This matches the use of functions like nth, in that
;; (nth 0 l) will extract the 0'th bit from an exploded number. It is
;; however unnatural with respect to how we write numbers, where the
;; low-order bit comes at the end. It also doesn't match the representation
;; used in cl-bitfields, which matches the way numbers are written.

(defun get-bit (v i)
  "Return bit I of V."
  (ash (logand v (ash 1 i)) (- i)))

(defun set-bit (v i b)
  "Return a new value of V with bit I set to B."
  (let ((mask (ash 1 i)))
    (if (equal b 1)
	(logior v mask)
	(if (equal (logand v mask) 0)
	    v
	    (logxor v mask)))))

(defun explode-bitfield (v)
  "Explode the bits of V into a list.

The low-order bit appears at the start of the list."
  (labels ((expl-bit (v)
	     (if (equal v 0)
		 '()
		 (append (list (logand v 1))
			 (expl-bit (ash v -1))))))
    (let ((bs (expl-bit v)))
      (if (null bs)
	  '(0)
	  bs))))

(defun explode-bitfield-indices (v is)
  "Extract the bits of V indexed by indices in IS.

The low-order bit is numbered 0."
  (mapcar (lambda (i)
	    (get-bit v i))
	  is))

(defun implode-bitfield (bs)
  "Implode the bits in BS into a value.

The low-order bit appears at the start of the list."
  (labels ((add-bits (bs)
	     (if (null bs)
		 0
		 (+ (car bs)
		    (ash  (add-bits (cdr bs)) 1)))))
    (add-bits bs)))

(defun implode-bitfield-indices (vs is)
  "Create a bitfield with the bits in VS at positions indexed in IS.

The low-order bit is number 0. Bits not indexed are set to 0."
  (let ((f 0))
    (mapc (lambda (i)
	    (setf f (set-bit f i (nth i vs))))
	  is)
    f))

(defun fix-bitfield-width (bf w)
  "Fix the width of bitfield BF to W bits by padding with zeros in the high-order bit positions.

The bitfield is returned unchanged  if it is already of at least width W."
  (let ((zs (- w (length bf))))
    (if (> zs 0)
	(append bf (make-list zs :initial-element 0))
	bf)))

(defun changed-bit-indices (o n)
  "Return a list of indices of bits changed between O and N."
  (flet ((make-equal-length (p q)
	   (let ((pq (max (length p) (length q))))
	     (list (fix-bitfield-width p pq)
		   (fix-bitfield-width q pq)))))
    (let* ((obs (explode-bitfield o))
	   (nbs (explode-bitfield n))
	   (onbs (make-equal-length obs nbs))
	   (is (iota (length (car onbs)) :start 0))
	   (changed (mapcar (lambda (ob nb i)
			      (if (not (equal ob nb))
				  (list i)
				  '()))
			    (car onbs) (cadr onbs) is)))
      (apply #'append changed))))

(defun update-bit-indices (o vs is)
  "Update the bits of O at indices IS with the values in VS."
  (let ((n o))
    (mapc (lambda (i v)
	    (setf n (set-bit n i v)))
	  is vs)
    n))
