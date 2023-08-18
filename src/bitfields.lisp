;; bitfields.lisp: Manipulate a number to extract bitfields
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

;; ---------- Helper functions ----------

(defun extract-symbols (l)
  "Extract a list of symbols appearing in L.

This identifies variables (symbols) to be matched against bits.
Elements 0, 1, and - are ignored. A list should contain a symbol name
and a bit width. Any other elements cause an error."
  (if (null l)
      '()
      (let ((syms (extract-symbols (cdr l)))
	    (s (car l)))
	(cond ((member s '(0 1 -))
	       syms)
	      ((symbolp s)
	       (if (member s syms)
		   syms
		   (cons s syms)))
	      ((listp s)
	       (let ((q (car s)))
		 (cond ((not (symbolp q))
			(error "(~S ...) cannot appear in a bitmap pattern" q))
		       ((member q syms)
			syms)
		       (t
			(cons q syms)))))
	      (t
	       (error "~S cannot appear in a bitmap pattern" s))))))


(defun extract-bit (b n)
  "Extract bit B from N, where the least-significant bit is numbered 0."
  (logand (ash n (- b)) 1))


(defun extract-bits (b w n)
  "Extract W bits starting at B from N, where the least-significant bit is numbered 0"
  (logand (ash n (- (- (1+ b) w)))
	  (- (ash 1 w) 1)))


;; ---------- Code generator ----------

(defun bits-in-pattern (pat)
  "Count the number of bits PAT is trying to match."
  (labels ((count-bits (pat)
	     (let ((p (car pat)))
	       (cond ((null p)
		      0)
		     ((listp p)
		      (+ (cadr p) (count-bits (cdr pat))))
		     (t
		      (1+ (count-bits (cdr pat))))))))
    (count-bits pat)))

(defun generate-match-bitfield-code (pattern var escape)
  "Construct a matching for PATTERN against variable VAR.

This function is the code generator for `destructuring-bind-bitfield'
that constructs the list of tests and assignments implied by the
pattern. A list of bit tests is returned, with any errors resulting in
a nil return from the block designated by ESCAPE."
  (labels ((match-bit (pat l)
	     (if (null pat)
		 '()
		 (let ((p (car pat)))
		   (if (listp p)
		       (let ((n (car p))
			     (w (cadr p)))
			 (cons `(setq ,n (+ (extract-bits ,l ,w ,var)
					    (ash ,n ,w)))
			       (match-bit (cdr pat) (- l w))))
		       (case p
			 ((0 1) (cons `(if (not (equal (extract-bit ,l ,var) ,p))
					   (return-from ,escape nil))
				      (match-bit (cdr pat) (1- l))))
			 (- (match-bit (cdr pat) (1- l)))
			 (otherwise
			  (cons `(setq ,p (+ (extract-bit ,l ,var)
					     (ash ,p 1)))
				(match-bit (cdr pat) (1- l))))))))))
    (match-bit pattern (- (bits-in-pattern pattern) 1))))


;; ---------- Macro ----------

(defmacro destructuring-bind-bitfield (pattern n &rest body)
  "Bind variables in PATTERN to the bits of N within BODY.

PATTERN is interpreted as a bitfield pattern with each element
corrsponding to a bit. The last element of the list corresponds to the
rightmpost (least significant) bit of N.

Each element of PATTERN is one of 0, 1, -, a list, or a symbol. 0 and
1 must match with 0 or 1 in the corrsponding bit of N. - matches
either 0 or 1 (the bit is ignored). A symbol will be declared in a let
binding and have the corresponding bit bound to it. If the same symbol
appears several times in PATTERN then it receives all the bits in the
obvious order. The bits taken into a variable don't have to be
consecutive.

A list element should take the form (x w) where x is a symbol and w is
a number. This creates a symbol with the name x that consumes w bits
from the pattern. As with symbols binding single bits, the same symbol
can appear in several width specifiers, or alone to bind a single bit.

For example, the pattern '(x x x 0), when matched against the number
10 (#2r1010), will bind x to 5 (#2r101), the bits in the corresponding
positions. The same pattern matched against 11
(#2r1011) will fail because the rightmost bits of the number and the
pattern don't match. The pattern '((x 3) 0) is the same as the pattern
'(x x x 0), and matches x against three consecutive bits.

DESTRUCTURING-BIND-BITFIELD returns the value of executing BODY in an
environment extended by the extracted variables (if any), or nil if
PATTERN doesn't match N."
  (let* ((syms (extract-symbols pattern))
	 (let-bindings (mapcar #'(lambda (s) (list s 0)) syms))
	 (escape (gensym))
	 (decls-and-tests (generate-match-bitfield-code pattern n escape)))
    `(block ,escape
       (let ,let-bindings
	 ,@decls-and-tests
	 ,@body))))
