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


(defun extract-bits (b w n)
  "Extract W bits starting at B from N, where the least-significant bit is numbered 0"
  (logand (ash n (- (- (1+ b) w)))
	  (- (ash 1 w) 1)))


;; ---------- Code generator ----------

;; TODO Also have to be careful to compute the width expressions once, and
;; left-to-right in case of side effects.

;; TODO Check the edge cases of widths being zero, statically or at run-time.
;; The static case can be dealt with in the pattern compressor.
;; Widths less than one are always errors.

(defun bits-in-pattern (pat)
  "Count the number of bits PAT is trying to match.

If the number of bits can be computed at compile-time (i.e., all the
widths specifiers are constants), return the total number of bits as a
number. Otherwise, return an expression that computes the number at
run-time."
  (labels ((count-bits (pat)
	     (let ((p (car pat)))
	       (cond ((null p)
		      (list 0 '()))

		     ;; width specifier
		     ((listp p)
		      (let ((w (cadr p))
			    (rest (count-bits (cdr pat))))
			(if (numberp w)
			    (list (+ (car rest) w) (cadr rest))
			    (list (car rest) (cons w (cadr rest))))))

		     ;; single-bit match
		     (t
		      (let ((rest (count-bits (cdr pat))))
			(list (1+ (car rest)) (cadr rest))))))))
    (let ((bits (count-bits pat)))
      (if (null (cadr bits))
	  ;; no expressions, return the number of bits
	  (car bits)

	  ;; we have expressions, return the number-of-bits calculation
	  (if (equal (car bits) 0)
	      (if (equal (length (cadr bits)) 1)
		  (cadr bits)
		  `(+  ,@(cadr bits)))
	      `(+ ,(car bits) ,@(cadr bits)))))))


(defun compress-pattern (pat)
  "Compress PAT for more efficient compilation.

This combines adjacent instances of the same variable into a width
specifier, which extracts several bits in one go and so involves less
bit-twiddling."
  (if (null pat)
      '()
      (let* ((p (car pat))
	     (r (compress-pattern (cdr pat)))
	     (q (car r)))
	(cond ((listp p)
	       (cond ((listp q)
		      (if (equal (car p) (car q))
			  ;; (x 3) (x 4) -> (x 7)
			  (if (and (numberp (cadr p))
				   (numberp (cadr q)))
			      ;; both widths known, compute total now
			      (cons (list (car p) (+ (cadr p) (cadr q)))
				    (cdr r))
			      ;; both sizes not known, add computation
			      (cons (list (car p) `(+ ,(cadr p) ,(cadr q)))
				    (cdr r)))
			  ;; (x 3) (y 4) -> unchanged
			  (cons p r)))
		     ((symbolp q)
		      (if (equal (car p) q)
			  ;; (x 3) x -> (x 4)
			  (if (numberp (cadr p))
			      ;; width is known, increment it
			      (cons (list (car p) (1+ (cadr p)))
				    (cdr r))
			      ;; width is not known, add computation
			      (cons (list (car p) `(1+ ,(cadr p)))
				    (cdr r)))
			  ;; (x 3) y -> unchanged
			  (cons p r)))
		     (t
		      (cons p r))))
	      ((symbolp p)
	       (cond ((listp q)
		      (if (equal p (car q))
			  ;; x (x 4) -> (x 5)
			  (if (numberp (cadr q))
			      ;; width is known, increment it
			      (cons (list (car q) (1+ (cadr q)))
				    (cdr r))
			      ;; width is not known, add computation
			      (cons (list (car q) `(1+ ,(cadr q)))
				    (cdr r)))
			  ;; x (y 4) -> unchanged
			  (cons p r)))
		     ((symbolp q)
		      (if (equal p q)
			  ;; x x -> (x 2)
			  (cons (list p 2) (cdr r))
			  ;; x y -> unchanged
			  (cons p r)))
		     (t
		      (cons p r))))
	      (t
	       (cons p r))))))


(defun generate-match-bitfield-code (pattern var escape)
  "Construct a matching for PATTERN against variable VAR.

This function is the code generator for `destructuring-bind-bitfield'
that constructs the list of tests and assignments implied by the
pattern. A list of assignments is returned, with any errors resulting in
a nil return from the block designated by ESCAPE."
  (labels ((match-bit (pat bits known computed)
	     (if (null pat)
		 '()
		 (let ((p (car pat)))
		   (if (listp p)
		       ;; width specifier
		       (let ((n (car p))
			     (w (cadr p)))
			 (let ((exs (generate-extract bits w known computed)))
			   (cons `(setq ,n (+ ,(car exs)
					      (ash ,n ,w)))
				 (match-bit (cdr pat) bits (cadr exs) (caddr exs)))))

		       ;; single-bit match
		       (let ((exs (generate-extract bits 1 known computed)))
			 (case p
			   ;; a 0 or 1 matches that bit, or escapes the binding
			   ((0 1)
			    (cons `(if (not (equal ,(car exs) ,p))
				       (return-from ,escape nil))
				  (match-bit (cdr pat) bits (cadr exs) (caddr exs))))

			   ;; a - matches any bit
			   (-
			    (match-bit (cdr pat) bits (cadr exs) (caddr exs)))

			   ;; a symbol binds the bit in that variable
			   (otherwise
			    (cons `(setq ,p (+ ,(car exs)
					       (ash ,p 1)))
				  (match-bit (cdr pat) bits (cadr exs) (caddr exs))))))))))

	   ;; generate the code for the bit offset, collapsing ny zeros
	   ;; or empty lists
	   (generate-offset (bits known computed)
	     (if (null computed)
		 (if (equal known 0)
		     bits
		     `(- ,bits ,known))
		 (if (equal known 0)
		     `(- ,bits ,@computed)
		     `(- ,bits ,known ,@computed))))

	   ;; generate the call to `extract-bits`
	   (generate-extract (bits wanted known computed)
	     (if (numberp bits)
		 ;; known width, implies no computation and all numbers
		 (list `(extract-bits ,(- bits known) ,wanted ,var)
		       (+ known wanted)
		       computed)

		 ;; unknown width
		 (let ((offset (generate-offset bits known computed)))
		   (if (numberp wanted)
		       ;; known number of bits
		       (list `(extract-bits ,offset ,wanted ,var)
			     (+ known wanted)
			     computed)
		       ;; unknown number of bits
		       (list `(extract-bits ,offset ,wanted ,var)
			     known
			     (append computed (list wanted))))))))

    (let ((bits (bits-in-pattern pattern)))
      (if (numberp bits)
	  ;; number of bits is known at compile-time, use constants
	  (match-bit pattern (1- bits) 0 nil)

	  ;; number of bits must be computed
	  (let ((nob (gensym)))
	    `((let ((,nob (1- ,(if (equal (length bits) 1)
				   (car bits)
				   bits))))
		 ,@(match-bit pattern nob 0 nil))))))))


;; ---------- Macro ----------

(defmacro destructuring-bind-bitfield (pattern n &rest body)
  "Bind variables in PATTERN to the bits of N within BODY.

PATTERN is interpreted as a bitfield pattern with each element
corrsponding to a bit. The last element of the list corresponds to the
rightmpost (least significant) bit of N.

Each element of PATTERN is one of 0, 1, -, a list, or a symbol. 0 and
1 must match with 0 or 1 in the corrsponding bit of N. - matches
either 0 or 1 (the bit is ignored).

A symbol will be declared in a let binding and have the corresponding
bit bound to it. If the same symbol appears several times in PATTERN
then it receives all the bits in the obvious order. The bits taken
into a variable don't have to be consecutive.

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
	 (let-bindings (mapcar #'(lambda (s) (list s 0))
			       syms))
	 (escape (gensym))
	 (decls-and-tests (generate-match-bitfield-code pattern n escape)))
    `(block ,escape
       (let ,let-bindings
	 ,@decls-and-tests
	 ,@body))))
