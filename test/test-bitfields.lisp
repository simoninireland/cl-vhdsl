;; test-bitfields.lisp: Test bitfield extraction and binding
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

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl)


;; ---------- Extracting symbols from bitfield patterns ----------

(test test-symbols
  "Test we can extract symbols from a pattern."
  ;; null patterns
  (is (null (cl-vhdsl::extract-symbols '())))
  (is (null (cl-vhdsl::extract-symbols '(0))))
  (is (null (cl-vhdsl::extract-symbols '(1))))
  (is (null (cl-vhdsl::extract-symbols '(-))))
  (is (null (cl-vhdsl::extract-symbols '(0 1))))
  (is (null (cl-vhdsl::extract-symbols '(0 - 1))))

  ;; single symbols (including weird ones)
  (is (equal (cl-vhdsl::extract-symbols '(x)) '(x)))
  (is (equal (cl-vhdsl::extract-symbols '(x 0)) '(x)))
  (is (equal (cl-vhdsl::extract-symbols '(*)) '(*)))

  ;; multiple symbols
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '(x y)) '(x y))))
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '(0 x y)) '(x y))))
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '(x 0 y)) '(x y))))
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '(x y -)) '(x y))))
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '(x x y y y)) '(x y))))

  ;; symbols with width specifiers
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '((x 3) 0)) '(x))))
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '(z (x 3) y)) '(x y z))))
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '(z (x 3) x)) '(x z)))))


(test test-bad-symbols
  "Test that meaningless elements in a bitfield pattern throw an error."
  ;; string, not symbol
  (signals error
    (cl-vhdsl::extract-symbols '(x y "hello")))

  ;; non-bit value
  (signals error
    (cl-vhdsl::extract-symbols '(2 - -)))

  ;; non-symbol leading a width specifier
  (signals error
    (cl-vhdsl::extract-symbols '((2 1)))))


;; ---------- Extracting bits ----------

(test test-bit
  "Test we can extract single bits."
  (is (equal (cl-vhdsl::extract-bits 0 1 0) 0))
  (is (equal (cl-vhdsl::extract-bits 0 1 1) 1))
  (is (equal (cl-vhdsl::extract-bits 1 1 1) 0))
  (is (equal (cl-vhdsl::extract-bits 3 1 #2r1000) 1))
  (is (equal (cl-vhdsl::extract-bits 5 1 #2r1000) 0)))


(test test-bits
  "Test we can extract multiple bits."
  (is (equal (cl-vhdsl::extract-bits 0 1 0) 0))
  (is (equal (cl-vhdsl::extract-bits 0 1 1) 1))
  (is (equal (cl-vhdsl::extract-bits 1 1 1) 0))
  (is (equal (cl-vhdsl::extract-bits 4 3 #2r11100) #2r111))
  (is (equal (cl-vhdsl::extract-bits 2 2 #2r11100) #2r10)))


;; ---------- Pattern compression ----------

(test test-pattern-compression
  "Test the pattern compression logic."
  ;; no action
  (is (equal (cl-vhdsl::compress-pattern '(x y z))
	     '(x y z)))
  (is (equal (cl-vhdsl::compress-pattern '(x y x))
	     '(x y x)))
  (is (equal (cl-vhdsl::compress-pattern '(x 1 - x))
	     '(x 1 - x)))
  (is (equal (cl-vhdsl::compress-pattern '((x 4) y x))
	     '((x 4) y x)))
  (is (equal (cl-vhdsl::compress-pattern '((x 4) y (x 3)))
	     '((x 4) y (x 3))))

  ;; sequences
  (is (equal (cl-vhdsl::compress-pattern '(x x x))
	     '((x 3))))
  (is (equal (cl-vhdsl::compress-pattern '(x x y))
	     '((x 2) y)))
  (is (equal (cl-vhdsl::compress-pattern '((x 2) (x 3) y))
	     '((x 5) y)))
  (is (equal (cl-vhdsl::compress-pattern '((x 2) x x))
	     '((x 4))))
  (is (equal (cl-vhdsl::compress-pattern '(y - (x 2) (x 3) y))
	     '(y - (x 5) y)))

  ;; sequences with unknown (run-time) widths
  (is (equal (cl-vhdsl::compress-pattern '((x n) x y))
	     '((x (1+ n)) y)))
  (is (equal (cl-vhdsl::compress-pattern '(x (x n) y))
	     '((x (1+ n)) y)))
  (is (equal (cl-vhdsl::compress-pattern '(x (y n) z))
	     '(x (y n) z)))
  (is (equal (cl-vhdsl::compress-pattern '((x n) x (x n) y))
	     '((x (+ n (1+ n))) y)))
  (is (equal (cl-vhdsl::compress-pattern '((x n) (x m) y))
	     '((x (+ n m)) y))))


;; ---------- with-bitfields ----------

(test test-destructuring-constants
  "Test constant matches, no binding."
  ;; successes
  (is (with-bitfields (0) #2r100
				   T))
  (is (with-bitfields (1 0 0) #2r100
				   T))

  ;; failures (body isn't evaluated)
  (is (not (with-bitfields (1 0 1) #2r100
					T))))


(test test-destructuring-ignore
  "Test we ignore bits when needed."
  (is (with-bitfields (1 - 0) #2r100
				   T))
  (is (with-bitfields (1 - 0) #2r110
				   T)))


(test test-destructuring-bind-single
  "Test we bind single-bit variables correctly."
  ;; single-character symbols
  (is (equal (with-bitfields (x 0 0) #2r100
					  x)
	     1))

  ;; long symbols
  (is (equal (with-bitfields (xyz 0 0) #2r100
					  xyz)
	     1)))


(test test-destructuring-bind-multi
  "Test we bind multiple-bit variables correctly."
  (is (equal (with-bitfields (x x 0) #2r100
					  x)
	     #2r10))
  (is (equal (with-bitfields (x x x) #2r100
					  x)
	     #2r100))

  ;; occurrances don't have to be contiguous
  (is (equal (with-bitfields (x x 0 x) #2r1101
					  x)
	     #2r111)))


(test test-destructuring-bind-several
  "Test the binding of several variables."
  (is (equal (with-bitfields (x y 0) #2r100
					  (list x y))
	     (list 1 0)))
  (is (equal (with-bitfields (x x y 0) #2r1100
					  (list x y))
	     (list #2r11 0)))
  (is (equal (with-bitfields (x x y y) #2r1101
					  (list x y))
	     (list #2r11 1)))

  ;; intermingle with constants and wildcard bits
  (is (equal (with-bitfields (x x - - y y 1 0) #2r11011010
					  (list x y))
	     (list #2r11 #2r10)))

  ;; occurrances can be interleaved (strange but legal)
  (is (equal (with-bitfields (x y x y) #2r1101
					  (list x y))
	     (list #2r10 #2r11))))


(test test-destructuring-bind-width
  "Test we can bind bitfields using a width specifier."
  ;; simple match
  (is (equal (with-bitfields ((x 3)) #2r1010
					  x)
	     #2r10))

  ;; match with other elements
  (is (equal (with-bitfields ((x 3) 0) #2r1010
					  x)
	     #2r101))
  (is (equal (with-bitfields (y (x 3) z) #2r11010
					  (list x y z))
	     (list #2r101 1 0)))
  (is (equal (with-bitfields (0 (x 3) z) #2r11010
					  (list x z))
	     nil))

  ;; match several uses of the same symbol
  (is (equal (with-bitfields (x (x 3) y) #2r11010
					  (list x y))
	     (list #2r1101 0)))
  (is (equal (with-bitfields ((x 2) (x 3) y) #2r110101
					  (list x y))
	     (list #2r11010 1)))
  (is (equal (with-bitfields ((x 2) y (x 3)) #2r110101
					  (list x y))
	     (list #2r11101 0)))

  ;; match a zero-width field
  (is (equal (with-bitfields ((x 0) y (x 3)) #2r110101
					  (list x y))
	     (list #2r101 0)))
  (is (equal (with-bitfields ((x 0) y (x 0)) #2r110101
					  (list x y))
	     (list 0 1)))

  ;; fail on negative and non-integer field widths


  )

(test destructuring-bind-variable-width
  "Test that we can include expressions as field widths."
  ;; the simplest case
  (let ((w 3))
    (is (equal (with-bitfields ((x w)) #2r1110
					    x)
	       #2r110))

    ;; more complicated cases
    (is (equal (with-bitfields (y (x w) z) #2r11101
					    (list x y z))
	       (list #2r110 1 1)))
    (is (equal (with-bitfields (y (x w) (x w) z) #2r11101011
					    (list x y z))
	       (list #2r110101 1 1)))
    (is (equal (with-bitfields (y (x w) 0 (x w) z) #2r111001011
					    (list x y z))
	       (list #2r110101 1 1)))
    (is (equal (with-bitfields (y (x w) 1 (x w) z) #2r111001011
					    (list x y z))
	       nil))

    ;; computed field widths
    (is (equal (with-bitfields (y (x (+ w 1)) (x w) z) #2r111101011
					    (list x y z))
	       (list #2r1110101 1 1)))

    ;; side-effects in calculations (do once, in order)
    (let ((v 1))
      (is (equal (with-bitfields ((x v)
					       (y (setq v (1+ v)))
					       (z (setq v (1+ v))))
					      #2r110111
					      (list x y z))
		 (list #2r1 #2r10 #2r111))))
    (let ((v 1))
      (is (equal (with-bitfields ((x v)
					       (y (setq v (1+ v)))
					       (y (setq v (1+ v))))
					      #2r110111
					      (list x y))
		 (list #2r1 #2r10111))))

    ;; zero-width fields
    (let ((v 0))
      (is (equal (with-bitfields ((x v) y (x 3)) #2r110101
					      (list x y))
		 (list #2r101 0))))
    (let ((v 0))
      (is (equal (with-bitfields ((x v) y) #2r110101
					      (list x y))
		 (list 0 1))))

    ;; fields with nothing to match
    (let ((v 0))
      (is (equal (with-bitfields ((x v) (y v)) #2r110101
					      (list x y))
		 (list 0 0))))

    ;; fail with negative or non-integer dynamic field widths
    ))
