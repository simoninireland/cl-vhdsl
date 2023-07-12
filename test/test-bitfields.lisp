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
  (is (null (set-exclusive-or (cl-vhdsl::extract-symbols '(x x y y y)) '(x y)))))


(test test-bad-symbols
  "Test that meaningless elements in a bitfield pattern throw an error."
  (signals error
    (cl-vhdsl::extract-symbols '(x y (x))))
  (signals error
    (cl-vhdsl::extract-symbols '(x y "hello")))
   (signals error
    (cl-vhdsl::extract-symbols '(2 - -))))


;; ---------- Extracting bits ----------

(test test-bits
  "Test we can extract bits."
  (is (equal (cl-vhdsl::extract-bit 0 0) 0))
  (is (equal (cl-vhdsl::extract-bit 0 1) 1))
  (is (equal (cl-vhdsl::extract-bit 1 1) 0))
  (is (equal (cl-vhdsl::extract-bit 3 #2r1000) 1))
  (is (equal (cl-vhdsl::extract-bit 5 #2r1000) 0)))


;; ---------- destructuring-bind-bitfield ----------

(test test-destructuring-constants
  "Test constant matches, no binding."
  ;; successes
  (is (destructuring-bind-bitfield (0) #2r100
				   T))
  (is (destructuring-bind-bitfield (1 0 0) #2r100
				   T))

  ;; failures (body isn't evaluated)
  (is (not (destructuring-bind-bitfield (1 0 1) #2r100
					T))))


(test test-destructuring-ignore
  "Test we ignore bits when needed."
  (is (destructuring-bind-bitfield (1 - 0) #2r100
				   T))
  (is (destructuring-bind-bitfield (1 - 0) #2r110
				   T)))


(test test-destructuring-bind-single
  "Test we bind single-bit variables correctly."
  ;; single-character symbols
  (is (equal (destructuring-bind-bitfield (x 0 0) #2r100
					  x)
	     1))

  ;; long symbols
  (is (equal (destructuring-bind-bitfield (xyz 0 0) #2r100
					  xyz)
	     1)))


(test test-destructuring-bind-multi
  "Test we bind multiple-bit variables correctly."
  (is (equal (destructuring-bind-bitfield (x x 0) #2r100
					  x)
	     #2r10))
  (is (equal (destructuring-bind-bitfield (x x x) #2r100
					  x)
	     #2r100))

  ;; occurrances don't have to be contiguous
  (is (equal (destructuring-bind-bitfield (x x 0 x) #2r1101
					  x)
	     #2r111)))


(test test-destructuring-bind-several
  "Test the binding of several variables."
  (is (equal (destructuring-bind-bitfield (x y 0) #2r100
					  (list x y))
	     (list 1 0)))
  (is (equal (destructuring-bind-bitfield (x x y 0) #2r1100
					  (list x y))
	     (list #2r11 0)))
  (is (equal (destructuring-bind-bitfield (x x y y) #2r1101
					  (list x y))
	     (list #2r11 1)))

  ;; intermingle with constants and wildcard bits
  (is (equal (destructuring-bind-bitfield (x x - - y y 1 0) #2r11011010
					  (list x y))
	     (list #2r11 #2r10)))

  ;; occurrances can be interleaved (strange but legal)
  (is (equal (destructuring-bind-bitfield (x y x y) #2r1101
					  (list x y))
	     (list #2r10 #2r11))))
