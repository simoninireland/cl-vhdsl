;; test-bus.lisp: Tests of bitlists
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

;; ---------- Bitfields to and from lists ----------

(test test-explode-bitfields
  "Test we can convert numbers to lists."
  (is (equal (cl-vhdsl::explode-bitfield 0)
	     '(0)))
  (is (equal (cl-vhdsl::explode-bitfield 1)
	     '(1)))
  (is (equal (cl-vhdsl::explode-bitfield #2r10)
	     '(0 1)))
  (is (equal (cl-vhdsl::explode-bitfield #2r1011)
	     '(1 1 0 1))))


(test test-implode-bitfields
  "Test we can create numbers from lists."
  (is (equal (cl-vhdsl::implode-bitfield '())
	     0))
  (is (equal (cl-vhdsl::implode-bitfield '(0))
	     0))
  (is (equal (cl-vhdsl::implode-bitfield '(1))
	     1))
  (is (equal (cl-vhdsl::implode-bitfield '(0 1))
	     #2r10))
  (is (equal (cl-vhdsl::implode-bitfield '(1 0 1 1 0))
	     #2r1101)))


(test test-implode-explode-bitfields
  "Test that implosion is the inverse of explosion."
  (is (equal (cl-vhdsl::implode-bitfield (cl-vhdsl::explode-bitfield 0))
	     0))
  (is (equal (cl-vhdsl::implode-bitfield (cl-vhdsl::explode-bitfield 1))
	     1))
  (is (equal (cl-vhdsl::implode-bitfield (cl-vhdsl::explode-bitfield #2r1011101101))
	     #2r1011101101)))


(test test-implode-explode-indices
  "Test we can implode and explode specific bit indices."
  ;; exploding -- extracting specific bits
  (is (equal (cl-vhdsl::explode-bitfield-indices #2r10110 '())
	     '()))
  (is (equal (cl-vhdsl::explode-bitfield-indices #2r10110 '(0 1))
	     '(0 1)))
  (is (equal (cl-vhdsl::explode-bitfield-indices #2r10110 '(0 4 5))
	     '(0 1 0)))

  ;; imploding -- setting specific bits (making others 0)
  (is (equal (cl-vhdsl::implode-bitfield-indices '(0 1 0 0 1) '())
	     0))
  (is (equal (cl-vhdsl::implode-bitfield-indices '(0 1 0 0 1) '(1))
	     #2r10))
  (is (equal (cl-vhdsl::implode-bitfield-indices '(0 1 0 0 1) '(1 2))
	     #2r10))
  (is (equal (cl-vhdsl::implode-bitfield-indices '(0 1 0 0 1) '(1 2 4))
	     #2r10010)))


(test test-fix-bitfield-width
  "Test we can fix bitfield widths."
  (is (equal (cl-vhdsl::fix-bitfield-width '(0) 1)
	     '(0)))
  (is (equal (cl-vhdsl::fix-bitfield-width '(1) 1)
	     '(1)))
  (is (equal (cl-vhdsl::fix-bitfield-width '(1 0) 1)
	     '(1 0)))
  (is (equal (cl-vhdsl::fix-bitfield-width '(1 0 1) 5)
	     '(1 0 1 0 0))))

(test test-change-bit-indices
  "Test we can find the bits that have changed between two numbers."
  (is (equal (cl-vhdsl::changed-bit-indices #2r1 #2r1)
	     '()))
  (is (equal (cl-vhdsl::changed-bit-indices #2r1 #2r0)
	     '(0)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r101)
	     '()))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r100)
	     '(0)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r111)
	     '(1)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r110)
	     '(0 1)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r010)
	     '(0 1 2)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r001)
	     '(2)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r1101)
	     '(3))))

(test test-get-set-bits
  "Test we can get and set individual bits."
  (is (equal (cl-vhdsl::get-bit #2r1010 0)
	     0))
  (is (equal (cl-vhdsl::get-bit #2r1010 1)
	     1))
  (is (equal (cl-vhdsl::get-bit #2r1010 7)
	     0))
  (is (equal (cl-vhdsl::set-bit #2r1010 0 0)
	     #2r1010))
  (is (equal (cl-vhdsl::set-bit #2r1010 0 1)
	     #2r1011))
  (is (equal (cl-vhdsl::set-bit #2r1010 1 0)
	     #2r1000))
  (is (equal (cl-vhdsl::set-bit #2r1010 3 0)
	     #2r10))
  (is (equal (cl-vhdsl::set-bit #2r1010 3 1)
	     #2r1010)))

(test test-update-bit-indices
  "Test we can update individually-indexed bits of a number."
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '() '())
	     #2r1010))
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '(1) '(0))
	     #2r1011))
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '(1 0) '(0 1))
	     #2r1001))
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '(1) '(2))
	     #2r1110))
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '(1) '(3))
	     #2r1010)))
