;; Tests of bitwise access
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

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl/rtl)


;; ---------- Single-bit access ----------

(test test-width-bit
  "Test we can extract a bit from a value."
  (is (subtypep (rtl:typecheck '(let ((a #2r10110))
				 (rtl::bref a 1)))
		'(unsigned-byte 1))))


(test test-width-bits
  "Test we can extract bits from a value."
  ;; single-bit equivalents
  (is (subtypep (rtl:typecheck '(let ((a #2r10110))
				 (rtl::bref a 1 :end 1)))
		'(unsigned-byte 1)))
  (is (subtypep (rtl:typecheck '(let ((a #2r10110))
				 (rtl::bref a 1 :width 1)))
		'(unsigned-byte 1)))

  ;; multiple bits
  (is (subtypep (rtl:typecheck '(let ((a #2r10110))
				 (rtl::bref a 1 :width 2)))
		'(unsigned-byte 2)))
  (is (subtypep (rtl:typecheck '(let ((a #2r10110))
				 (rtl::bref a 1 :end 0)))
		'(unsigned-byte 2)))
  (is (subtypep (rtl:typecheck '(let ((a #2r10110 :width 8))
				 (rtl::bref a 7)))
		'(unsigned-byte 8)))

  ;; matching and non-matching explicit widths
  (is (subtypep (rtl:typecheck '(let ((a #2r10110))
				 (rtl::bref a 4 :end 2 :width 3)))
		'(unsigned-byte 3)))
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(let ((a #2r10110))
		     (rtl::bref a 4 :end 2 :width 4)))))


(test test-positive-start-end-width
  "Test that we detect non-positive values."
  (signals (rtl:value-mismatch)
    (rtl:typecheck '(let ((a #2r10110))
		     (rtl::bref a 4 :end -1))))
  (signals (rtl:value-mismatch)
    (rtl:typecheck '(let ((a #2r10110))
		     (rtl::bref a 4 :width -1))))
  (signals (rtl:value-mismatch)
    (rtl:typecheck '(let ((a #2r10110))
		     (rtl::bref a -2 :end 0))))
  (signals (rtl:value-mismatch)
    (rtl:typecheck '(let ((a #2r10110))
		     (rtl::bref a -2)))))
