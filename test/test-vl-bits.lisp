;; Tests of bitwise access
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/rtl)


;; ---------- Single-bit access ----------

(test test-width-bit
  "Test we can extract a bit from a value."
  (is (subtypep (vl:typecheck '(let ((a #2r10110))
				 (vl::bref a 1)))
		'(unsigned-byte 1))))


(test test-width-bits
  "Test we can extract bits from a value."
  ;; single-bit equivalents
  (is (subtypep (vl:typecheck '(let ((a #2r10110))
				 (vl::bref a 1 :end 1)))
		'(unsigned-byte 1)))
  (is (subtypep (vl:typecheck '(let ((a #2r10110))
				 (vl::bref a 1 :width 1)))
		'(unsigned-byte 1)))

  ;; multiple bits
  (is (subtypep (vl:typecheck '(let ((a #2r10110))
				 (vl::bref a 1 :width 2)))
		'(unsigned-byte 2)))
  (is (subtypep (vl:typecheck '(let ((a #2r10110))
				 (vl::bref a 1 :end 0)))
		'(unsigned-byte 2)))
  (is (subtypep (vl:typecheck '(let ((a #2r10110 :width 8))
				 (vl::bref a 7)))
		'(unsigned-byte 8)))

  ;; matching and non-matching explicit widths
  (is (subtypep (vl:typecheck '(let ((a #2r10110))
				 (vl::bref a 4 :end 2 :width 3)))
		'(unsigned-byte 3)))
  (signals (vl:type-mismatch)
    (vl:typecheck '(let ((a #2r10110))
		     (vl::bref a 4 :end 2 :width 4)))))


(test test-positive-start-end-width
  "Test that we detect non-positive values."
  (signals (vl:value-mismatch)
    (vl:typecheck '(let ((a #2r10110))
		     (vl::bref a 4 :end -1))))
  (signals (vl:value-mismatch)
    (vl:typecheck '(let ((a #2r10110))
		     (vl::bref a 4 :width -1))))
  (signals (vl:value-mismatch)
    (vl:typecheck '(let ((a #2r10110))
		     (vl::bref a -2 :end 0))))
  (signals (vl:value-mismatch)
    (vl:typecheck '(let ((a #2r10110))
		     (vl::bref a -2)))))
