;; Tests of synthesisable fragment passes and synthesis
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
(declaim (optimize debug))


;; ---------- Fixed-width integers ----------

(test test-unsigned-range
  "Test the range of an unsigned fixed-width integer."
  (is (typep 12 '(rtl::fixed-width-unsigned 8)))
  (is (typep 12 '(rtl::fixed-width-unsigned 16)))

  (is (typep 512 '(rtl::fixed-width-unsigned 16)))
  (is (not (typep 512 '(rtl::fixed-width-unsigned 8))))

  (is (not (typep -64 '(rtl::fixed-width-unsigned 8)))))


(test test-signed-range
  "Test the range of a signed fixed-width integer."
  (is (typep 12 '(rtl::fixed-width-signed 8)))
  (is (typep 12 '(rtl::fixed-width-signed 16)))
  (is (typep 127 '(rtl::fixed-width-signed 8)))
  (is (typep -128 '(rtl::fixed-width-signed 8)))

  (is (not (typep 128 '(rtl::fixed-width-signed 8))))
  (is (not (typep -129 '(rtl::fixed-width-signed 8))))
  (is (not (typep -64 '(rtl::fixed-width-signed 4)))))


;; ---------- Widths ----------

(test test-bitwidths-integer-types
  "Test we can extract the widths of integer types."
  (is (= (rtl::bitwidth '(rtl:fixed-width-unsigned 8) ()) 8))
  (is (= (rtl::bitwidth '(rtl:fixed-width-signed 8) ()) 8)))


(test test-bitwidths-integer-constants
  "Test we can extract bit widths of integer constants."
  (is (= (rtl::bitwidth 0 ()) 0))
  (is (= (rtl::bitwidth 1 ()) 1))
  (is (= (rtl::bitwidth 2 ()) 2))
  (is (= (rtl::bitwidth 127 ()) 7))

  (is (= (rtl::bitwidth -127 ()) 8))
  (is (= (rtl::bitwidth -1 ()) 2)))
