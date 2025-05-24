;; Tests of synthesisable fragment passes and synthesis
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


;; ---------- Fixed-width integers ----------

(test test-unsigned-range
  "Test the range of an unsigned fixed-width integer."
  (is (typep 12 '(unsigned-byte 8)))
  (is (typep 12 '(unsigned-byte 16)))

  (is (typep 512 '(unsigned-byte 16)))
  (is (not (typep 512 '(unsigned-byte 8))))

  (is (not (typep -64 '(unsigned-byte 8)))))


(test test-signed-range
  "Test the range of a signed fixed-width integer."
  (is (typep 12 '(signed-byte 8)))
  (is (typep 12 '(signed-byte 16)))
  (is (typep 127 '(signed-byte 8)))
  (is (typep -128 '(signed-byte 8)))

  (is (not (typep 128 '(signed-byte 8))))
  (is (not (typep -129 '(signed-byte 8))))
  (is (not (typep -64 '(signed-byte 4)))))


;; ---------- Widths ----------

(test test-bitwidths-integer-types
  "Test we can extract the widths of integer types."
  (is (= (vl::bitwidth '(unsigned-byte 8)) 8))
  (is (= (vl::bitwidth '(signed-byte 8)) 8)))


(test test-bitwidths-integer-constants
  "Test we can extract bit widths of integer constants."
  (is (= (vl::bitwidth 0) 1))
  (is (= (vl::bitwidth 1) 1))
  (is (= (vl::bitwidth 2) 2))
  (is (= (vl::bitwidth 127) 7))

  (is (= (vl::bitwidth -127) 8))
  (is (= (vl::bitwidth -1) 2)))
