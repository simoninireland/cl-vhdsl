;; Tests of CONDWITH-BITFIERLSmacro
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


(test test-extract-runs
  "Test we can extract runs of symbols from a list."
  (is (null (rtl::extract-runs '())))
  (is (equal (rtl::extract-runs '(a))
	     '((a 0 0))))
  (is (equal (rtl::extract-runs '(a a a))
	     '((a 2 0))))
  (is (equal (rtl::extract-runs '(a a a b b b))
	     '((a 5 3) (b 2 0))))
  (is (equal (rtl::extract-runs '(a a a b a a))
	     '((a 5 3) (b 2 2) (a 1 0))))
  (is (equal (rtl::extract-runs '(a a a b a a c))
	     '((a 6 4) (b 3 3) (a 2 1) (c 0 0)))))


;; Should this function be in utils?
(test test-duplicate-keys
  "Test we can detect duplicate keys in alists."
  (is (rtl::duplicate-keys-p '((a 12) (b 13) (a 1))))
  (is (rtl::duplicate-keys-p '((a 12) (a 13))))

  (is (not (rtl::duplicate-keys-p '())))
  (is (not (rtl::duplicate-keys-p '((a 12)))))
  (is (not (rtl::duplicate-keys-p '((a 12) '(b 12))))))


(test test-extract-bitfields
  "Test we can extract bitfields from patterns."
  (is (equal (rtl::extract-bitfields '(a))
	     '((a 0 0))))
  (is (equal (rtl::extract-bitfields '(1 a))
	     '((1 1 1) (a 0 0))))
  (signals (rtl:bitfield-mismatch)
    (rtl::extract-bitfields '(1 a 1 a))))