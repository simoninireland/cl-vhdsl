;; Tests of shadowing pass
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


(test test-detect-shadowed
  "Test we can detect a shadowed variable."
  (signals (rtl:duplicate-variable)
    (rtl::detect-shadowing '(let ((a 12))
			     (setq a (+ a 1))
			     (let ((b 1)
				   (a 34))
			       (setq a (+ b a))))))

  ;; no shadowing
  (is (rtl::detect-shadowing '(let ((a 12))
			       (setq a (+ a 1))
			       (let ((b 1)
				     (c 34))
				 (setq c (+ b a)))))))


(test test-detect-shadowed-module
  "Test we can detect shadowing in the body of a module."
  (signals (rtl:duplicate-variable)
    (rtl::detect-shadowing '(rtl::module test ()
			     (let ((a 12))
			       (setq a (+ a 1))
			       (let ((b 1)
				     (a 34))
				 (setq a (+ b a))))))))
