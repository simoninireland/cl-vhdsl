;; Tests of COND macro
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


(test test-synthesise-cond
  "Test we can synthesise a COND, which is a macro needing to be expanded first."
  (is (rtl:synthesise (rtl::expand-macros '(cond ((rtl::<< a 1)
						  (setf b 1))
					    ((rtl::<< a 2)
					     (setf b 2))
					    (t
					     (setf b 3))))
		      :inblock)))


(test test-synthesise-cond-assignment
  "Test we can synthesise a COND in an assignment"
  (is (rtl:synthesise (rtl::expand-macros '(let ((a 1)
						 (b 2))
					    (let ((c (cond ((= b 1)
							    3)
							   ((= b 0)
							    4)
							   (t 6))))
					      (setq c (+ c 2)))
					    ))
		      :inblock)))
