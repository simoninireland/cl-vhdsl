;; Tests of floating LET blocks pass
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


(test test-let-float
  "Test that nested LETs float."
  (destructuring-bind (form decls)
      (rtl:float-let-blocks '(let ((a 1 :width 8))
			      (setq a 12)
			      (let ((b (+ a 1)))
				(setq a (* a b)))))
    (is (equal (mapcar #'car decls)
	       '(a b)))

    ;; this may change when we float constant initial values
    (is (equal (mapcar #'cadr decls)
	       '(1 0)))))


(test test-let-float-markers
  "Test we retain constant (and other) markers when floating."
  (destructuring-bind (form decls)
      (rtl::float-let-blocks '(let ((a 1 :width 10))
			       (setq a 10)
			       (let ((b 12 :as :constant))
				 (setq a (* b a)))))
    (is (equal (mapcar #'cddr decls)
	       '((:width 10)
		 (:as :constant))))))
