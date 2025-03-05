;; Tests of variable re-writing pass
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


(test test-rewrite-simple
  "Test simple re-writing cases."

  ;; literals
  (let ((p 23))
    (is (equal (rtl::rewrite-variables p '())
	       p)))
  (let ((p 23))
    (is (equal (rtl::rewrite-variables p '((a 24)))
	       p)))

  ;; symbols (same variable)
  (is (equal (rtl::rewrite-variables 'a '((a 24)))
	     24))

  ;; symbols (no or different variables
  (is (equal (rtl::rewrite-variables 'a '())
	     'a))
  (is (equal (rtl::rewrite-variables 'a '((b 24)))
	     'a))

  ;; operators (normal recursion)
  (is (equal (rtl::rewrite-variables '(+ 1 2 3) '((a 5)))
	     '(+ 1 2 3)))
  (is (equal (rtl::rewrite-variables '(+ 1 2 a) '((a 5)))
	     '(+ 1 2 5)))
  (is (equal (rtl::rewrite-variables '(+ 1 2 b) '((a 5)))
	     '(+ 1 2 b)))

  ;; let (different variables)
  (let ((p '(let ((b 23))
	     (setq b 24))))
    (is (equal (rtl::rewrite-variables p '((a 5)))
	       p)))

  ;; let (identified variables)
  (is (equal (rtl::rewrite-variables '(let ((b 1))
				       (setq b a))
				     '((a 5)))
	     '(let ((b 1))
	       (setq b 5))))

  ;; let (shadowed variables)
  (is (equal (rtl::rewrite-variables '(let ((b 1))
				       (setq b a)
				       (let ((a 45))
					 (setq b a)
					 (setq a c)))
				     '((a 5) (c 10)))
	     '(let ((b 1))
	       (setq b 5)
	       (let ((a 45))
		 (setq b a)
		 (setq a 10))))))
