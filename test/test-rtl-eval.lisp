;; Tests of evaluation of static values
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

(test test-eval-literal
  "Test literals are static constants."
  (is (= (rtl::ensure-static 12 emptyenv) 12)))


(test test-eval-parameter
  "Test that module parameters are static constants."
  (let ((env (rtl::add-frame emptyenv)))
    (rtl::declare-variable 'a '((:width 5)
				(:initial-value 12)
				(:as :parameter))
			   env)

    (is (= (rtl::ensure-static 'a env)
	   12))))


(test test-eval-constant
  "Test that variables declared as constants are static constants."
  (let ((env (rtl::add-frame emptyenv)))
    (rtl::declare-variable 'a '((:width 5)
				(:initial-value 12)
				(:as :constant))
			   env)

    (is (= (rtl::ensure-static 'a env)
	   12))))


(test test-eval-expression
  "Test that expressions involving only constants are static constants."
  (let ((env (rtl::add-frame emptyenv)))
    (rtl::declare-variable 'a '((:width 5)
				(:initial-value 12)
				(:as :constant))
			   env)

    (is (= (rtl::ensure-static '(+ a (+ a 12)) env)
	   36))))


(test test-eval-non-constant-variable
  "Test that non-constant variables are rejected."
  (let ((env (rtl::add-frame emptyenv)))
    (mapc (lambda (decl)
	    (destructuring-bind (n props)
		decl
	      (rtl::declare-variable n props env)))
	  '((a ((:width 5)
		(:initial-value 12)
		(:as :constant)))
	    (b ((:width 5)
		(:initial-value 12)
		(:as :register)))))

    (signals (rtl:not-static)
      (rtl::ensure-static '(+ a b 12) env))))


(test test-eval-static
  "Test we get the result from a static expression, and nil from one that's not."
  (let ((env (rtl::add-frame emptyenv)))
    (mapc (lambda (decl)
	    (destructuring-bind (n props)
		decl
	      (rtl::declare-variable n props env)))
	  '((a ((:width 5)
		(:initial-value 12)
		(:as :constant)))
	    (b ((:width 5)
		(:initial-value 12)
		(:as :register)))))

     ;; statis
     (is (= (rtl::eval-if-static '(+ a (+ a 12)) env)
	    36))

     ;; not static
     (is (null (rtl::eval-if-static '(+ a b 12) env)))))
