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


;; ---------- Environment closure ----------

(test test-make-environment
  "Test we shadow variables correctly."
  (let ((env1 (rtl::add-environment-frame (rtl::empty-environment))))
    (mapc (lambda (decl)
	    (rtl::declare-environment-variable (car decl) (cadr decl) env1))
	  '((a ((:initial-value 4)))
	    (b ((:initial-value 23) (:g 34)))
	    (c ((:initial-value 12)))))

    (let ((env2 (rtl::add-environment-frame env1)))
      (mapc (lambda (decl)
	      (rtl::declare-environment-variable (car decl) (cadr decl) env2))
	    '((a ((:initial-value 5)))
	      (d ((:initial-value 234) (:g 76)))))

      ;; a should be shadowed and appear only once
      (let ((decls (rtl::make-environment-alist env2)))
	(is (equal (mapcar #'car decls)
		   '(d a c b)))
	(is (= (cadr (assoc 'a decls)) 5))))))


;; ---------- Evaluation ----------

(test test-eval-literal
  "Test literals are static constants."
  (is (= (rtl::ensure-static 12) 12)))


(test test-eval-parameter
  "Test that module parameters are static constants."
  (rtl::with-new-frame
    (rtl::declare-variable 'a '((:width 5)
				(:initial-value 12)
				(:as :parameter)))

    (is (= (rtl::ensure-static 'a)
	   12))))


(test test-eval-constant
  "Test that variables declared as constants are static constants."
  (rtl::with-new-frame
    (rtl::declare-variable 'a '((:width 5)
				(:initial-value 12)
				(:as :constant)))

    (is (= (rtl::ensure-static 'a)
	   12))))


(test test-eval-expression
  "Test that expressions involving only constants are static constants."
  (rtl::with-new-frame
    (rtl::declare-variable 'a '((:width 5)
				(:initial-value 12)
				(:as :constant)))

    (is (= (rtl::ensure-static '(+ a (+ a 12)))
	   36))))


(test test-eval-non-constant-variable
  "Test that non-constant variables are rejected."
  (rtl::with-new-frame
    (mapc (lambda (decl)
	    (destructuring-bind (n props)
		decl
	      (rtl::declare-variable n props)))
	  '((a ((:width 5)
		(:initial-value 12)
		(:as :constant)))
	    (b ((:width 5)
		(:initial-value 12)
		(:as :register)))))

    (signals (rtl:not-static)
      (rtl::ensure-static '(+ a b 12)))))


(test test-eval-static
  "Test we get the result from a static expression, and nil from one that's not."
  (rtl::with-new-frame
    (mapc (lambda (decl)
	    (destructuring-bind (n props)
		decl
	      (rtl::declare-variable n props)))
	  '((a ((:width 5)
		(:initial-value 12)
		(:as :constant)))
	    (b ((:width 5)
		(:initial-value 12)
		(:as :register)))))

     ;; statis
     (is (= (rtl::eval-if-static '(+ a (+ a 12)))
	    36))

     ;; not static
     (is (null (rtl::eval-if-static '(+ a b 12))))))


(test test-shadowed-variables
  "Test we declare only the shallowest declaration of each variable."
  (rtl::with-new-frame
    (rtl::declare-variable 'a '((:width 5)
				(:initial-value 12)
				(:as :constant)))
    (rtl::declare-variable 'b '((:width 5)
				(:initial-value 15)
				(:as :constant)))

    (rtl::with-new-frame
      (rtl::declare-variable 'b '((:width 5)
				  (:initial-value 30)
				  (:as :constant)))

      (is (= (rtl::eval-in-static-environment '(+ a b)) 42)))))
