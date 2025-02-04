;; Tests of basic utilities of RTLisp
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


(defvar emptyenv (rtl:empty-environment))


;; ---------- Environments ----------

(test test-names
  "Test we can extract names from an environment."
  ;; empty environment has no names
  (is (equal (rtl:get-environment-names emptyenv)
	     nil))

  ;; single name
  (let ((env1 (rtl:extend-environment 'a '() emptyenv)))
    (is (equal (rtl:get-environment-names env1)
	       '(a)))

    ;; nested environment
    (let ((env2 (rtl:extend-environment 'b '() env1)))
      (is (set-equal (rtl:get-environment-names env2)
		     '(a b)))

      ;; duplicate name
      (let ((env3 (rtl:extend-environment 'a '() env2)))
	(is (set-equal (rtl:get-environment-names env3)
		       '(a b)))))))


(test test-identifiers
  "Test legal and illegal identifiers."

  ;; legal identifiers
  (dolist (s '("a" "abc" "abc1" "a1Bc" "_a" "a_" "_123" "_camelCase" "_reg"))
    (is (string-equal (rtl::ensure-legal-identifier s) s)))

  ;; illegal idenfiers (bad characters or arrangements)
  (dolist (from-to '(("0abc" "_0abc")
		     ("01" "_01")
		     ("test-me" "test_me")
		     ("1test-me" "_1test_me")))
    (destructuring-bind (from to)
	from-to
      (is (string-equal (rtl::ensure-legal-identifier from) to))))

  ;; illegal identifiers (keywords)
  (dolist (from-to '(("reg" "_reg")
		     ("inout" "_inout")))
    (destructuring-bind (from to)
	from-to
      (is (string-equal (rtl::ensure-legal-identifier from) to))))

  ;; check signalling of really bad variable name choice
  (signals (rtl:bad-variable)
    (rtl::ensure-legal-identifier "_")))


(test test-filter-env
  "Test we can filter environments."
  (flet ((filter-by-f (n env)
	   (if-let ((prop (rtl::get-environment-property n :f env)))
	     (> prop 20))))
    (let ((env (foldl (lambda (decl env)
			(rtl::extend-environment (car decl) (cadr decl) env))
		      '((a ()) (b ((:f 23) (:g 34))) (c ((:f 12))))
		      emptyenv)))
      ;; by name
      (is (equal (rtl::filter-environment (lambda (n env)
					    (equal n 'a))
					  env)
		 '((a))))

      ;; by property
      (is (equal (rtl::filter-environment #'filter-by-f
					  env)
		 '((b (:f 23) (:g 34))))))))


(test test-map-env
  "Test we can map across environments."
  (let ((env (foldl (lambda (decl env)
		      (rtl::extend-environment (car decl) (cadr decl) env))
		    '((a ()) (b ((:f 23) (:g 34))) (c ((:f 12))))
		    emptyenv)))
    (is (equal (rtl::map-environment (lambda (n env)
				       (or (rtl::get-environment-property n :f env)
				      0))
				env)
	       '(0 23 12)))))
