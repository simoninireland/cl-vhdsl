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
  (is (rtl::legal-identifier-p "a"))
  (is (rtl::legal-identifier-p "abc"))
  (is (rtl::legal-identifier-p "abc1"))
  (is (rtl::legal-identifier-p "a1Bc"))
  (is (rtl::legal-identifier-p "_a"))
  (is (rtl::legal-identifier-p "a_"))
  (is (rtl::legal-identifier-p "_123"))
  (is (rtl::legal-identifier-p "_camelCase"))

  ;; illegal idenfiers (bad characters or arrangements)
  (is (not (rtl::legal-identifier-p "_")))
  (is (not (rtl::legal-identifier-p "0abc")))
  (is (not (rtl::legal-identifier-p "01")))

  ;; illegal identifiers (keywords)
  (is (not (rtl::legal-identifier-p "reg")))
  (is (not (rtl::legal-identifier-p "inout")))

  ;; check signalling
  (signals (rtl:not-synthesisable)
    (is (not (rtl::ensure-legal-identifier "reg")))))


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
