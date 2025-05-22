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


(defparameter emptyenv (rtl:empty-environment))


;; ---------- Frames ----------

(test test-declare-variables
  "Test we can declare variables in a frame."
  (let ((env (rtl::empty-environment)))
    (is (rtl::variable-declared-in-environment-p 'a (rtl::declare-environment-variable 'a '((:a 1) (:b 2)) env)))
    (is (rtl::variable-declared-in-environment-p 'b (rtl::declare-environment-variable 'b '((:a 6) (:b 4)) env)))

    ;; can't declare duplicates in the same frame
    (signals (rtl::duplicate-variable)
      (rtl::declare-environment-variable 'b '((:a 6) (:b 4)) env))

    ;; names
    (is (set-equal (rtl::get-frame-names env)
		   '(b a)))
    (is (rtl::variable-declared-in-frame-p 'a env))
    (is (not (rtl::variable-declared-in-frame-p 'c env)))))


(test test-get-properties
  "Test we can retrieve properties from a frame."
  (let ((env (rtl::empty-environment)))
    (rtl::declare-environment-variable 'a '((:a 1) (:b 2)) env)
    (is (rtl::get-frame-properties 'a env)
	'((:a 1) (:b 2)))

    (signals (rtl:unknown-variable)
      (rtl::get-frame-properties 'b env))))


(test test-get-property
  "Test we can retrieve a property."
  (let ((env (rtl::empty-environment)))
    (rtl::declare-environment-variable 'a '((:a 1) (:b 2)) env)
    (is (equal (rtl::get-frame-property 'a :b env) 2))

    (is (null (rtl::get-frame-property 'a :c env)))

    (rtl::declare-environment-variable 'c '((:a 1) (:b 2)) env)
    (is (eql (rtl::get-frame-property 'c :c env :default 'ttt)
	     'ttt))))


(test test-frame-declaring
  "Test we can find the frame declaring a variable."
  (let ((env1 (rtl::empty-environment)))
    (rtl::declare-environment-variable 'a '((:a 1) (:b 2)) env1)
    (rtl::declare-environment-variable 'b '((:a 1) (:b 2)) env1)

    (let ((env2 (rtl::add-environment-frame env1)))
      (rtl::declare-environment-variable 'b '((:a 1) (:b 2)) env2)

      (equal (rtl::get-frame-declaring 'b env2) env2)
      (equal (rtl::get-frame-declaring 'a env2) env1)
      (equal (rtl::get-frame-declaring 'b env1) env1)

      (signals (rtl:unknown-variable)
	(rtl::get-frame-declaring 'c env2)))))


(test test-set-property
  "Test we can set frame properties."
  (let ((env (rtl::empty-environment)))
    (rtl::declare-environment-variable 'a '((:a 1) (:b 2)) env)
    (rtl::declare-environment-variable 'b '((:a 1) (:b 2)) env) ; maximise sharing danger

    ;; update a property
    (rtl::set-frame-property 'a :a 12 env)
    (is (equal (rtl::get-frame-property 'a :a env) 12))
    (is (equal (rtl::get-frame-property 'a :b env) 2))
    (is (equal (rtl::get-frame-property 'b :a env) 1))

    ;; add a property
    (is (null (rtl::get-frame-property 'a :c env)))
    (rtl::set-frame-property 'a :c 99 env)
    (is (equal (rtl::get-frame-property 'a :c env) 99))
    (is (null (rtl::get-frame-property 'b :c env)))))


;; ---------- Environments ----------

(test test-names
  "Test we can extract names from an environment."
  ;; empty environment has no names
  (is (null (rtl:get-environment-names emptyenv)))

  (let ((env1 (rtl::add-environment-frame emptyenv)))
    (rtl::declare-environment-variable 'a '((:a 1 :b 2)) env1)
    (is (set-equal (rtl::get-environment-names env1)
		   '(a)))

    ;; nested frame
    (let ((env2 (rtl::add-environment-frame env1)))
      (rtl::declare-environment-variable 'b '((:a 1 :b 2)) env2)
      (rtl::declare-environment-variable 'c '((:a 1 :b 2)) env2)
      (is (set-equal (rtl::get-environment-names env2)
		     '(a b c)))

      ;; nested again
      (let ((env3 (rtl::add-environment-frame env2)))
	(rtl::declare-environment-variable 'b '((:a 4 :b 5)) env3)
	(is (set-equal (rtl::get-environment-names env3)
		       '(a b c)))

	;; local (frame) property is retrieved correctly
	(is (equal (rtl::get-frame-property 'b :a env3) 4))

	;; global (environment) property is the same
	(is (equal (rtl::get-environment-property 'b :a env3) 4))

	;; shallower frame is undisturbed
	(is (equal (rtl::get-frame-property 'b :a env2) 1))

	;; finding variables works as expected
	(is (not (rtl::variable-declared-in-frame-p 'a env3)))
	(is (rtl::variable-declared-in-environment-p 'a env3))

	;; variable can't be found from topmost frame
	(signals (rtl:unknown-variable)
	  (rtl::get-frame-properties 'a env3))

	;; ...but is found correctly globally
	(is (equal (rtl::get-environment-property 'a :a env3) 1))))))


(test test-names-empty-env
  "Test that an empty environment has no names."
  (is (null (rtl::get-environment-names (rtl::empty-environment)))))


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
  (signals (rtl:not-synthesisable)
    (rtl::ensure-legal-identifier "_")))


(test test-test-filter-frame
  "Test we can filter a single frame."
  (flet ((filter-by-f (n env)
	   (if-let ((prop (rtl::get-environment-property n :f env)))
	     (> prop 20))))
    (let ((env1 emptyenv))
      (is (null (rtl::get-frame-names env1)))

      (let ((env2 (rtl::add-environment-frame env1)))
	(is (rtl::declare-environment-variable 'a '((:f 10)) env2))
	(is (rtl::declare-environment-variable 'b '((:f 30)) env2))
	(set-equal (rtl::get-frame-names (rtl::filter-environment #'filter-by-f env2))
		   '(b))))))


(test test-filter-env
  "Test we can filter environments."
  (flet ((filter-by-f (n env)
	   (if-let ((prop (rtl::get-environment-property n :f env)))
	     (> prop 20))))

    (let ((env1 (rtl::empty-environment)))
      (mapc (lambda (decl)
	      (rtl::declare-environment-variable (car decl) (cadr decl) env1))
	    '((a ((:f 4)))
	      (b ((:f 23) (:g 34)))
	      (c ((:f 12)))))

      (let ((env2 (rtl::add-environment-frame env1)))
	(mapc (lambda (decl)
		(rtl::declare-environment-variable (car decl) (cadr decl) env2))
	      '((a ((:f 28)))
		(d ((:f 99)))))

	;; by name
	(let ((kept-names (rtl::filter-environment (lambda (n env)
						     (equal n 'a))
						   env2)))
	  (is (set-equal (rtl::get-environment-names kept-names)
			 '(a))))

	;; by property
	(let ((kept-props (rtl::filter-environment #'filter-by-f
						   env2)))
	  (is (set-equal (rtl::get-environment-names kept-props)
			 '(a d b))))))))


(test test-map-env
  "Test we can map across environments."
  (let ((env1 (rtl::add-environment-frame (rtl::empty-environment))))
    (mapc (lambda (decl)
	    (rtl::declare-environment-variable (car decl) (cadr decl) env1))
	  '((a ((:g 4)))
	    (b ((:f 23) (:g 34)))
	    (c ((:f 12)))))

    (let ((env2 (rtl::add-environment-frame env1)))
      (mapc (lambda (decl)
	      (rtl::declare-environment-variable (car decl) (cadr decl) env2))
	    '((a ((:f 28)))
	      (d ((:g 99)))))

      (is (equal (rtl::map-environment (lambda (n env)
					 (or (rtl::get-environment-property n :f env)
					     0))
				       env2)
		 '(0 28 12 23 0))))))


;; ---------- Property access and update ----------

(test test-env-get-property
  "Test we can get a property."
  (let ((env1 (rtl::add-environment-frame emptyenv)))
    (rtl::declare-environment-variable 'a '((:type (unsigned-byte 8))
					    (:width 8))
				       env1)
    (is (equal (rtl::get-frame-property 'a :type env1)
	       '(unsigned-byte 8)))

    (let ((env2 (rtl::add-environment-frame env1)))
      (rtl::declare-environment-variable 'a '((:type (unsigned-byte 16))
					     (:width 16))
					env2)
      (is (equal (rtl::get-frame-property 'a :type env2)
		 '(unsigned-byte 16)))

      ;; underlying type is unchanged
      (is (equal (rtl::get-frame-property 'a :type env1)
		 '(unsigned-byte 8))))))


(test test-env-set-property-frame
  "Test we can set a property in the shallowest frame."
  (let ((env1 (rtl::add-environment-frame emptyenv)))
    (rtl::declare-environment-variable 'a '((:type (unsigned-byte 8)))
				       env1)
    (rtl::declare-environment-variable 'b '((:type (unsigned-byte 12)))
				       env1)
    (rtl::set-frame-property 'a :type '(unsigned-byte 12) env1)
    (is (equal (rtl::get-frame-property 'a :type env1)
	       '(unsigned-byte 12)))

    (let ((env2 (rtl::add-environment-frame env1)))
      (rtl::declare-environment-variable 'a '((:type (unsigned-byte 16)))
					 env2)

      (rtl::set-frame-property 'a :type '(unsigned-byte 32) env2)
      (is (equal (rtl::get-frame-property 'a :type env2)
		 '(unsigned-byte 32)))

      ;; deeper declaration is unaffected
      (is (equal (rtl::get-frame-property 'a :type env1)
		 '(unsigned-byte 12))))))


(test test-env-set-property-env
  "Test we can set a property in a deeper frame."
  (let ((env1 (rtl::add-environment-frame emptyenv)))
    (rtl::declare-environment-variable 'a '((:type (unsigned-byte 8)))
				       env1)
    (rtl::declare-environment-variable 'b '((:type (unsigned-byte 12)))
				       env1)

    (let ((env2 (rtl::add-environment-frame env1)))
      (rtl::declare-environment-variable 'a '((:type (unsigned-byte 16)))
					 env2)

      (rtl::set-environment-property 'b :type '(unsigned-byte 32) env2)
      (is (equal (rtl::get-environment-property 'b :type env2)
		 '(unsigned-byte 32)))

      ;; same declaration, not a new one
      (is (equal (rtl::get-frame-property 'b :type env1)
		 '(unsigned-byte 32))))))
