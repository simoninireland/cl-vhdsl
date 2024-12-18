;; Tests of synthesisable fragment parsing
;;
;; Copyright (C) 2024 Simon Dobson
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
(in-suite cl-vhdsl)

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


;; ---------- Argument and other detailed parsing ----------

(test test-split-args-params
  "Test we can split args and params (for modules)."
  (let ((decls '(a (b :width 2)
		 :key c (d 12))))
    (destructuring-bind (args params)
	(cl-vhdsl/rtl::split-args-params decls)
      (is (equal args
		 '(a (b :width 2))))
      (is (equal params
		 '(c (d 12)))))))


(test test-split-args
  "Test we can split args with no params (for modules)."
  (let ((decls '(a (b :width 2))))
    (destructuring-bind (args params)
	(cl-vhdsl/rtl::split-args-params decls)
      (is (equal args
		 '(a (b :width 2))))
      (is (equal params nil)))))


(test test-split-params
  "Test we can split params with no args (for modules)."
  (let ((decls '(:key c (d 12))))
    (destructuring-bind (args params)
	(cl-vhdsl/rtl::split-args-params decls)
      (is (equal args nil))
      (is (equal params '(c (d 12)))))))


;; ---------- Constants ----------

(test test-constant-literal
  "Test a literal is a constant."
  (is (rtl::constant-p 6 emptyenv)))


(test test-constant-param
  "Test a module parameter is a constant."
  (is (rtl::constant-p 'e
		       (rtl:extend-environment 'e '((:type :lisp)) emptyenv))))


(test test-constant-param
  "Test a constant is a constant."
  (is (rtl::constant-p 'e
		       (rtl:extend-environment 'e '((:constant t)) emptyenv))))


(test test-constant-operator
  "Test an operator applied to constants is a constant."
  (is (rtl::constant-p '(+ 1 2 3) emptyenv))

  ;; a is known to be a constant
  (is (rtl::constant-p '(+ 1 a 3) '((a (:width 2) (:constant t)))))

  ;; a is a parameter and therefore constant
  (is (rtl::constant-p '(+ 1 a 3) '((a (:width 2) (:type :lisp)))))

  ;; a isn't known to be constant
  (is (not (rtl::constant-p '(+ 1 a 3) '((a (:width 2)))))))


;; ---------- Structure checking ----------

(test test-add
  "Test addition maintain types."
  (is (subtypep (rtl::check '(+ 1 2) emptyenv)
		'(rtl:fixed-width-integer 4))))


(test test-assignment
  "Test we can do an assignment."
  (is (subtypep (rtl::check '(let ((a 0 :width 8))
			       (+ a 1))
			    emptyenv)
		'(rtl:fixed-width-integer 9))))


(test test-assignment-too-narrow
  "Test we catch too-wide assignments to too-narrow variables."
  (signals (rtl:type-mismatch)
    (rtl::check '(let ((a 0 :width 2))
		    (setf a 90)
		  a)
		emptyenv)))


(test test-assignment-illegal-identifier
  "Test we catch illegal identifiers."
  (signals (rtl:not-synthesisable)
    (rtl::check '(let ((reg 0 :width 2))
		  reg)
		emptyenv)))


(test test-simple-module
  "Test we can declare a very simple module."
  (is (rtl:check '(cl-vhdsl/rtl::module test1 ((a (:width 1 :direction :in))
					       (b (:width 8 :direction :out))
					       (c (:direction :in))
					       (d (:width 8 :direction :inout))
					       e)
		   (+ a b))
		 emptyenv)))


(test test-simple-module-no-argument
  "Test we can handle a very simple module with a mis-used argument."
  (signals (rtl:unknown-variable)
    (rtl::check '(cl-vhdsl/rtl::module test1 (e)
		  (+ a b))
		emptyenv)))


(test test-simple-module-param
  "Test we can declare a very simple module with a paramater."
  (is (rtl:check '(cl-vhdsl/rtl::module test1 ((a (:width 1 :direction :in))
					       :key (b 25))
		    (+ a b))
		 emptyenv)))



(test test-complex-module-legal-assignments
  "Test we can check a complex module successfully."
  (is (rtl:check '(rtl::module test ((clk :width 1 :direction :in)
				     (a :width 8 :direction :in)
				     (b :width 4 :direction :out)
				     :key e (f 45))
		   (let ((x 0 :width 8)
			 (y 10 :width 8))
		     (when (rtl::posedge clk)
		       (setf x (+ x b) :sync t)     ; to a register we define
		       (setf b (+ x 5) :sync t))))  ; to an :out argument
		 emptyenv)))


(test test-complex-module-illegal-in-assignment
  "Test we can detect an illegal assignment to an :in argument."
  (signals (rtl:not-synthesisable)
    (rtl:check '(rtl::module test ((clk :width 1 :direction :in)
				   (a :width 8 :direction :in)
				   (b :width 4 :direction :out)
				   :key e (f 45))
		 (let ((x 0 :width 8)
		       (y 10 :width 8))
		   (when (rtl::posedge clk)
		     (setf a (+ x b)))))
	       emptyenv)))


(test test-complex-module-illegal-param-assignment
  "Test we can detect an illegal assignment to a parameter"
  (signals (rtl:not-synthesisable)
    (rtl:check '(rtl::module test ((clk :width 1 :direction :in)
				   (a :width 8 :direction :in)
				   (b :width 4 :direction :out)
				   :key e (f 45))
		 (let ((x 0 :width 8)
		       (y 10 :width 8))
		   (when (rtl::posedge clk)
		     (setf f (+ x b)))))
	       emptyenv)))


;; ---------- Synthesis ----------

;; It's not really possible to check the results of synthesis, so
;; we just make sure about the errors

(test test-synthesise-module
  "Test we can syntheise a module with a variety of features."
  (is (rtl:synthesise '(rtl::module test ((clk :width 1 :direction :in)
					  (a :width 8 :direction :in)
					  (b :width 4)
					  :key e (f 45))
			(let ((x 0 :width 8)
			      (y 10 :width 8))
			  (when (rtl::posedge clk)
			    (setf x (+ x b) :sync t))))
		      :statement)))


(test test-synthesise-constant
  "Test we can synthesise constants."
  (is (rtl:synthesise '(rtl::module test ((clk :width 1 :direction :in)
					  (a :width 8 :direction :in)
					  (b :width 4 :direction :out)
					  :key e (f 45))
			(when (rtl::posedge clk)
			  (setf b (+ 1 2))))
		      :statement)))


(test test-synthesise-if-statement
  "Test we can synthesise if statements."

  ;; full statement
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))
			    (setf a (+ 1 3))))
		      emptyenv))

  ;; no else branch
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))))
		      emptyenv)))


(test test-blink
  "Test we can synthesise the blink application."
  (is (rtl:synthesise '(rtl::module blink
			((clk  :width 1 :direction :in)
			 (leds :width 5 :direction :out)
			 :key (width 5) (delay 22))
			(let ((counter 0 :width (+ bits delay))
			      (out 0 :width bits))
			  (when (rtl::posedge clk)
			    (setf counter (+ counter 1))
			    (setf out (rtl::>> counter delay)))
			  (rtl::wire leds out))))))
