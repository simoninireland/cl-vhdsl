;; Tests of synthesisable fragment passes and synthesis
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


;; ---------- Types ----------

(test test-unsigned-range
  "Test the range of an unsigned fixed-width integer."
  (is (typep 12 '(rtl::fixed-width-unsigned 8)))
  (is (typep 12 '(rtl::fixed-width-unsigned 16)))

  (is (typep 512 '(rtl::fixed-width-unsigned 16)))
  (is (not (typep 512 '(rtl::fixed-width-unsigned 8))))

  (is (not (typep -64 '(rtl::fixed-width-unsigned 8)))))


(test test-signed-range
  "Test the range of a signed fixed-width integer."
  (is (typep 12 '(rtl::fixed-width-signed 8)))
  (is (typep 12 '(rtl::fixed-width-signed 16)))
  (is (typep 127 '(rtl::fixed-width-signed 8)))
  (is (typep -128 '(rtl::fixed-width-signed 8)))

  (is (not (typep 128 '(rtl::fixed-width-signed 8))))
  (is (not (typep -129 '(rtl::fixed-width-signed 8))))
  (is (not (typep -64 '(rtl::fixed-width-signed 4)))))


(test test-bitwidths-integer-types
  "Test we can extract the widths of integer types."
  (is (= (rtl::bitwidth '(rtl:fixed-width-unsigned 8) ()) 8))
  (is (= (rtl::bitwidth '(rtl:fixed-width-signed 8) ()) 8)))


(test test-bitwidths-integer-constants
  "Test we can extract bit widths of integer constants."
  (is (= (rtl::bitwidth 0 ()) 0))
  (is (= (rtl::bitwidth 1 ()) 1))
  (is (= (rtl::bitwidth 2 ()) 2))
  (is (= (rtl::bitwidth 127 ()) 7))

  (is (= (rtl::bitwidth -127 ()) 8))
  (is (= (rtl::bitwidth -1 ()) 2)))


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


;; ---------- Lispification and evaluation ----------

(test test-eval-empty
  "Test we can construct and evaluate an expression in an empty environment."
  (is (= (rtl::eval-in-static-environment '(+ 1 2) emptyenv)
	 3)))


(test test-eval-env
  "Test we can evaluate an expression with a static variable."
  (let ((env (rtl:extend-environment 'a '((:initial-value 2)
					  (:parameter t))
				     emptyenv)))
    (is (= (rtl::eval-in-static-environment '(+ 1 a) env)
	 3))))


(test test-eval-not-static
  "Test we don't pick up non-static variables."
  (let* ((env1 (rtl:extend-environment 'a '((:initial-value 2)
					    (:parameter t))
				       emptyenv))
	 (env2  (rtl:extend-environment 'b '((:initial-value 3))
				       env1)))
    (is (= (rtl::eval-in-static-environment '(+ 1 a) env2)
	   3))

    (signals (unbound-variable)
      (rtl::eval-in-static-environment '(+ a b) env2))))


(test test-eval-non-lisp
  "Test we synthesise the right operator for << and >>."
  (is (= (rtl::eval-in-static-environment '(rtl::<< 1 3) emptyenv)
	 8))
  (is (= (rtl::eval-in-static-environment '(rtl::>> 8 3) emptyenv)
	 1)))


;; ---------- Type and width checking ----------

(test test-literal-widths
  "Test we can extract the types of literals."
  (is (subtypep (rtl:typecheck 2 emptyenv)
		'(rtl::fixed-width-unsigned 2)))

  (is (not (subtypep (rtl:typecheck -2 emptyenv)
		     '(rtl::fixed-width-signed 2))))
  (is (subtypep (rtl:typecheck -2 emptyenv)
		'(rtl::fixed-width-signed 3))))


(test test-add-widths
  "Test we can determine the widths of additions."
  (is (subtypep (rtl:typecheck '(+ 1 1)
			       emptyenv)
		'(rtl::fixed-width-unsigned 2)))
  (is (subtypep (rtl:typecheck '(+ 15 2)
			       emptyenv)
		`(rtl::fixed-width-unsigned 5)))

  (is (not (subtypep (rtl:typecheck '(+ 15 -2)
				    emptyenv)
		     `(rtl::fixed-width-unsigned 5))))
  (is (subtypep (rtl:typecheck '(+ 15 -2)
			       emptyenv)
		`(rtl::fixed-width-signed 5))))


(test test-width-subtractions
  "Test we can extract the widths of subtractions."
    (is (subtypep (rtl:typecheck '(- 1)
			       emptyenv)
		  '(rtl::fixed-width-signed 2)))
    (is (subtypep (rtl:typecheck '(- 2 1)
			       emptyenv)
		  '(rtl::fixed-width-signed 3))))


(test test-width-shifts
  "Test we can extract the widths of shifts."
  (is (subtypep (rtl:typecheck '(rtl::<< 1 2)
			       emptyenv)
		'(rtl::fixed-width-unsigned 4)))
  (is (subtypep (rtl:typecheck '(rtl::<< 15 15)
			       emptyenv)
		'(rtl::fixed-width-unsigned 19)))

  (is (subtypep (rtl:typecheck '(rtl::>> 16 4)
			       emptyenv)
		'(rtl::fixed-width-unsigned 1)))

  ;; wrong number of arguments
  (dolist (op '(rtl::<< rtl::>>))
    (signals (rtl:not-synthesisable)
      (rtl:typecheck `(,op 1 2 3) emptyenv))
    (signals (rtl:not-synthesisable)
      (rtl:typecheck `(,op 3) emptyenv))))


(test test-width-bits
  "Test we can extract bits from a value."
  (is (subtypep (rtl:typecheck '(let ((a #2r10110))
				 (rtl::bits a 2 1))
			       emptyenv)
		'(rtl::fixed-width-unsigned 2))))


(test test-let-single
  "Test we can typecheck an expression."
  (is (subtypep (rtl:typecheck '(let ((a 1 :width 5))
				 (+ 1 a))
			       emptyenv)
		'(rtl::fixed-width-unsigned 6))))


(test test-let-single-infer-width
  "Test we can infer a width."
  (is (subtypep (rtl:typecheck '(let ((a 1))
				 (+ 1 a))
			       emptyenv)
		'(rtl::fixed-width-unsigned 2))))


(test test-let-double
  "Test we can typecheck an expression with two variables."
  (is (subtypep (rtl:typecheck '(let ((a 1 :width 8)
				      (b 6 :width 16))
				 (+ a b))
			       emptyenv)
		'(rtl::fixed-width-unsigned 24))))


(test test-let-too-narrow
  "Test we pick up too-wide initial values."
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(let ((a 100 :width 5))
		     (+ 1 a))
		   emptyenv)))


(test test-let-widen
  "Test we can take the width from a given type."
  (is (subtypep (rtl:typecheck '(let ((a 5 :type (rtl::fixed-width-unsigned 8)))
				 (+ 1 a))
			       emptyenv)
		'(rtl::fixed-width-unsigned 9))))


(test test-let-scope
  "Test we catch variables not declared."
  (signals (rtl:unknown-variable)
    (rtl:typecheck '(let ((a 1))
		     (+ 1 b))
		   emptyenv)))


(test test-let-result
  "Test we pick up the right result type."
  (is (subtypep (rtl:typecheck '(let ((a 99)
				      (b 100 :width 8))
				 (+ b 1)
				 (+ b a b))
			       emptyenv)
		'(rtl::fixed-width-unsigned 10))))


(test test-let-constant
  "Test we admit constant bindings."
  (is (subtypep (rtl:typecheck '(let ((a 15 :as :constant))
				 a)
			       emptyenv)
		'(rtl::fixed-width-unsigned 4))))


(test test-setq
  "Test we can typecheck the SETQ form."
  (is (subtypep (rtl:typecheck '(let ((a 12))
				 (setq a 9))
			       emptyenv)
		'(rtl::fixed-width-unsigned 8)))

  (signals (rtl:not-synthesisable)
    (rtl:typecheck '(let ((a 12 :as :constant))
		     (setq a 9))
		   emptyenv)))


(test test-setf-as-setq
  "Test we can convert a simple SETF into a SETQ."
  (is (subtypep (rtl:typecheck '(let ((a 12))
				 (setf a 9))
			       emptyenv)
		'(rtl::fixed-width-unsigned 8)))

  (is (subtypep (rtl:typecheck '(let ((a 12))
				 (setf a 9 :sync t))
			       emptyenv)
		'(rtl::fixed-width-unsigned 8))))


(test test-let-naked
  "Test that we accept "naked" declarations."
  (is (subtypep (rtl:typecheck '(let ((a 10)
				      b)
				 (+ a b))
			       emptyenv)
		`(rtl::fixed-width-unsigned ,(1+ rtl::*default-register-width*)))))


(test test-if-then-else
  "Test we can check a complete if form."
  (is (subtypep (rtl:typecheck '(if 1
				 (+ 1 2)
				 (+ 16 8))
			       emptyenv)
		 '(rtl::fixed-width-unsigned 6))))


(test test-if-then
  "Test we can check an incomplete if form."
  (is (subtypep (rtl:typecheck '(if 1
				 (+ 1 2))
			       emptyenv)
		 '(rtl::fixed-width-unsigned 3))))


(test test-case-compatible
  "Test we can typecheck cases with compatible clauses."
  (is (subtypep (rtl:typecheck '(let ((a 12)
				      b)
				 (case a
				   (1
				    (setf b 23))
				   (2
				    (setf b 34))
				   (t
				    (setf b 0))))
			       emptyenv)
		 '(rtl::fixed-width-unsigned 8))))


(test test-case-incompatible
  "Test we catch cases with incompatible clauses."
  (signals (rtl:type-mismatch)
    (is (rtl:typecheck '(let ((a 12)
			      b)
			 (case a
			   (1
			    (setf b 23))
			   (2456
			    (setf b 34))))
		       emptyenv)
	'(rtl::fixed-width-unsigned 8))))


(test test-assignment-same-width
  "Test we can assign."
  (is (subtypep (rtl:typecheck '(let ((a 10))
				 (setf a 12))
			       emptyenv)
		'(rtl::fixed-width-unsigned 5))))


(test test-assignment-same-width-sync
  "Test we can assign synchronously (same types)."
  (is (subtypep (rtl:typecheck '(let ((a 10))
				 (setf a 12 :sync t))
			       emptyenv)
		'(rtl::fixed-width-unsigned 5))))


(test test-assignment-too-wide
  "Test we can't assign a value that's too wide."
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(let ((a 10))
		     (setf a 120))
		   emptyenv)))


(test test-assignment-out-of-scope
  "Test we can't assign to a non-existent variable."
  (signals (rtl:unknown-variable)
    (rtl:typecheck '(let ((a 10))
		     (setf b 12))
		   emptyenv)))


(test test-assignment-constant
  "Test we can't assign to a constant variable."
  (signals (rtl:not-synthesisable)
    (rtl:typecheck '(let ((a 10 :as :constant))
		     (setf a 12))
		   emptyenv)))


(test test-typecheck-constant
  "Test that a constants typecheck properly."
  (is (subtypep (rtl:typecheck '(let ((a 23 :as :constant))
				 a)
			       emptyenv)
		'(rtl::fixed-width-unsigned 8))))


(test test-bit-access
  "Test we can typecheck a bit access to a variable."
  (is (subtypep (rtl:typecheck '(let ((a 1 :width 8))
				 (setf a (+ a (bit a 0))))
			       emptyenv)
		'(rtl::fixed-width-unsigned 9))))


(test test-typecheck-module
  "Test we can type-check a module with a variety of features."
  (is (subtypep (type-of (rtl:typecheck '(rtl::module test ((clk :width 1 :direction :in)
							    (a :width 8 :direction :in)
							    (b :width 4)
							    :key e (f 45))
					  (let ((x 0 :width 8)
						(y 10 :width 8))
					    (setf x (+ x b) :sync t)))
					emptyenv))
		'rtl::module-interface)))


;; ---------- Typechecking setf to generalised places ----------

(test test-setf-variable
  "Test we can setf to a variable."
  (is (rtl:typecheck '(let ((a 12))
		       (setf a 14))
		     emptyenv)))


(test test-setf-variable-bits
  "Test we can setf to bits in a variable."
  (is (subtypep (rtl:typecheck '(let ((a 12))
				 (setf (rtl::bits a 1 0) 2))
			       emptyenv)
		'(rtl::fixed-width-unsigned 2)))

  ;; fails because default width of a is too small
  (signals (rtl:not-synthesisable)
    (rtl:typecheck '(let ((a 12))
		     (setf (rtl::bits a 6 0) 0))
		   emptyenv))

  ;; fixed with an explicit width
  (is (subtypep (rtl:typecheck '(let ((a 12 :width 8))
				 (setf (rtl::bits a 6 0) 0))
			       emptyenv)
		'(rtl::fixed-width-unsigned 2))))


(test test-the
  "Test we can typecheck THE."
  (is (subtypep (rtl:typecheck '(the (rtl::fixed-width-unsigned 8) 12)
			       emptyenv)
		'(rtl::fixed-width-unsigned 8)))

  (signals (rtl:type-mismatch)
    (rtl:typecheck '(the (rtl::fixed-width-unsigned 8) 1230)
		   emptyenv)))


;; ---------- Floating LET blocks ----------

(test test-let-float
  "Test that nested LETs float."
  (destructuring-bind (form decls)
      (rtl::float-let-blocks '(let ((a 1 :width 8))
			       (setq a 12)
			       (let ((b (+ a 1)))
				 (setq a (* a b)))))
    (is (equal (mapcar #'car decls)
	       '(a b)))

    ;; this may change when we float constant initial values
    (is (equal (mapcar #'cadr declsv)
	       '(0 0)))))


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


;; ---------- Macro expansion ----------

(test test-expand-cond
  "Test we can expand the COND macro."
  (let* ((f '(cond ((= a 12)
		    (+ a 1))
	      ((> a 34)
	       (+ a 67))
	      (t
	       0)))
	 (g (rtl::expand-macros f)))
    (is (equal g '(if (= a 12)
		   (+ a 1)
		   (if (> a 34)
		       (+ a 67)
		       (the t 0)))))))


(test test-no-expand-and
  "Test we don't expand AND, which is a macro in Common Lisp."
  (is (equal (rtl::expand-macros '(and a b))
	     '(and a b))))


(test test-extract-runs
  "Test we can extract runs of symbols from a list."
  (is (null (rtl::extract-runs '())))
  (is (equal (rtl::extract-runs '(a))
	     '((a 0 0))))
  (is (equal (rtl::extract-runs '(a a a))
	     '((a 2 0))))
  (is (equal (rtl::extract-runs '(a a a b b b))
	     '((a 5 3) (b 2 0))))
  (is (equal (rtl::extract-runs '(a a a b a a))
	     '((a 5 3) (b 2 2) (a 1 0))))
  (is (equal (rtl::extract-runs '(a a a b a a c))
	     '((a 6 4) (b 3 3) (a 2 1) (c 0 0)))))


;; Should this function be in utils?
(test test-duplicate-keys
  "Test we can detect duplicate keys in alists."
  (is (rtl::duplicate-keys-p '((a 12) (b 13) (a 1))))
  (is (rtl::duplicate-keys-p '((a 12) (a 13))))

  (is (not (rtl::duplicate-keys-p '())))
  (is (not (rtl::duplicate-keys-p '((a 12)))))
  (is (not (rtl::duplicate-keys-p '((a 12) '(b 12))))))


(test test-extract-bitfields
  "Test we can extract bitfields from patterns."
  (is (equal (rtl::extract-bitfields '(a))
	     '((a 0 0))))
  (is (equal (rtl::extract-bitfields '(1 a))
	     '((1 1 1) (a 0 0))))
  (signals (rtl:bitfield-mismatch)
    (rtl::extract-bitfields '(1 a 1 a))))


;; ---------- Shadowing ----------

(test test-detect-shadowed
  "Test we can detect a shadowed variable."
  (signals (rtl:duplicate-variable)
    (rtl::detect-shadowing '(let ((a 12))
			     (setq a (+ a 1))
			     (let ((b 1)
				   (a 34))
			       (setq a (+ b a))))
			   emptyenv))

  ;; no shadowing
  (is (rtl::detect-shadowing '(let ((a 12))
			       (setq a (+ a 1))
			       (let ((b 1)
				     (c 34))
				 (setq c (+ b a))))
			     emptyenv)))


(test test-detect-shadowed-module
  "Test we can detect shadowing in the body of a module."
  (signals (rtl:duplicate-variable)
    (rtl::detect-shadowing '(rtl::module test ()
			     (let ((a 12))
			       (setq a (+ a 1))
			       (let ((b 1)
				     (a 34))
				 (setq a (+ b a)))))
			   emptyenv)))


;; ---------- Re-writing variables ----------

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


;; ---------- Synthesis ----------

;; It's not generally possible to check the results of synthesis, so
;; we just make sure about the errors

(test test-operators
  "Test we can synthesise operators."
  ;; arithmetic
  (dolist (op '(+ - *))
    ;; conventional two-operand
    (is (rtl:synthesise `(,op 1 2)
			:inexpression))

    ;; Lisp-y multi-operand
    (is (rtl:synthesise `(,op 1 2 3)
			:inexpression)))

  ;; unary minus
  (is (rtl:synthesise `(- 1)
		      :inexpression))

  ;; nested
  (is (rtl:synthesise `(+ 1 (- (* 2 3 -1)))
		      :inexpression))

  ;; shifts
  (dolist (op '(rtl::<< rtl::>>))
    ;; conventional two-operand
    (is (rtl:synthesise `(,op 1 2)
			:inexpression))))


(test test-synthesise-setf
  "Test we can synthesise assignments."
  ;; as statements
  (is (rtl:synthesise '(setf a 5)
		      :inblock))
  (is (rtl:synthesise '(setf a 5 :sync t)
		      :inblock)))


(test test-synthesise-progn
  "Test we can synthesise PROGN forms."
  (is (rtl:synthesise '(progn
			(setf a 5)
			(setf b 34))
		      :inblock)))


(test test-synthesise-binders
  "Test we can synthesise binders."
  ;; as statements
  (is (rtl:synthesise `(let ((a 1 :width 8))
			 (setf a (+ a 1)))
		      :inblock))

  ;; with constants
  (is (rtl:synthesise '(let ((a 1 :width 8)
			     (b 23 :as :constant))
			(setf c (+ a b)))
		      :inblock))

  ;; with wires
  (is (rtl:synthesise '(let ((a 1 :width 8)
			     (b 0 :width 16 :as :wire))
			(setf c (+ a b)))
		      :inblock))

  ;; can't return values in statement role
  (signals (error)
    (rtl:synthesise `(let ((a 1 :width 8))
		       (+ a 1))
		    :inblock)))


(test test-synthesise-module
  "Test we can syntheise a module with a variety of features."
  (is (rtl:synthesise '(rtl::module test ((clk :width 1 :direction :in)
					  (a :width 8 :direction :in)
					  (b :width 4 :direction :in)
					  :key e (f 45))
			(let ((x 0 :width 8)
			      (y 10 :width 8)
			      (z 44 :as :constant))
			  (rtl::@ (rtl::posedge clk)
			    (setf x (+ x b) :sync t))))
		      :toplevel)))


(test test-synthesise-if-statement
  "Test we can synthesise if forms."
  ;; as statements
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))
			    (setf a (+ 1 3))))
		      :inmodule))
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))

			    ;; a two-form else branch
			    (setf a (+ 1 3))
			    (setf a 12)))
		      :inmodule))
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (progn
			      ;; a two-form else branch
			      (setf a (+ 1 2))
			      (setf a (+ 1 3)))
			    (setf a 12)))
		      :inmodule))

  ;; no else branch
  (is (rtl:synthesise '(let ((a 0 :width 4))
			(if (logand 1 1)
			    (setf a (+ 1 2))))
		      :inblock)))


(test test-synthesise-cond
  "Test we can synthesise a COND, which is a macro needing to be expanded first."
  (is (rtl:synthesise (rtl::expand-macros'(cond ((rtl::<< a 1)
						 (setf b 1))
						((rtl::<< a 2)
						 (setf b 2))
						(t
						 (setf b 3))))
		      :inblock)))


(test test-synthesise-case
  "Test we can synthesise a CASE."
  (is (rtl:synthesise '(let ((a 12)
			     (b 0))
			(case a
			  (1
			   (setf b 23))
			  (2
			   (setf b 34 :sync t)
			   (setf a 0))
			  (t
			   (setf b 0))))
		      :inblock)))


(test test-blink
  "Test we can synthesise the blink application."
  (is (rtl:synthesise '(rtl::module blink
			((clk  :width 1 :direction :in)
			 (leds :width 5 :direction :out)
			 :key (width 5) (delay 22))
			(let ((counter 0 :width (+ bits delay))
			      (out 0 :width bits))
			  (rtl::@ (rtl::posedge clk)
				  (setf counter (+ counter 1))
				  (setf out (rtl::>> counter delay)))
			  (setf leds out)))
		      :toplevel)))
