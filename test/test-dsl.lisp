;; Tests of DSL definition facility
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
(in-suite cl-vhdsl/dsl)
(declaim (optimize debug))


;; ---------- DSL definition ----------

(test test-dsl-define
  "Test we can define a new DSL."
  (is (null dsl::*current-dsl*))

  (dsl:defdsl test-dsl-define
      (:documentation "A test DSL"))

  (is (not (null (symbol-value 'test-dsl-define))))
  (is (null dsl::*current-dsl*)))


(test test-dsl-in-dsl
  "Test IN-DSL works as expected."
  (is (null dsl::*current-dsl*))

  (dsl:defdsl test-dsl-in-dsl
      (:documentation "A test DSL"))

  (dsl:in-dsl test-dsl-in-dsl)
  (is (eql dsl::*current-dsl* test-dsl-in-dsl))

  (dsl:in-dsl)
  (is (null dsl::*current-dsl*)))


;; ---------- Functions over DSLs ----------

(test test-dsl-fun/dsl
  "Test we can declare a function over a DSL."
  (dsl:defdsl test-dsl-for-functions
      (:documentation "A DSL to receive functions."))

  (dsl:defun/dsl testdef (form env)
    (:documentation "A test function using an extra argument")
    (:dsl test-dsl-for-functions))

  (is (dsl::dsl-function-p 'testdef test-dsl-for-functions)))


(test test-dsl-fun/dsl-duplicate
  "Test we can't re-declare a function over a DSL."
  (dsl:defdsl test-dsl-for-functions-duplicate
      (:documentation "A DSL to receive functions."))

  (dsl:defun/dsl testdef (form env)
    (:documentation "A test function using an extra argument")
    (:dsl test-dsl-for-functions-duplicate))

  (signals (dsl:duplicate-dsl-function)
    (dsl:defun/dsl testdef (form env)
      (:documentation "A test function using an extra argument")
      (:dsl test-dsl-for-functions-duplicate))))


(test test-dsl-form
  "Test we can declare a form within a DSL."
  (dsl:defdsl test-dsl-for-forms
      (:documentation "A DSL to receive forms."))

  (dsl:deform/dsl +
      (:dsl test-dsl-for-forms))

  (is (dsl::dsl-form-p '+ test-dsl-for-forms)))


(test test-dsl-function-over-form
  "Test we can define a DSL function over a form."

  (dsl:defdsl test-dsl-function-over-forms
      (:documentation "A DSL to receive a form and function."))

  (dsl:deform/dsl +
    (:dsl test-dsl-function-over-forms))

  (dsl:defun/dsl testdef (form env)
    (:documentation "A test function using an extra argument")
    (:dsl test-dsl-function-over-forms))

  ;; function defined over the whole form
  (dsl:defun/form testdef ((form integer))
    (:dsl test-dsl-function-over-forms)
    form)

  (is (= (testdef 12 '())
	 12))

  ;; function defined over a declared form
  (dsl:defun/form testdef + (&rest args)
    (:dsl test-dsl-function-over-forms)
    (apply #'+ (mapcar (rcurry #'testdef '()) args)))

  (is (= (testdef '(+ 1 2 3) '())
	 (+ 1 2 3))))


(test test-dsl-function-together
  "Test we can define a form and its functions."
  (dsl:defdsl test-dsl-function-together
      (:documentation "A DSL to receive a form and function."))

  (dsl:defun/dsl testdef (form env)
    (:documentation "A test function using an extra argument")
    (:dsl test-dsl-function-together))

  (dsl:defun/dsl testagain (form)
    (:documentation "Another test function")
    (:dsl test-dsl-function-together))

  (dsl:deform/dsl + (&rest args)
    (:dsl test-dsl-function-together)

    (testdef
     (apply #'+ (mapcar (rcurry #'testdef '()) args)))

    (testagain
     (list 4 5 6 7)))

  (is (= (testdef '(+ 1 2 3) '())
	 (+ 1 2 3)))
  (is (equal (testagain '(+ 1 2))
	     '(4 5 6 7))))
