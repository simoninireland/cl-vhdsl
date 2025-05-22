;; Tests of arrays
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


;; ---------- Array declarations ----------

(test test-array-decl
  "Test we can declare arrays."
  (is (subtypep (rtl:typecheck '(make-array '(16)
				 :element-type (unsigned-byte 8)))
		'(array (unsigned-byte 8) (16))))

  ;; version without the Lisp-compatible quote on the shape
  (is (subtypep (rtl:typecheck '(make-array (16)
				 :element-type (unsigned-byte 8)))
		'(array (unsigned-byte 8) (16))))

  ;; at the moment we only allow one dimension
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(make-array '(16 16)
		     :element-type (unsigned-byte 8)))))


(test test-array-bind
  "Test we can bind arrays in LET forms."
  (let ((p (copy-tree '(let ((a (make-array '(16)
				 :element-type (unsigned-byte 8)))
			     (b 0))
			(setq b 12)))))
    (is (subtypep (rtl:typecheck p)
		  '(unsigned-byte 8)))))


(test test-synthesise-array-decl
  "Test we can synthesise array declarations."
  (let ((p (copy-tree '(let ((a (make-array '(16)
				 :element-type (unsigned-byte 8))
			      :type (array (unsigned-byte 8) (32)))
			     (b (make-array '(8)
				 :element-type (unsigned-byte 8))
			      :type (array (unsigned-byte 8) (16))
			      :as :wire)
			     (c 10 :type (unsigned-byte 8)))
			(setf c 0)))))
    (rtl:typecheck p)
    (is (rtl:synthesise p :inmodule))))


(test test-synthesise-array-decl-type-inferred
  "Test we can synthesise array declarations when we infer the type of the array."
  (let ((p '(let ((a (make-array '(16)
		      :element-type (unsigned-byte 8)))
		  (b (make-array '(8)
		      :element-type (unsigned-byte 8))
		   :as :wire)
		  (c 10))
	     (setf c 100))))
    (rtl:typecheck p)
    (is (rtl:synthesise p :inmodule))))


(test test-synthesise-array-init-from-data
  "Test we can synthesise array declarations with initial data inline."
  (let ((p (copy-tree '(let ((b (make-array '(4)
				 :element-type (unsigned-byte 8)
				 :initial-contents '(1 2 3 4))
			      :as :register)
			     (c 10))
			(setf c (aref b 1))))))
    (rtl:typecheck p)
    (is (rtl:synthesise p :inmodule))))


(test test-synthesise-array-init-from-file
  "Test we can synthesise array declarations with initial data from a file."
  (rtl::clear-module-late-initialisation)

  (let ((p (copy-tree '(let ((b (make-array '(4)
				 :element-type (unsigned-byte 8)
				 :initial-contents '(:file "ttt.hex"))
			      :as :register)
			     (c 10))
			(setf c (aref b 1))))))
    (rtl:typecheck p)
    (rtl:synthesise p :inmodule)
    (is (rtl::module-late-initialisation-p))))


;; ---------- Array accesses ----------

(test test-aref-simple
  "Test we can index into an array."
  (let ((p (copy-tree '(let ((a (make-array '(16)
				 :element-type (unsigned-byte 32))))
			(setf (aref a 8) (aref a 0))))))
    (is (subtypep (rtl:typecheck p)
		  '(unsigned-byte 32)))))


(test test-aref-bits
  "Test we can bit-index into an element of an array."
  (let ((p (copy-tree '(let ((a (make-array '(16)
				 :element-type (unsigned-byte 32))))
			(setf (rtl::bref (aref a 8) 3 :end 0)
			 (rtl::bref (aref a 0) 3 :end 0))))))
    (is (subtypep (rtl:typecheck p)
		  '(unsigned-byte 32)))))


(test test-synthesise-aref-simple
  "Test we can synthesise a simple array reference."
  (let ((p (copy-tree '(let ((a (make-array '(16)
				 :element-type (unsigned-byte 32))))
			(setf (aref a 8) (aref a 0))))))
    (rtl:typecheck p)
    (is (rtl:synthesise p :inblock))))


;; ---------- Initialisation ----------

(test test-typecheck-array-initialiser
  "Test we can typecheck an array initialiser."
  (let ((p (copy-tree  '(let ((a (make-array (5)
				  :initial-contents (1 2 3 4 5))))
			 (aref a 0)))))
    (is (subtypep (rtl:typecheck p)
		  '(unsigned-byte 8))))

  (signals (rtl:shape-mismatch)
    (rtl:typecheck '(let ((a (make-array (5)
			      :initial-contents (1 2 3))))
		     (aref a 0)))))


(test test-typecheck-array-initialiser-bad-value
  "Test we can detect a badly-typed value in an array initialiaser."
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(let ((a (make-array (5)
			      :element-type '(unsigned-byte 4)
			      :initial-contents (1 2 35))))
		     (aref a 0)))))


(test test-syntheseise-array-init
  "Test we can synthesise array initialisation."
  (let ((p (copy-tree '(let ((a (make-array '(10)
				 :initial-contents '(1 2 3 4 5 6 7 8 9 10)))
			     (b 0))
			(setf b (aref a 1))))))
    (rtl:typecheck p)
    (is (rtl:synthesise p :inblock))))


;; The next test uses ROM data from the SAP-1 example

(test test-synthesise-array-init-from-example
  "Test we can synthesise array initialisation from a file."
  (let* ((fn (pathname-relative-to-project-root "examples/sap-1-raw/program.bin"))
	 (p (copy-tree `(let ((a (make-array '(10)
					     :initial-contents '(:file ,fn)))
			      (b 0))
			  (setf b (aref a 1))))))
    (rtl:typecheck p)
    (is (rtl:synthesise p :inblock))
    (rtl::run-module-late-initialisation)))
