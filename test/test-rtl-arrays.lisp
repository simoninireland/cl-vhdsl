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
(declaim (optimize debug))


;; ---------- Array declarations ----------

(test test-array-decl
  "Test we can declare arrays."
  (is (subtypep (rtl:typecheck '(make-array '(16)
				 :element-type (rtl:fixed-width-unsigned 8))
			       emptyenv)
		'(array (rtl:fixed-width-unsigned 8) 16)))

  ;; version without the Lisp-compatible quote on the shape
  (is (subtypep (rtl:typecheck '(make-array (16)
				 :element-type (rtl:fixed-width-unsigned 8))
			       emptyenv)
		'(array (rtl:fixed-width-unsigned 8) 16)))

  ;; at the moment we only allow one dimension
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(make-array '(16 16)
		     :element-type (rtl:fixed-width-unsigned 8))
		   emptyenv)))


(test test-array-bind
  "Test we can bind arrays in LET forms."
  (is (subtypep (rtl:typecheck '(let ((a (make-array '(16) :element-width 8))
				      (b 0))
				 (setq b 12))
			       emptyenv)
		'(rtl::fixed-width-unsigned 8))))


(test test-synthesise-array-decl
  "Test we can synthesise array declarationss."
  (is (rtl:synthesise '(let ((a (make-array '(16) :element-width 8))
			     (b (make-array '(8) :element-width 8) :as :wire)
			     (c 10))
			(setf b 0))
		      :inmodule)))


;; ---------- Array accesses ----------

(test test-aref-simple
  "Test we can index into an array."
  (is (subtypep (rtl:typecheck '(let ((a (make-array '(16) :element-width 32)))
				 (setf (aref a 8) (aref a 0)))
			       emptyenv)
		'(rtl::fixed-width-unsigned 32))))


(test test-aref-bits
  "Test we can bit-index into an element of an array."
  (is (subtypep (rtl:typecheck '(let ((a (make-array '(16) :element-width 32)))
				 (setf (rtl::bits (aref a 8) 3 :end 0)
				       (rtl::bits (aref a 0) 3 :end 0)))
			       emptyenv)
		'(rtl::fixed-width-unsigned 32))))


(test test-synthesise-aref-simple
  "Test we can synthesise a simple array reference."
  (is (rtl:synthesise '(let ((a (make-array '(16) :element-width 32)))
			(setf (aref a 8) (aref a 0)))
		      :inblock)))


;; ---------- Initialisation ----------

(test test-typecheck-array-initialiser
  "Test we can typecheck an array initialiser."
  (is (subtypep (rtl:typecheck '(let ((a (make-array (5)
					  :initial-contents (1 2 3 4 5))))
				 (aref a 0))
			       emptyenv)
		'(rtl:fixed-width-unsigned 1)))

  (signals (rtl:shape-mismatch)
    (rtl:typecheck '(let ((a (make-array (5)
			      :initial-contents (1 2 3))))
		     (aref a 0))
		   emptyenv)))


(test test-typecheck-array-initialiser-bad-value
  "Test we can detect a badly-typed value in an array initialiaser."
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(let ((a (make-array (5)
			      :element-type '(rtl:fixed-width-unsigned 4)
			      :initial-contents (1 2 35))))
		     (aref a 0))
		   emptyenv)))


(test test-syntheseise-array-init
  "Test we can synthesise array initialisation."
  (is (rtl:synthesise '(let ((a (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)))
			     (b 0))
			(setf b (aref a 1)))
		      :inblock)))


(test test-read-array-data
  "Test we can read array data from a stream."
  (with-input-from-string (str "1d 2 3 4 5f")
    (is (equal (rtl::read-array-data-from-stream str)
	       '(#16r1d 2 3 4 #16r5f))))

  ;; can handle embedded newlines and whitespace
  (with-input-from-string (str (format nil "1d    2~%    3 4~%~%~%5f"))
    (is (equal (rtl::read-array-data-from-stream str)
	       '(#16r1d 2 3 4 #16r5f)))))


;; The next two tests use ROM data from the SAP-1 example

(test test-read-array-file
  "Test we can read array data from a file."
  (is (equal (length (rtl::load-array-data-from-file "examples/sap-1-raw/program.bin"))
	     16)))


(test test-synthesise-array-init-from-file
  "Test we can synthesise array initialisation from a file."
  (is (rtl:synthesise '(let ((a (make-array '(10) :initial-contents '(:file "examples/sap-1-raw/program.bin")))
			     (b 0))
			(setf b (aref a 1)))
		      :inblock)))
