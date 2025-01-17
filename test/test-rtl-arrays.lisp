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
