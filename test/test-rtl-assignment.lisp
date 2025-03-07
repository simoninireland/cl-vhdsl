;; Tests of simple assignments and generalised places
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
(declaim (optimize (size 0)))

;; ---------- Simple assignment (SETQ) ----------

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


(test test-assignment-same-width
  "Test we can assign."
  (is (subtypep (rtl:typecheck '(let ((a 10))
				 (setq a 12))
			       emptyenv)
		'(rtl::fixed-width-unsigned 5))))


(test test-assignment-same-width-sync
  "Test we can assign synchronously (same types)."
  (is (subtypep (rtl:typecheck '(let ((a 10))
				 (setq a 12 :sync t))
			       emptyenv)
		'(rtl::fixed-width-unsigned 5))))


(test test-assignment-too-wide
  "Test we can't assign a value that's too wide."
  (signals (rtl:type-mismatch)
    (rtl:typecheck '(let ((a 10))
		     (setq a 120))
		   emptyenv)))


(test test-assignment-out-of-scope
  "Test we can't assign to a non-existent variable."
  (signals (rtl:unknown-variable)
    (rtl:typecheck '(let ((a 10))
		     (setq b 12))
		   emptyenv)))


(test test-assignment-constant
  "Test we can't assign to a constant variable."
  (signals (rtl:not-synthesisable)
    (rtl:typecheck '(let ((a 10 :as :constant))
		     (setq a 12))
		   emptyenv)))


(test test-synthesise-setq
  "Test we can synthesise assignments."
  ;; as statements
  (is (rtl:synthesise '(setq a 5)
		      :inblock))
  (is (rtl:synthesise '(setq a 5 :sync t)
		      :inblock)))


(test test-typecheck-setq-generalised-place
  "Test we catch the common mistake of using SETQ when we mean SETF."
  (signals (rtl:not-synthesisable)
    (rtl:typecheck '(let ((a 0 :width 4))
		     (setq (bit a 0) 1))
		   emptyenv)))


;; ---------- Generalised places (SETF) ----------

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


(test test-setf-variable
  "Test we can setf to a variable."
  (is (rtl:typecheck '(let ((a 12))
		       (setf a 14))
		     emptyenv)))


(test test-setf-variable-bits
  "Test we can setf to bits in a variable."
  (is (subtypep (rtl:typecheck '(let ((a 12))
				 (setf (rtl::bits a 1 :end 0) 2))
			       emptyenv)
		'(rtl::fixed-width-unsigned 12)))

  ;; fails because default width of a is too small
  (signals (rtl:width-mismatch)
    (rtl:typecheck '(let ((a 12))
		     (setf (rtl::bits a 6 :end 0) 0))
		   emptyenv))

  ;; fixed with an explicit width
  (is (subtypep (rtl:typecheck '(let ((a 12 :width 8))
				 (setf (rtl::bits a 6 :end 0) 0))
			       emptyenv)
		'(rtl::fixed-width-unsigned 12))))
