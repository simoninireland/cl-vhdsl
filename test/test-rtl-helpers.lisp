;; Tests of helper macros
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


;; ---------- with-rtl-errors-not-synthesisable ----------

(test test-helper-errors
  "Test we trap non-RTL errors."

  ;; traps and translates the divide-by-zero condition
  (signals (rtl::not-synthesisable)
    (rtl::with-rtl-errors-not-synthesisable
      (let (a)
	(setq a (/ 12 0)))))

  ;; passes the unknown variable condition
  (signals (rtl::unknown-variable)
    (rtl::with-rtl-errors-not-synthesisable
      (rtl:typecheck '(let (a)
		       (setq b (+ 12 2)))))))


;; ---------- with-unknown-form ----------

(test test-unknown-form
  "Test we can trap unknown forms."
  (signals (rtl::unknown-form)
    (rtl:typecheck '(let (a)
		     (blig 34)))))


;; ---------- with-continue-on-error ----------

(test test-continue-on-error
  "Test we can continue with compilation after an error."
  (let ((errors 0)
	(result '()))

    (handler-bind
	((error (lambda (condition)
		  (declare (ignore condition))
		  ;; record the error
		  (incf errors)

		  ;; jump back in for the next element
		  (invoke-restart 'continue))))

      (dolist (i (list 1 2 3 4))
	(rtl::with-continue-on-error
	    (if (= i 2)
		;; 2 is bad...
		(error "Bad thing happened")

		;; ...anyting else is fine
		(appendf result (list i)))

	  ;; no recovery action needed
	  nil)))

    (is (= errors 1))
    (is (equal result '(1 3 4)))))


(test test-continue-typecheck-on-error
  "Test that type-checking continues."
  (let ((errors 0))

    (handler-bind
	((rtl::unknown-variable (lambda (condition)
				  (declare (ignore condition))
				  ;; record the error
				  (incf errors)

				  ;; jump back in for the next element
				  (invoke-restart 'continue))))

      (let ((ty (rtl:typecheck '(let ((a 45)
				      (b 0)
				      (c 2))
				 (setq d 12)
				 8))))

	;; the type should be that of 8, where we re-started
	(is (subtypep ty '(unsigned-byte 4)))))

    (is (= errors 1))))
