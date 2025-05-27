;; Tests of simple assignments and generalised places
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/vl)


;; ---------- Simple assignment (SETQ) ----------

(test test-setq
  "Test we can typecheck the SETQ form."
  (is (subtypep (vl:typecheck '(let ((a 13))
				 (setq a 9)))
		'(unsigned-byte 8)))

  (signals (vl:not-synthesisable)
    (vl:typecheck '(let ((a 12 :as :constant))
		     (setq a 9)))))


(test test-assignment-same-width
  "Test we can assign."
  (is (subtypep (vl:typecheck '(let ((a 10))
				 (setq a 12)))
		'(unsigned-byte 5))))


(test test-assignment-same-width-sync
  "Test we can assign synchronously (same types)."
  (is (subtypep (vl:typecheck '(let ((a 10))
				 (setq a 12 :sync t)))
		'(unsigned-byte 5))))


(test test-assignment-too-wide
  "Test we catch assigning a value that's too wide for its explicit type."
  (signals (vl:type-mismatch)
    (vl:typecheck '(let ((a 10 :type (unsigned-byte 5)))
		     (setq a 120)))))


(test test-assignment-too-wide-widenable
  "Test we can assign a value to a variable that can be widened."
  (is (subtypep (vl:typecheck '(let ((a 10))
				 (setq a 120)))
		'(unsigned-byte 7))))


(test test-assignment-too-wide-updated
  "Test we don't update the type when we have an explicit one already."
  (let ((p (copy-tree '(let ((a 10 :type (unsigned-byte 5) :as :register))
			(setq a 120)))))
    (subtypep (vl:typecheck p)
	      '(unsigned-byte 7))

    ;; code tree not updated
    (is (equal p
	       '(let ((a 10 :type (unsigned-byte 5) :as :register))
		  (setq a 120))))))


(test test-assignment-too-wide-widenable-updated
  "Test we update the code to match inferred types."
  (let ((p (copy-tree'(let ((a 10))
		       (setq a 120)))))
    (is (subtypep (vl:typecheck p)
		  '(unsigned-byte 7)))

    ;; code tree updated to reflect inferred type
    (is (equal p
	       '(let ((a 10 :type (unsigned-byte 7) :as :register))
		 (setq a 120))))))


(test test-assignment-out-of-scope
  "Test we can't assign to a non-existent variable."
  (signals (vl:unknown-variable)
    (vl:typecheck '(let ((a 10))
		     (setq b 12)))))


(test test-assignment-constant
  "Test we can't assign to a constant variable."
  (signals (vl:not-synthesisable)
    (vl:typecheck '(let ((a 10 :as :constant))
		     (setq a 12)))))


(test test-synthesise-setq
  "Test we can synthesise assignments."
  ;; as statements
  (is (vl:synthesise '(setq a 5)))
  (is (vl:synthesise '(setq a 5 :sync t))))


(test test-typecheck-setq-generalised-place
  "Test we catch the common mistake of using SETQ when we mean SETF."
  (signals (vl:not-synthesisable)
    (vl:typecheck '(let ((a 0 :type (unsigned-byte 4)))
		     (setq (bit a 0) 1)))))


;; ---------- Generalised places (SETF) ----------

(test test-setf-as-setq
  "Test we can convert a simple SETF into a SETQ."
  (is (subtypep (vl:typecheck '(let ((a 12))
				(setf a 9)))
		'(unsigned-byte 4))))
