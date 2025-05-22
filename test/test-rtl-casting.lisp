;; Tests of type assertions and casting
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


;; ---------- Type assertions  ----------

(test test-typecheck-the
  "Test we can typecheck THE."
  (is (subtypep (rtl:typecheck '(the (unsigned-byte 8) 12))
		'(unsigned-byte 8)))

  (signals (rtl:type-mismatch)
    (rtl:typecheck '(the (unsigned-byte 8) 1230))))


(test test-synthesise-the
  "Test we can synthesise a value whose type has been asserted explicitly."
  (is (rtl:synthesise '(the (unsigned-byte 8) 12) :inexpression)))



;; ---------- Type coercions ----------

(test test-typecheck-coerce
  "Test we can typecheck coercions."
  (is (subtypep (rtl:typecheck '(coerce 12 (unsigned-byte 8)))
		'(unsigned-byte 8)))

  ;; type is the type coerced to, not of the value (unlike for the)
  (is (not (subtypep (rtl:typecheck '(coerce 12 (unsigned-byte 8)))
		     '(unsigned-byte 4))))

  (is (subtypep (rtl:typecheck '(coerce 12 (signed-byte 5)))
		'(signed-byte 5)))

  ;; can't coerce anything not fixed-width
  (signals (rtl:coercion-mismatch)
    (rtl:typecheck '(coerce (make-array '(10) :element-type (unsigned-byte 8))
		     (signed-byte 58))))

  ;; can coerce elements though
  (is (subtypep (rtl:typecheck '(let ((a (make-array '(10) :element-type (unsigned-byte 8))))
				 (coerce (aref a 3) (signed-byte 16))))
		'(signed-byte 16))))


(test test-synthesise-coerce-equal-width-same-sign
  "Test we can coerce equal-width and -signedness numbers."
  (rtl::with-new-frame
    (rtl::declare-variable 'a '((:type (unsigned-byte 8))))
    (is (rtl:synthesise '(coerce a (unsigned-byte 8)) :inexpression))

    (rtl::declare-variable 'b '((:type (signed-byte 8))))
    (is (rtl:synthesise '(coerce b (signed-byte 8)) :inexpression))

    (rtl::declare-variable 'c '((:type (unsigned-byte 8))))
    (is (rtl:synthesise '(coerce c (signed-byte 8)) :inexpression))

    (rtl::declare-variable 'd '((:type (signed-byte 8))))
    (is (rtl:synthesise '(coerce d (unsigned-byte 8)) :inexpression))))


(test test-synthesise-coerce-narrower-same-sign
  "Test we can coerce narrower numbers."
  (rtl::with-new-frame
    (rtl::declare-variable 'a '((:type (unsigned-byte 16))))
    (is (rtl:synthesise '(coerce a (unsigned-byte 8)) :inexpression))

    (rtl::declare-variable 'b '((:type (signed-byte 16))))
    (is (rtl:synthesise '(coerce b (signed-byte 8)) :inexpression))

    (rtl::declare-variable 'c '((:type (unsigned-byte 16))))
    (is (rtl:synthesise '(coerce c (signed-byte 8)) :inexpression))

    (rtl::declare-variable 'd '((:type (signed-byte 16))))
    (is (rtl:synthesise '(coerce d (unsigned-byte 8)) :inexpression))))


(test test-synthesise-coerce-wider-same-sign
  "Test we can coerce wider numbers."
  (rtl::with-new-frame
    (rtl::declare-variable 'a '((:type (unsigned-byte 8))))
    (is (rtl:synthesise '(coerce a (unsigned-byte 16)) :inexpression))

    (rtl::declare-variable 'b '((:type (signed-byte 8))))
    (is (rtl:synthesise '(coerce b (signed-byte 16)) :inexpression))

    (rtl::declare-variable 'c '((:type (unsigned-byte 8))))
    (is (rtl:synthesise '(coerce c (signed-byte 16)) :inexpression))

    (rtl::declare-variable 'd '((:type (signed-byte 8))))
    (is (rtl:synthesise '(coerce d (unsigned-byte 16)) :inexpression))))


(test test-synthesise-coerce-real
  "Test coercions against a real expression."
  (let ((p (copy-tree '(let ((instr 0 :width 32)
			     a)
			(let ((bs (rtl:bref instr 31 :end 20)))
			  (let ((Iimm (coerce bs
					      (signed-byte 32))))
			    (setq a Iimm)))))))
    (rtl:typecheck p)
    (is (rtl:synthesise p :inmodule))))
