;; Tests of ISA definitions
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
(in-suite cl-vhdsl/def)


;; ---------- Synthesisable types (should be moved to RTL) ----------

(test test-synthesisable-types
  "Test we can determine synthesisable types."
  ;; fixed-width types with known width
  (is (def::synthesisable-type-p '(unsigned-byte 8)))
  (is (def::synthesisable-type-p '(signed-byte 8)))

  ;; some derived types
  (is (def::synthesisable-type-p 'bit))

  ;; unknown widths
  (is (not (def::synthesisable-type-p 'unsigned-byte)))

  ;; arrays of synthesisable types
  (is (def::synthesisable-type-p '(array (unsigned-byte 32))))
  (is (def::synthesisable-type-p '(array (unsigned-byte 32) (32))))
  (is (def::synthesisable-type-p '(array bit)))

  ;; arrays of unsynthesisable types
  (is (not (def::synthesisable-type-p '(array unsigned-byte))))
  (is (not (def::synthesisable-type-p '(array *)))))


;; ---------- Forming bitfields from slots ----------

(test test-def-slot-widths-legal
  "Test we can extract slot and instruction widths."
  (defclass test-def-slot-widths-legal ()
    ((one
      :type (unsigned-byte 8))
     (two
      :type (signed-byte 4))))

  (let ((ins (make-instance 'test-def-slot-widths-legal)))
    (is (= (def::instruction-width ins) 12))))


(test test-def-slot-widths-illegal
  "Test we can detect non-typed slots."
  (defclass test-def-slot-widths-illegal ()
    ((one
      :type (unsigned-byte 8))
     (two)))

  (let ((ins (make-instance 'test-def-slot-widths-illegal)))
    (signals (def:slot-type-mismatch)
      (def::instruction-width ins))))


(test test-def-slot-bitfields
  "Test we can form bitfields from slots in a desired order."
  (defclass test-def-slot-bitfields ()
    ((one
      :initarg :one
      :type (unsigned-byte 8))
     (two
      :initarg :two
      :type (signed-byte 4))))

  (let ((ins (make-instance 'test-def-slot-bitfields
			    :one #2r10001000
			    :two #2r1010)))
    (is (= (def::form-bitfield-from-slots ins '(one))
	   #2r10001000))
    (is (= (def::form-bitfield-from-slots ins '(one two))
	   #2r100010001010))
    (is (= (def::form-bitfield-from-slots ins '(two one))
	   #2r101010001000))))


(test test-def-slot-short-bitfields
  "Test we can create a bitfield from a 'short' slot."
  (defclass test-def-slot-bitfields ()
    ((one
      :initarg :one
      :type (unsigned-byte 8))
     (two
      :initarg :two
      :type (signed-byte 4))))
  )
