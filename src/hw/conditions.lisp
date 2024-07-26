;; Conditions for emulated hardware components
;;
;; Copyright (C) 2024 Simon Dobson
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

(in-package :cl-vhdsl/hw)


(define-condition conflicting-asserted-values ()
  ((wire
    :documentation "The wire on which the conflict occurs."
    :initarg :wire))
  (:report (lambda (c str)
	     (format str "Components are asserting conflicting values on wire ~a"
		     (slot-value c 'wire))))
  (:documentation "Condition signalled when two conflicting values are asserted on a wire.

Values conflict when more than one component attached to a wire
asserts a value on it, i.e., is not tri-stated."))


(define-condition reading-floating-value ()
  ((wire
    :documentation "The wire with the floating value."
    :initarg :wire)
   (pin
    :documentation "The pin being read."
    :initarg :pin))
  (:report (lambda (c str)
	     (format str "Pin ~a is reading a floating value from ~a"
		     (slot-value c 'pin)
		     (slot-value c 'wire))))
  (:documentation "Condition signalled when a pin reads a floating value.

This is almost certainly an error, as it suggests that the component
expects a logic value but isn't getting one, presumably because no
other component is asserting a value on the wire."))


(define-condition unrecognised-alu-operation ()
  ((opcode
    :documentation "The opcode for the operation requested."
    :initarg :opcode)
   (a
    :documentation "The a-side operand."
    :initarg :pin)
   (b
    :documentation "The b-side operand."
    :initarg :pin))
  (:report (lambda (c str)
	     (format str "ALU operation can't be performed (opcode ~a, a=~a, b=~a)"
		     (slot-value c 'opcode)
		     (slot-value c 'a)
		     (slot-value c 'b))))
  (:documentation "Condition signalled when an ALU cannot perform an operation.

This is likely to be caused by an unrecognised opcode being presented to
the ALU's operation-select bus. It might also be an issue with the operands."))


(define-condition mismatched-wires ()
  ((component
    :documentation "Th component the wires are being attached to."
    :initarg :component)
   (slot
    :documentation "Tyhe slot on the component being wired-up."
    :initarg :slot)
   (expected
    :documentation "The number of wires expected"
    :initarg :expected)
   (got
    :documentation "The number of wires received."
    :initarg :received))
  (:report (lambda (c str)
	     (format str "Expected ~s wires for slot ~s on component ~s, got ~s"
		     (slot-value c 'expected)
		     (slot-value c 'slot)
		     (slot-value c 'component)
		     (slot-value c 'got))))
  (:documentation "Condition signalled when an unexpected number of wires is received.

This is probably a mismatch between the width of a bus and the width
of the pin interface slot it is being attached to."))
