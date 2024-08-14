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


;; ---------- Wires and pins ----------

(define-condition conflicting-asserted-values ()
  ((wire
    :documentation "The wire on which the conflict occurs."
    :initarg :wire
    :reader conflicting-asserted-values-wire))
  (:report (lambda (c str)
	     (format str "Components are asserting conflicting values on wire ~a"
		     (conflicting-asserted-values-wire c))))
  (:documentation "Condition signalled when two conflicting values are asserted on a wire.

Values conflict when more than one component attached to a wire
asserts a value on it, i.e., is not tri-stated."))


(define-condition reading-floating-value ()
  ((wire
    :documentation "The wire with the floating value."
    :initarg :wire
    :reader reading-floating-value-wire)
   (pin
    :documentation "The pin being read."
    :initarg :pin
    :reader reading-floating-value-pin))
  (:report (lambda (c str)
	     (let ((p (reading-floating-value-pin c)))
	       (format str "Pin ~a is reading a floating value from ~a"
		       p (pin-wire p)))))
  (:documentation "Condition signalled when a pin reads a floating value.

This is almost certainly an error, as it suggests that the component
expects a logic value but isn't getting one, presumably because no
other component is asserting a value on the wire."))


(define-condition reading-non-reading-pin ()
  ((pin
    :documentation "The pin being read."
    :initarg :pin
    :reader reading-non-reading-pin-pin))
  (:report (lambda (c str)
	     (format str "Pin ~a is not configured for reading."
		     (reading-non-reading-pin-pin c))))
  (:documentation "Condition signalled when reading a non-reading pin.

This usually happens when a pin is tristated by accident. It is
almost certainly an error in wiring or configuraton,"))


(define-condition mismatched-wires ()
  ((component
    :documentation "The component the wires are being attached to."
    :initarg :component
    :reader mismatched-wires-component)
   (slot
    :documentation "The slot on the component being wired-up."
    :initarg :slot
    :reader mismatched-wires-slot)
   (expected
    :documentation "The number of wires expected"
    :initarg :expected
    :reader mismatched-wires-expected)
   (got
    :documentation "The number of wires received."
    :initarg :received
    :reader mismatched-wires-received))
  (:report (lambda (c str)
	     (format str "Expected ~s wires for slot ~s on component ~s, got ~s"
		     (mismatched-wires-expected c)
		     (mismatched-wires-slot c)
		     (mismatched-wires-component c)
		     (mismatched-wires-received c))))
  (:documentation "Condition signalled when an unexpected number of wires is received.

This is probably a mismatch between the width of a bus and the width
of the pin interface slot it is being attached to."))


;; ---------- Components ----------

(define-condition non-component-type ()
  ((type
    :documentation "The type encountered."
    :initarg :type
    :reader non-component-type))
  (:report (lambda (c str)
	     (format str "The type ~s is not a component type" (non-component-type c))))
  (:documentation "Condition signalled when a non-component type is encountered.

This typically happens when trying to build a micro-instruction when there
is a non-component slot."))


(define-condition non-pin-interface-slot ()
  ((component
    :documentation "The component."
    :initarg :component
    :reader non-pin-interface-component)
   (slot-name
    :documentation "The slot name."
    :initarg :slot
    :reader non-pin-interface-slot))
  (:report (lambda (c str)
	     (format str "The slot ~s is not in component ~s's pin interface"
		     (non-pin-interface-slot c)
		     (non-pin-interface-component c))))
  (:documentation "Condition signalled when a slot is not in the pin interface.

The pin interface consists of slots with a `:pins' value, representing
hardware interfaces. This condition almost certainly comes from the
use of the wrong slot name, or a missing declaration in the component's
class."))


;; ---------- Standard components ----------

(define-condition unrecognised-alu-operation ()
  ((opcode
    :documentation "The opcode for the operation requested."
    :initarg :opcode
    :reader unrecognised-alu-operation-opcode)
   (a
    :documentation "The a-side operand."
    :initarg :pin
    :reader unrecognised-alu-operation-a)
   (b
    :documentation "The b-side operand."
    :initarg :pin
    :reader unrecognised-alu-operation-b))
  (:report (lambda (c str)
	     (format str "ALU operation can't be performed (opcode ~a, a=~a, b=~a)"
		     (unrecognised-alu-operation-opcode c)
		     (unrecognised-alu-operation-a c)
		     (unrecognised-alu-operation-b c))))
  (:documentation "Condition signalled when an ALU cannot perform an operation.

This is likely to be caused by an unrecognised opcode being presented to
the ALU's operation-select bus. It might also be an issue with the operands."))


;; ---------- Wiring ----------

(define-condition incompatible-pin-widths ()
  ((coonnector
    :documentation "The connector."
    :initarg :connector
    :reader incompatible-pin-widths-connector)
   (bus
    :documentation "The bus."
    :initarg :bus
    :reader incompatible-pin-widths-bus))
  (:report (lambda (c str)
	     (format str "Connector width ~a doesn't match bus width ~s"
		     (connector-width (incompatible-pin-widths-connector c))
		     (bus-width (incompatible-pin-widths-bus c)))))
  (:documentation "Condition signalled when wiring incompatible connectors and buses.

The widths of buses and connectors need to be the same."))


(define-condition incompatible-pin-slot-widths ()
  ((slots-names
    :documentation "The slot names whose widths are being determined."
    :initarg :slot-names
    :reader incompatible-pin-slot-widths-slot-names)
   (unknown
    :documentation "Flag as to whether the width is unknown."
    :initarg :unknown
    :initform nil
    :reader incompatible-pin-slot-widths-unknown)
   (first
    :documentation "The first known width (if any)."
    :initarg :first
    :reader incompatible-pin-slot-widths-first)
   (second
    :documentation "The second known width (if any)."
    :initarg :second
    :reader incompatible-pin-slot-widths-second))
  (:report (lambda (c str)
	     (if (slot-value c 'unknown)
		 ;; no width could be determined
		 (format str "The widths of slots ~a cdan't be determined"
			 (incompatible-pin-slot-widths-slot-names c))

		 ;; two incompatible widths were determined
		 (format str "The pin slot widths ~a and ~a are incompatible when wiring ~a"
			 (incompatible-pin-slot-widths-first c)
			 (incompatible-pin-slot-widths-second c)
			 (incompatible-pin-slot-widths-slot-names c)))))
  (:documentation "Condition signalled when two pin slots have incompatible widths.

This happens when two slots are wired together but have different
widths, or when no wodth can be determined at all. It must be possible
to find exactly one width for all the pin slots."))
