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
	     (let ((pin (slot-value c 'pin)))
	       (format str "Pin ~a is reading a floating value from ~a"
		       pin (pin-wire pin)))))
  (:documentation "Condition signalled when a pin reads a floating value.

This is almost certainly an error, as it suggests that the component
expects a logic value but isn't getting one, presumably because no
other component is asserting a value on the wire."))


(define-condition reading-non-reading-pin ()
  ((pin
    :documentation "The pin being read."
    :initarg :pin))
  (:report (lambda (c str)
	     (format str "Pin ~a is not configured for reading."
		     (slot-value c 'pin))))
  (:documentation "Condition signalled when reading a non-reading pin.

This usually happens when a pin is tristated by accident. It is
almost certainly an error in wiring or configuraton,"))


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


(define-condition non-component-type ()
  ((type
    :documentation "The type encountered."
    :initarg :type))
  (:report (lambda (c str)
	     (format str "The type ~s is not a component type" (slot-value c 'type))))
  (:documentation "Condition signalled when a non-component type is encountered.

This typically happens when trying to build a micro-instruction when there
is a non-component slot."))


;; ---------- Slot names ----------

(define-condition unknown-slot-name ()
  ((slot-name
    :documentation "The slot name."
    :initarg :slot-name))
  (:report (lambda (c str)
	     (format str "The slot name ~s doesn't exist"
		     (slot-value c 'slot-name))))
  (:documentation "Condition signalled when a slot name is missing.

The slot name may be simple, and looked up directly on a
class, or qualified, which is looked up on a slot on the
component slot."))


;; ---------- Wiring ----------

(define-condition incompatible-pin-widths ()
  ((coonnector
    :documentation "The connector."
    :initarg :connector)
   (bus
    :documentation "The bus."
    :initarg :bus))
  (:report (lambda (c str)
	     (format str "Connector width ~a doesn't match bus width ~s"
		     (connector-width (slot-value c 'connector))
		     (bus-width (slot-value c 'bus)))))
  (:documentation "Condition signalled when wiring incompatible connectors and buses.

The widths of buses and connectors need to be the same."))


(define-condition incompatible-pin-slot-widths ()
  ((slots-names
    :documentation "The slot names whose widths are being determined."
    :initarg :slot-names)
   (unknown
    :documentation "Flag as to whether the width is unknown."
    :initarg :unknown
    :initform nil)
   (first
    :documentation "The first known width (if any)."
    :initarg :first)
   (second
    :documentation "The second known width (if any)."
    :initarg :second))
  (:report (lambda (c str)
	     (if (slot-value c 'unknown)
		 ;; no width could be determined
		 (format str "The widths of slots ~a cdan't be determined"
			 (slot-value c 'slot-names))

		 ;; two incompatible widths were determined
		 (format str "The pin slot widths ~a and ~a are incompatible when wiring ~a"
			 (slot-value c 'first)
			 (slot-value c 'second)
			 (slot-value c 'slot-names)))))
  (:documentation "Condition signalled when two pin slots have incompatible widths.

This happens when two slots are wired together but have different
widths, or when no wodth can be determined at all. It must be possible
to find exactly one width for all the pin slots."))
