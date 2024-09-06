;; Software-emulated arithmetic logic unit
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

;; ---------- ALU ----------

(defclass alu (component enabled)
  ((width
    :documentation "The width of the ALU."
    :initarg :width
    :initform 8
    :reader width)
   (a-bus
    :documentation "The ALU's A-side data bus."
    :initarg :a-bus
    :pins width
    :role :reading
    :reader alu-a-bus)
   (b-bus
    :documentation "The ALU's B-side data bus."
    :initarg :b-bus
    :pins width
    :role :reading
    :reader alu-b-bus)
   (c-bus
    :documentation "The ALU's C-side (results) bus."
    :initarg :c-bus
    :pins width
    :role :io
    :reader alu-c-bus)
   (op-bus
    :documentation "The ALU's operation-select bus."
    :initarg :op-bus
    :pins 3
    :role :control
    :reader alu-op-bus))
  (:metaclass metacomponent)
  (:documentation "An integer arithmetic logic unit.

An ALU is connected to four buses: the A- and B-side buses, which provide
operands; the C-side bus that generates the output; and the operation-select
bus that determines the operation the ALU performs. The available operations
are:

- #2r000 a        (pass-through)
- #2r001 a AND b
- #2r010 a OR b
- #2r011 NOT a
- #2r100 a + b
- #2r101 a - b
- #2r110 a + 1    (increment)
- #2r111 a - 1    (decrement)

The ALU is an entirely combinatorial component. The value on the C-side
represents the results of computing over the A- and B-sides, and is
updated whenever those values change and the component is enabled. There
is no clock interface."))


;; TODO Add carry bit

(defmethod on-pin-changed ((a alu))
  ;; perform the encoded operation
  (let* ((op (pins-value (alu-op-bus a)))
	 (v (switch (op)
	      ;; pass-through
	      (#2r000 (pins-value (alu-a-bus a)))

	      ;; logical and
	      (#2r001 (logand (pins-value (alu-a-bus a))
			      (pins-value (alu-b-bus a))))

	      ;; logical or
	      (#2r010 (logior (pins-value (alu-a-bus a))
			      (pins-value (alu-b-bus a))))

	      ;; logical not
	      (#2r011 (lognot (pins-value (alu-a-bus a))))

	      ;; addition
	      (#2r100 (+ (pins-value (alu-a-bus a))
			 (pins-value (alu-b-bus a))))

	      ;; subtraction
	      (#2r101 (- (pins-value (alu-a-bus a))
			 (pins-value (alu-b-bus a))))

	      ;; increment (a-side only)
	      (#2r110 (1+ (pins-value (alu-a-bus a))))

	      ;; decrement (a-side only)
	      (#2r111 (1- (pins-value (alu-a-bus a))))

	      (t (error 'unrecognised-alu-operation
			:opcode op
			:a (pins-value (alu-a-bus a))
			:b (pins-value (alu-b-bus a)))))))

    ;; update the output pins
    (setf (pins-value (alu-c-bus a)) v)))


(defmethod on-enable ((a alu))
  (setf (pin-states (alu-a-bus a)) :reading)
  (setf (pin-states (alu-b-bus a)) :reading)
  (setf (pin-states (alu-op-bus a)) :reading))


;; TODO This retains the output on the c-bus -- should it be tristated instead?

(defmethod on-disable ((a alu))
  (setf (pin-states (alu-a-bus a)) :tristate)
  (setf (pin-states (alu-b-bus a)) :tristate)
  (setf (pin-states (alu-op-bus a)) :tristate))
