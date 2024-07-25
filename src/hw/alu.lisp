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

(defclass alu (component)
  ((width
    :documentation "The width of the ALU."
    :initarg :width
    :initform 8
    :reader alu-width)
   (a-bus
    :documentation "The ALU's A-side data bus."
    :initarg :a-bus
    :pins width
    :role :io
    :reader alu-a-bus)
   (b-bus
    :documentation "The ALU's B-side data bus."
    :initarg :b-bus
    :pins width
    :role :io
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

- #2r000 a + b
- #2r001 a - b
- #2r010 a + 1
- #2r011 a- 1

The ALU is an entirely combinatorial component. The value on the C-side
represents the results of computing over the A- and B-sides."))


;; TBD add carry

(defmethod component-pin-changed ((a alu))
  (if (component-enabled-p a)
      ;; perform the encoded operation
      (let* ((op (pins-to-value (alu-op-bus a)))
	     (v (switch (op)
		  ;; addition
		  (#2r000 (+ (pins-to-value (alu-a-bus a))
			     (pins-to-value (alu-b-bus a))))

		  ;; subtraction
		  (#2r001 (- (pins-to-value (alu-a-bus a))
			     (pins-to-value (alu-b-bus a))))

		  ;; increment (a-side only)
		  (#2r010 (1+ (pins-to-value (alu-a-bus a))))

		  ;; decrement (a-side only)
		  (#2r011 (1- (pins-to-value (alu-a-bus a))))

		  (t (error (make-instance 'unrecognised-alu-operation
					   :opcode op
					   :a (pins-to-value (alu-a-bus a))
					   :b (pins-to-value (alu-b-bus a))))))))

	(setf (pins-states (alu-c-bus a)) v))

      ;; tri-state the c-bus
      (setf (pins-states (alu-c-bus a)) :tristate)))
