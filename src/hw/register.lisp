;; Software-emulated registers
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

;; ---------- Registers ----------

(defclass register (component clocked)
  ((width
    :documentation "The width of the register."
    :initarg :width
    :initform 8
    :reader register-width)
   (value
    :documentation "The register's current value."
    :initarg :value
    :initform 0
    :accessor register-value)
   (data-bus
    :documentation "The data bus the register is connected to."
    :initarg :bus
    :reader register-data-bus)
   (write-enable
    :documentation "The write-enable."
    :initarg :write-enable
    :reader register-write-enable))
  (:documentation "A register.

Registers must be connected to a bus and three wires. The data bus must
have at least as many wires as the register. The three other wires are
for clock, register enable, and write enable.

When write is enabled then at the next rising clock edge then the
value of the register will be written from the bus. When write is
disabled, read is enabled and the value of the register will be made
available on the bus. Write-enable should be see from the perspective
of a client outside the register."))


(defmethod initialize-instance :after ((r register) &rest initargs)
  (declare (ignore initargs))

  ;; attach a pin to the write-enable wire and set it for reading
  (setf (slot-value r 'write-enable)
	(pin-for-wire (slot-value r 'write-enable)
		      :component r
		      :state :reading))

  ;; attach pins to the data bus wires
  (setf (slot-value r 'data-bus)
	(pins-for-wires (bus-wires (slot-value r 'data-bus))
			:component r)))


(defmethod component-pin-triggered ((r register) p (v (eql 1)))
  (declare (ignore p))            ;; we only have one trigger pin
  (when (and (component-enabled-p r)
	     (register-write-enabled-p r))
    (register-value-from-data-bus r)))


(defmethod component-pin-changed ((r register))
  (if (component-enabled-p r)
      (if (register-write-enabled-p r)
	  ;; set all data bus pins to :reading
	  (setf (pins-states (register-data-bus r)) :reading)

	  ;; put the value of the register onto the data bus pins
	  (register-value-to-data-bus r))

      ;; tri-state the data bus
      (setf (pins-states (register-data-bus r)) :tristate)))


(defun register-value-to-data-bus (r)
  "Move the value of R to the pins of the data bus."
  (pins-from-value (register-data-bus r) (register-value r)))


(defun register-value-from-data-bus (r)
  "Make the value on the pins of the data bus the value of R.

This implies that the pins are all :reading."
  (setf (slot-value r 'value) (pins-to-value (register-data-bus r))))


(defun register-write-enabled-p (r)
  "Test if R is write-enabled.

Write-enabled means that the write enable pin is high, and that the
register is writeable from the data bus at the next rising clock edge."
  (equal (pin-state (register-write-enable r)) 1))


(defun register-read-enabled-p (r)
  "Test if R is read-enabled.

Read-enabled means that the write enable pin is low, and the value of
the register is available to be read from the data bus."
  (equal (pin-state (register-write-enable r)) 0))
