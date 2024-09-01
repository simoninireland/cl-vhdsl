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

(defclass register (component enabled clocked readwrite)
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
    :pins width
    :role :io
    :reader register-data-bus))
  (:metaclass metacomponent)
  (:documentation "A register.

Registers must be connected to a bus and three wires. The data bus must
have at least as many wires as the register. The three other wires are
for clock, register enable, and write enable.

When write is enabled then at the next rising clock edge then the
value of the register will be written from the bus. When write is
disabled, read is enabled and the value of the register will be made
available on the bus. Write-enable should be see from the perspective
of a client outside the register."))


(defmethod pin-triggered ((r register) p (v (eql 1)))
  (declare (ignore p))            ;; we only have one trigger pin

  (when (and (enabled-p r)
	     (write-enabled-p r))
    (register-value-from-data-bus r)))


(defmethod pin-changed ((r register))
  (if (enabled-p r)
      (if (write-enabled-p r)
	  ;; set all data bus pins to :reading
	  (setf (pin-states (register-data-bus r)) :reading)

	  ;; put the value of the register onto the data bus pins
	  (register-value-to-data-bus r))

      ;; tri-state the data bus
      (setf (pin-states (register-data-bus r)) :tristate)))


(defun register-value-to-data-bus (r)
  "Move the value of R to the pins of the data bus."
  (setf (pins-value (register-data-bus r)) (register-value r)))


(defun register-value-from-data-bus (r)
  "Make the value on the pins of the data bus the value of R.

This implies that the pins are all :reading."
  (setf (slot-value r 'value) (pins-value (register-data-bus r))))
