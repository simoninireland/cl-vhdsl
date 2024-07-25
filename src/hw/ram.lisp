;; Software-emulated RAM
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

(defclass ram (component clocked)
  ((address-width
    :documentation "The width of the address bus, in bits."
    :initarg :address-bus-width
    :initform 16)
   (data-width
    :documentation "The width of the data bus, in bits."
    :initarg :data-bus-width
    :initform 8)
   (address-bus
    :documentation "The address bus."
    :initarg :address-bus
    :pins address-bus-width
    :role :control
    :reader ram-address-bus)
   (data-bus
    :documentation "The data bus."
    :initarg :data-bus
    :pins data-bus-width
    :role :io
    :reader ram-data-bus)
   (write-enable
    :documentation "Write-enable."
    :initarg :write-enable
    :pins 1
    :role :control
    :reader ram-write-enable)
   (elements
    :documentation "The memory elements."
    :reader ram-elements))
  (:metaclass metacomponent)
  (:documentation "An emulated random-access memory (RAM).

The RAM is parameterised by its address and data bus widths in bits.
It needs address and data lines, a clock, an enable, and a write enable."))


(defmethod make-instance ((mem ram) &rest initargs)
  (declare (ignore initargs))

  (setf (slot-value mem 'elements)
	(make-array (list (ram-size mem))
		    :initial-element 0)))


(defun ram-size (mem)
  "Return the size of MEM in elements.

The size of elements is determined by the width of the data bus."
  (floor (expt 2 (slot-value mem 'address-width))))


(defun memory-write-enabled-p (mem)
  "Test if MEM is write-enabled.

Write-enabled means that the write enable pin is high, and that the
memory is writeable from its data bus at the next rising clock edge."
  (equal (pin-state (ram-write-enable r)) 1))


(defun memory-read-enabled-p (mem)
  "Test if MEM is read-enabled.

Read-enabled means that the write enable pin is low, and the value of
the memory  is available to be read from the data bus."
  (equal (pin-state (ram-write-enable r)) 0))


(defmethod component-pin-triggered ((mem ram) p (v (eql 1)))
  (declare (ignore p)) ;; we only have one trigger pin

  (when (and (component-enabled-p mem)
	     (ram-write-enabled-p mem))
    (let ((v (pins-to-value (ram-data-bus mem)))
	  (addr (pins-to-value (ram-address-bus mem))))
      (setf (aref (ram-elements addr) v)))))


(defmethod component-pin-changed ((mam ram))
  (if (component-enabled-p mem)
      (if (memory-write-enabled-p mem)
	  ;; set all the data bus pins to :reading
	  (setf (pins-states (memory--data-bus mem)) :reading)

	  ;; put the value of the memory addressed on the
	  ;; address bus onto the data bus
	  (let ((addr (pins-to-value (ram-address-bus mem))))
	    (pins-from-value (ram-data-bus mem)
			     (aref (ram=-elements mem) addr))))

      ;; tri-state the data bus
      (setf (pins-states (memory-data-bus mem)) :tristate)))
