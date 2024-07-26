;; Base class for fully-software-emulated hardware components
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

;; ---------- Components ----------

(defclass component ()
  ((enable
    :documentation "The component-enable pin."
    :initarg :enable
    :pins 1
    :role :control))
  (:metaclass metacomponent)
  (:documentation "A component in an architecture.

Components encapsulate functions and offer a pin-based interface."))


(defgeneric component-enabled-p (c)
  (:documentation "Test whether the component is enabled."))


(defmethod component-enabled-p ((c component))
  (equal (pin-state (slot-value c 'enable)) 1))


(defgeneric component-pin-changed (c)
  (:documentation "A callback called whenever a C's pin change level.

This only applies to pins that are :reading and whose state is changed
by some other component. The default does nothing: a combinatorial
component would define its pin logic here to re-compute it when
its inputs changed."))


(defmethod component-pin-changed ((c component)))


(defgeneric component-pin-triggered (c p v)
  (:documentation "A callback called whenever a trigger pin on W changes value.

P is the pin triggered and V its new value. This method is only called
on :trigger pins. The default does nothing: a sequential component
would override or advise this method to perform its triggering action.
Specialise V to the direction of edge of interest."))


(defmethod component-pin-triggered ((c component) p v))


;; ---------- Mixins for common components ----------

(defclass clocked ()
  ((clock
    :documentation "The component's clock pin."
    :initarg :clock
    :pins 1
    :role :trigger))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that has a clock line.

Clocked components do most of their active work when the clock
transitions, although they can change state at other times too."))


(defclass readwrite ()
  ((write-enable
    :documentation "The component's write-enable pin."
    :initarg :write-enable
    :pins 1
    :role :control))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that has a write-enable line..

This provides a common control line for determining whether a component
reads data from a bus (when write-enable is high) or makes data available
on the bus. 'Write' should be seen from the perspective of ourside the
component.

This only works for components with a single decision on read or write.
More complicated components might need several such control lines."))


(defun component-write-enabled-p (c)
  "Test if C is write-enabled.

Write-enabled means that the write enable pin is high, and that the
component is writeable from the data bus at the next rising clock edge."
  (equal (pin-state (slot-value c 'write-enable)) 1))


(defun component-read-enabled-p (c)
  "Test if C is read-enabled.

Read-enabled means that the write enable pin is low, and the value of
the component is available to be read from the data bus."
  (equal (pin-state (slot-value c 'write-enable)) 0))
