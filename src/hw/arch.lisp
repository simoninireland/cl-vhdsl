;; Fully-software-emulated hardware components
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
  ((pins-table
    :documentation "A hash table mapping pin names to pins. Pins may have several names."
    :initform (make-hash-table)
    :reader component-pins))
  (:documentation "A component in an architecture.

Components encapsulate functions and offer a pin-based interface."))


(defmethod component-pin ((c component) n)
  (gethash (component-pins c) n))


(defmethod (setf component-pin) (p (c component) ns)
  (let ((pt (component-pins c)))
    (dolist (n ns)
      (if (gethash n pt)
	  (error "Duplicate pin name ~a on component ~a" n c)
	  (setf (gethash n pt) p)))))


;; ---------- Wires ----------

(defclass wire ()
  ((state
    :documentation "The state of the wire."
    :initform :floating
    :initarg :state
    :reader wire-state)
   (pins
    :documentation "List of pins attached to the wire."
    :initform '()
    :accessor wire-pins)
   (assertions-table
    :documentation "A mapping from asserted value to the pins asserting it."
    :initform (make-hash-table)
    :reader wire-pin-assertions))
  (:documentation "A wire.

Wires connect several components together. Each pin connected to
a wire can have one of several states:

- 0 or 1, the logical values
- :floating, where no clear value is asserted

Components assert the states of pins, and all the pins on a wire
integrate to deterine the wire's state."))


(defun wire-logic-value-p (v)
  "Test whether V is a logic value.

V can take a logic value (0 or 1) or some other value
indicating a non-asserting state."
  (or (equal v 0)
      (equal v 1)))


(defun wire-other-logic-value (v)
  "Return the other logic value to V.

0 and 1 exchange values; other (non-logical) values are left unchanged."
  (cond ((symbolp v)
	 v)
	((= v 0)
	 1)
	((= v 1)
	 0)
	(t
	 (error "Strange value ~a assigned to wire" v))))


(defun wire-conflicting-assertion-p (w nv)
  "Test whether asserting NV on W generates a state conflict.

States conflict when NV is different to the present state, is not
tri-stating, and asserts one logic value while another component is
asserting the other."
  (and (not (equal nv (wire-state w)))
       (not (equal nv :tristate))
       (wire-any-pin-asserting-p w (wire-other-logic-value nv))))


(defun wire-floating-p (w)
  "Test whether W is floating."
  (equal (slot-value w 'state) :floating))


(defun wire-known-pin-p (w p)
  "Test that P is a pin attached to W."
  (member p (wire-pins w)))


(defun wire-pins-asserting (w v)
  "Return the list of pins of W asserting V."
  (gethash v (wire-pin-assertions w)))


(defun wire-pin-asserting-p (w p v)
  "Test whether P on W is asserting V.

This checks for the inclusion of P in the `wire-pins-asserting'
list for V."
  (member p (wire-pins-asserting w v)))


(defun wire-any-pin-asserting-p (w v)
  "Test if there is any pin or W asserting V."
  (> (length (wire-pins-asserting w v)) 0))


(defun wire-remove-pin-asserting (w p v)
  "Remove P from the pins of W asserting V."
  (if-let ((asserting (wire-pins-asserting w v)))
    (setf (gethash v (slot-value w 'assertions-table))
	  (delete p asserting))))


(defun wire-add-pin-asserting (w p v)
  "Add P to the pins of W asserting V."
  (if-let ((asserting (wire-pins-asserting w v)))
    ;; value exists, add to the list
    (setf (gethash v (slot-value w 'assertions-table))
	  (cons p asserting))

    ;; new value, create a new list
    (setf (gethash v (slot-value w 'assertions-table))
	  (list p))))


(defmethod wire-add-pin ((w wire) p)
  (let ((v (slot-value p 'state)))
    ;; check we don't already know this pin
    (when (wire-known-pin-p w p)
      (error "Pin ~a is already attached to ~a" p w))

    ;; record the pin
    (setf (wire-pins w) (cons p (wire-pins w)))

    ;; place the component into the correct bucket
    (wire-add-pin-asserting w p v)

    ;; set the wire's state based on this new pin
    (if (wire-conflicting-assertion-p w v)
	(progn
	  (setf (slot-value w 'state) :floating)
	  (signal (make-instance 'conflicting-asserted-values :wire w)))
	(if (wire-logic-value-p v)
	    (setf (slot-value w 'state) v)))))


(defmethod (setf wire-state) (nv (w wire) p)
  (let ((wire-ov (slot-value w 'state))
	(pin-ov (slot-value p 'state))
	wire-nv)
    (when (not (equal wire-ov nv))
      ;; state is changing, remove pin from old bucket
      (wire-remove-pin-asserting w p pin-ov)

      ;; check for conflicts
      (when (wire-conflicting-assertion-p w nv)
	;; new assertion conflicts with others
	(wire-add-pin-asserting w p nv)
	(setf (slot-value w 'state) :floating)
	(signal (make-instance 'conflicting-asserted-values :wire w)))

      ;; put pin into the correct bucket
      (wire-add-pin-asserting w p nv)

      ;; determine new wire state
      (if (wire-logic-value-p nv)
	  ;; set logic value
	  (setq wire-nv nv)

	  ;; not a logic level, determine the new value
	  (setq wire-nv
		(cond ((wire-any-pin-asserting-p w 0) 0)
		      ((wire-any-pin-asserting-p w 1) 1)
		      (t :floating))))

      ;; set the new state
      (setf (slot-value w 'state) wire-nv)

      ;; propagate our new state to any :reading or :trigger pins
      ;; if the wire has achieved a new logic level: floating
      ;; values don't trigger a notification (but will signal
      ;; a condition if the pin is read)
      (when (not (and (equal wire-nv pin-ov)
		      (wire-logic-value-p wire-nv)))
	(dolist (p (wire-pins-asserting w :reading))
	  (pin-wire-state-changed p wire-nv))
	(dolist (p (wire-pins-asserting w :trigger))
	  (pin-wire-state-changed p wire-nv))))))


;; ---------- Pins ----------

(defclass pin ()
  ((component
    :documentation "The component the pin is attached to."
    :initarg :component
    :reader pin-component)
   (wire
    :documentation "The wire the pin connects to."
    :initarg :wire
    :initform nil
    :reader pin-wire)
   (state
    :documentation "The state being asseted by the component onto the wire."
    :initform :tristate
    :initarg :state))
  (:documentation "A pin connects a component to a wire.

Pins can be in one of several states, including:

- 1, asserting a logical 1
- 0, asserting a logical 0
- :read, reading the state of the wire
- :tristate, effectively disconnected from the wire
- :trigger, like read but waiting for an edge"))


(defmethod initialize-instance :after ((p pin) &rest initargs)
  (declare (ignore initargs))
  ;; if there's no wire been supplied, create one
  (when (null (pin-wire p))
    (setf (slot-value p 'wire) (make-instance 'wire)))

  ;; add pin to its wire
  (wire-add-pin (pin-wire p) p))


(defmethod pin-state ((p pin))
  (let ((pin-state (slot-value p 'state)))
    (if (equal pin-state :reading)
	(let ((w (pin-wire p)))
	  (if (wire-floating-p w)
	      (signal (make-instance 'reading-floating-value :wire w))
	      (wire-state w)))
	(error "Reading from non-reading pin ~a" p))))


(defmethod (setf pin-state) (nv (p pin))
  (let ((ov (slot-value p 'state)))
    (when (not (equal ov nv))
      (setf (wire-state (pin-wire p) p) nv)
      (setf (slot-value p 'state) nv))))


(defgeneric pin-wire-state-changed (p v)
  (:documentation "Notification method called when the wire P is attached to changes.

This method is called when the value of the wire changes to V due to
actions not caused by P: essentially is't a notification that someone
else asserted a new value on the wire.

The default does nothing: you can override or advise the method to
provide functions."))


(defmethod pin-wire-state-changed ((p pin) v)
  (declare (ignore p v)))


;; ---------- Buses ----------

(defclass bus ()
  ((width
    :documentation "The width of the bus."
    :initarg :width
    :initform 8
    :reader bus-width)
   (wires
    :documentation "The bus' wires, as a sequence."
    :reader bus-wires))
  (:documentation "A bus consisting of several wires."))


(defmethod initialize-instance :after ((b bus) &rest initargs)
  (declare (ignore initargs))

  ;; create the wires
  (let* ((width (bus-width b))
	 (wires (make-array (list width))))
    (dolist (i (iota width))
      (setf (elt wires i)
	    (make-instance 'wire)))
    (setf (slot-value b 'wires) wires)))


;; ---------- Registers ----------

(defclass register (component)
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
   (clock
    :documentation "The clock."
    :initarg :clock
    :reader register-clock)
   (enable
    :documentation "The register-enable."
    :initarg :enable
    :reader register-enable)
   (write-enable
    :documentation "The write-enable."
    :initarg :write-enable
    :reader register-write-enable))
  (:documentation "A register.

Registers must be connected to a bus and two wires. The data bus must
have at least as many wires as the register. The three other wires are
for clock, register enable, and write enable."))


(defmethod initialize-instance :after ((r register) &rest initargs)
  (declare (ignore initargs))

  ;; replace wires with pins connected to those wires
  (dolist (slot '(clock enable write-enable))
    (setf (slot-value r slot) (make-instance 'pin
					     :component r
					     :wire (slot-value r slot))))

  ;; set up all the triggers
  (defmethod pin-wire-state-changed ((p (eql (register-write-enable r))) (v (eql 1)))
    (let ((reg (pin-component p)))
      ;; change data bus to all :reading
      (dolist (p (register-data-bus reg))
	(setf (pin-state p) :reading))))

  )
