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
    :reader component-pins)
    (enable
    :documentation "The component-enable pin."
    :initarg :enable
    :reader component-enable))
  (:documentation "A component in an architecture.

Components encapsulate functions and offer a pin-based interface."))


(defmethod initialize-instance :after ((c component) &rest initargs)
  (declare (ignore initargs))

  ;; create a pin for the enable wire
  (setf (slot-value c 'enable) (make-instance 'pin
					      :component c
					      :wire (slot-value c 'enable)
					      :state :reading
					      :component c))
)


(defmethod component-pin ((c component) n)
  (gethash (component-pins c) n))


(defmethod (setf component-pin) (p (c component) ns)
  (let ((pt (component-pins c)))
    (dolist (n ns)
      (if (gethash n pt)
	  (error "Duplicate pin name ~a on component ~a" n c)
	  (setf (gethash n pt) p)))))


(defgeneric component-enabled-p (c)
  (:documentation "Test whether the component is enabled."))


(defmethod component-enabled-p ((c component))
  (equal (pin-state (component-enable c)) 1))


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



(defclass clocked ()
  ((clock
    :documentation "The component's clock pin."
    :initarg :clock
    :reader component-clock))
  (:documentation "A mixin for a component that has a clock line.

Clocked components do most of their active work when the clock
transitions, although they can change state at other times too."))


(defmethod initialize-instance :after ((c clocked) &rest initargs)
  (declare (ignore initargs))

  ;; create a pin for the enable clock wire
  (setf (slot-value c 'clock) (make-instance 'pin
					     :component c
					     :wire (slot-value c 'clock)
					     :state :trigger)))


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


(defun wire-floating-p (w)
  "Test whether W is floating."
  (equal (slot-value w 'state) :floating))


(defun wire-known-pin-p (w p)
  "Test that P is a pin attached to W."
  (member p (wire-pins w)))


(defun wire-pins-asserting (w v)
  "Return the list of pins of W asserting V."
  (gethash v (wire-pin-assertions w)))


(defun wire-components-with-pins-asserting (w v)
  "Return a list of all components of pins of W asserting V."
  ;; filter-out any nulls, from pins without an associated component
  ;; (usually global pins like clocks)
  (remove-if #'null
	     (mapcar #'pin-component (wire-pins-asserting w v))))


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


(defun wire-determine-state (w)
  "Determine the electrical state 0f W.

The wire is in states 0 or 1 if only that logic value is being asserted
by any pin, and :floating otherwise. Pins that are :reading,
:trigger, or :tristate have no effect."
  (cond ((and (wire-any-pin-asserting-p w 0)
	      (not (wire-any-pin-asserting-p w 1)))
	 0)
	((and (wire-any-pin-asserting-p w 1)
	      (not (wire-any-pin-asserting-p w 0)))
	 1)
	(t
	 :floating)))


(defun wire-update-state-and-trigger (w)
  "Update the state of W and notify any pins.

The components of :reading pins are notified by calling
`component-pin-changed' once per component. The components of :trigger
pins are notifed by calling `component-pin-triggered' once per pin: if
a single component happened to have two or ore :tigger pins attached
to the same wire, it would get multiple notifications, one for each
pin. (This siutaion is assumed to be unusual.)"
  (let* ((wire-ov (slot-value w 'state))
	 (wire-nv (wire-determine-state w)))
     (when (not (equal wire-ov wire-nv))
	 ;; store the new state
	 (setf (slot-value w 'state) wire-nv)

	 ;; propagate changes
	 (when (wire-logic-value-p wire-nv)
	   ;; for :reading pins, call their components' change notification
	   (dolist (c (wire-components-with-pins-asserting w :reading))
	     (component-pin-changed c))

	   ;; for :trigger pins, call the pin's change notification
	   (dolist (p (wire-pins-asserting w :trigger))
	     (component-pin-triggered (pin-component p) p wire-nv))))))


(defmethod wire-add-pin ((w wire) p)
  (let ((v (slot-value p 'state)))
    ;; check we don't already know this pin
    (when (wire-known-pin-p w p)
      (error "Pin ~a is already attached to ~a" p w))

    ;; record the pin
    (setf (wire-pins w) (cons p (wire-pins w)))

    ;; place the pin into the correct bucket
    (wire-add-pin-asserting w p v)

    ;; set the wire's state based on this new pin
    (wire-update-state-and-trigger w)))


(defmethod (setf wire-state) (nv (w wire) p ov)
  (when (not (equal ov nv))
    ;; state is changing, remove pin from old bucket and place
    ;; into new bucket
    (wire-remove-pin-asserting w p ov)
    (wire-add-pin-asserting w p nv)

    ;; set the wire's state based on this new pin
    (wire-update-state-and-trigger w)))


;; ---------- Pins ----------

(defclass pin ()
  ((component
    :documentation "The component the pin is attached to."
    :initform nil
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
	  (when (wire-floating-p w)
	    ;; signal to allow rejection of floating logic levels
	    (error (make-instance 'reading-floating-value :pin p :wire w)))

	  ;; if not floating, and we return from the condition, return the value
	  (wire-state w))
	(error "Reading from non-reading pin ~a" p))))


(defmethod (setf pin-state) (nv (p pin))
  (let ((ov (slot-value p 'state)))
    (when (not (equal ov nv))
      (setf (slot-value p 'state) nv)
      (setf (wire-state (pin-wire p) p ov) nv))))


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
  (setf (slot-value r 'write-enable) (make-instance 'pin
						    :component r
						    :wire (slot-value r 'write-enable)
						    :state :reading))

  ;; attach pins to the data bus wires
  (setf (slot-value r 'data-bus)
	(map 'vector (lambda (w)
		       (make-instance 'pin :wire w :state :tristate))
	     (bus-wires (slot-value r 'data-bus)))))


(defmethod component-pin-triggered ((r register) p (v (eql 1)))
  (declare (ignore p))            ;; we only have one trigger pin

  (when (and (component-enabled-p r)
	     (register-write-enabled-p r))
    (register-value-from-data-bus r)))


(defmethod component-pin-changed ((r register))
  (if (component-enabled-p r)
      (if (register-write-enabled-p r)
	  ;; set all data bus pins to :reading
	  (map nil (lambda (p)
		 (setf (pin-state p) :reading))
	   (register-data-bus r))

	  ;; put the value of the register onto the data bus pins
	  (register-value-to-data-bus r))

      ;; tri-state the data bus
       (map nil (lambda (p)
		 (setf (pin-state p) :tristate))
	   (register-data-bus r))))


(defun register-value-to-data-bus (r)
  "Move the value of R to the pins of the data bus."
  (let ((nv (register-value r))
	(pins (register-data-bus r)))
    (dolist (i (iota (register-width r)))
      (setf (pin-state (elt pins i)) (logand nv 1))
      (setf nv (ash nv -1)))))


(defun register-value-from-data-bus (r)
  "Make the value on the pins of the data bus the value of R.

This implies that the pins are all :reading."
  (let* ((nv 0)
	 (pins (register-data-bus r))
	 (n (1- (length pins))))
    (dolist (i (iota (register-width r)))
      (setf nv (+ (ash nv 1)
		  (pin-state (elt pins (- n i))))))
    (setf (slot-value r 'value) nv)))


(defun register-write-enabled-p (r)
  "Test if the R is write-enabled.

Write-enabled means that the write enable pin is high."
  (equal (pin-state (register-write-enable r)) 1))


(defun register-read-enabled-p (r)
  "Test if the R is read-enabled.

Read-enabled means that the write enable pin is highlow."
  (equal (pin-state (register-write-enable r)) 0))
