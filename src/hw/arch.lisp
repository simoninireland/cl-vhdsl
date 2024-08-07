;; Underlying services for fully-software-emulated hardware components
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
  ;; filter-out any nulls from pins without an associated component
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
pin. (This situation is assumed to be unusual.)"
  (let* ((wire-ov (slot-value w 'state))
	 (wire-nv (wire-determine-state w)))
     (when (not (equal wire-ov wire-nv))
	 ;; store the new state
	 (setf (slot-value w 'state) wire-nv)

	 ;; propagate changes
	 ;; TBC: should we also propagate floating values?
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
      (error "Pin ~s is already attached to wire ~s" p w))

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


;; ---------- Pins ----------

(defclass pin ()
  ((component
    :documentation "The component the pin is attached to."
    :initform nil
    :initarg :component
    :accessor pin-component)
   (wire
    :documentation "The wire the pin connects to."
    :initarg :wire
    :initform (make-instance 'wire)
    :accessor pin-wire)
   (state
    :documentation "The state being asseted by the component onto the wire."
    :initform :tristate
    :initarg :state))
  (:documentation "A pin connects a component to a wire.

Pins can be in one of several states, including:

- 1, asserting a logical 1
- 0, asserting a logical 0
- :reading, reading the state of the wire
- :tristate, effectively disconnected from the wire
- :trigger, like read but waiting for an edge

The main difference between :reading and :trigger is how changes to
the underlying wire state are notified to the attached components. For
a :reading pin, a change causes a call to `component-pin-changed'
on the pin's component. For :trigger pins, a change causes a call
to `component-pin-triggered', which also specifies the pin that
caused the notification."))


(defmethod initialize-instance :after ((p pin) &rest initargs)
  (declare (ignore initargs))

  (wire-add-pin (pin-wire p) p))


(defmethod (setf pin-wire) (wire (p pin))
  (setf (slot-value p 'wire) wire)
  (wire-add-pin wire p))


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


;; ---------- Manipulating several pins simultaneousosly ----------

(defun pins-floating (ps)
  "Test whether any of the pins in PS are floating.

This is used to propagate floating states."
  (some #'(lambda (p)
	    (wire-floating-p (pin-wire p)))
	ps))


(defun pins-states (ps)
  "Return a sequence of state values for the pins PS."
  (map 'vector #'pin-state ps))


(defmethod (setf pins-states) (nv ps)
  (map nil (lambda (p)
	     (setf (pin-state p) nv))
       ps))


(defun pins-from-value (ps v)
  "Move the value of V onto the pins PS.

The least-significant bit of V goes onto the first pin
in the sequence PS, and so on."
  (let ((nv v))
    (dolist (i (iota (length ps)))
      (setf (pin-state (elt ps i)) (logand nv 1))
      (setf nv (ash nv -1)))))


(defun pins-to-value (ps)
  "Move the values of the pins in PS into an integer.

The first pin in the sequence PS is moved to the least-significant
bit of the value, and so on."
  (let* ((v 0)
	 (n (1- (length ps))))
    (dolist (i (iota (length ps)))
      (setf v (+ (ash v 1)
		 (pin-state (elt ps (- n i))))))
    v))


(defgeneric pins-for-wires (ws &key state component)
  (:documentation "Create a connector to WS.

The connector is a seuence of pins attached to the wires of WS.
WS may be a sequence or a bus. The pins are initially given state
STATE (:io by default) and are assocated with COMPONENT."))


(defmethod pins-for-wires ((b bus) &key (state ':io) component)
  (pins-for-wires (bus-wires b) :state state :component component))


(defmethod pins-for-wires ((ws sequence) &key (state ':io) component)
  (map 'vector #'(lambda (w)
		   (make-instance 'pin :wire w :state state :component component))
       ws))
