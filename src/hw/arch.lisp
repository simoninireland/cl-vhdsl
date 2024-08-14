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
indicating a non-asserting (floating) state. Use
`wire-floating-p' to check for floating wires."
  (or (equal v 0)
      (equal v 1)))


(defun wire-other-logic-value (v)
  "Return the other logic value to V.

0 and 1 exchange values; other (non-logical) values are left unchanged."
  (cond ((= v 0)
	 1)
	((= v 1)
	 0)
	((symbolp v)
	 v)
	(t
	 (error "Strange value ~a assigned to wire" v))))


(defun wire-floating-p (w)
  "Test whether W is floating.

A pin is floating if it doesn't have a stable logic value
asserted on it."
  (eql (wire-state w) :floating))


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


(defun pin-wire-state (p)
  "Return the state of P's wire.

The wire may have a value asserted, or may be floating. Unlike
`pin-state' it is not an error to call this function on a floating
or tristated pin."
  (wire-state (pin-wire p)))


(defun pin-tristated-p (p)
  "Test if P is tristated."
  (equal (slot-value p 'state) :tristate))


(defun pin-reading-p (p)
  "Test if P is reading."
  (equal (slot-value p 'state) :reading))


(defun pin-asserted-p (p)
  "Test if P has a value asserted on it."
  (let ((v (slot-value p 'state)))
    (or (equal v 0)
	(equal v 1))))


(defun pin-floating-p (p)
  "Test if P is floating.

A floating pin is either tristated or configured for reading
at attached to a floating pin."
  (or (pin-tristated-p p)
      (and (pin-reading-p p)
	   (wire-floating-p (pin-wire p)))))


(defun pin-state (p)
  "Return the state of P.

This function can only be used to read the states of pins
that are reading or asserted. For asserted pins it returns their
asserted value. For `:reading' pins it returns the state of the underlying
wire, signalling a `reading-floating-value' condition if the wire is
floating; and for other pins, notable `:tristate', it signals a
`reading-non-reading-pin' condition.

The `reading-floating-value' signal can be ignored if the caller is
happy to receive floating values. returning from the condition will
return the floating value."
  (cond ((pin-asserted-p p)
	 (slot-value p 'state))

	((pin-tristated-p p)
	 (error 'reading-non-reading-pin :pin p))

	((pin-floating-p p)
	 (error 'reading-floating-value :pin p))

	((pin-reading-p p)
	 (pin-wire-state p))

	;; shouldn't get here, it's a weird state
	(t
	 (error 'reading-non-reading-pin :pin p))))


(defmethod (setf pin-state) (nv (p pin))
  (let ((ov (slot-value p 'state)))
    (when (not (equal ov nv))
      (setf (slot-value p 'state) nv)
      (setf (wire-state (pin-wire p) p ov) nv))))


;; ---------- Connectors ----------

(defclass connector ()
  ((width
    :documentation "The width of the connector."
    :initarg :width
    :reader connector-width)
   (pins
    :documentation "The pins of the connector."
    :reader connector-pins)
   (component
    :documentation "The component this connector's pins are connected to."
    :initarg :component
    :initform nil
    :reader connector-component)
   (role
    :documentation "The role of the pins in this connector."
    :initarg :role
    :initform :io
    :reader connector-role))
  (:documentation "A sequence of pins.

A connector connects a sequence of wires to a component."))


(defmethod initialize-instance :after ((conn connector) &rest initargs)
  (declare (ignore initargs))

  ;; create the pins, without wires
  (let* ((c (connector-component conn))
	 (width (connector-width conn))
	 (role (connector-role conn))
	 (pins (make-array (list width))))
    (dolist (i (iota width))
      (let ((pin (make-instance 'pin)))
	(configure-pin-for-role pin role)
	(setf (elt pins i) pin)
	(setf (pin-component pin) c)))
    (setf (slot-value conn 'pins) pins)))


(defun connector-pins-floating (conn)
  "Return a list of floating pins in CONN."
  (remove-if-not #'pin-floating-p (connector-pins conn)))


(defun connector-pins-floating-p (conn)
  "Test whether any of the pins of CONN are floating."
  (> (length (connector-pins-floating conn)) 0))


(defun connector-pin-states (conn)
  "Return a sequence of state values for the pins of CONN."
  (map 'vector #'pin-state (connector-pins conn)))


(defmethod (setf connector-pin-states) (nv (conn connector))
  (map nil (lambda (p)
	     (setf (pin-state p) nv))
       (connector-pins conn)))


(defun connector-pins-value (conn)
  "Move the values of the pins on CONN into an integer.

The first pin in the pins of CONN is moved to the least-significant
bit of the value, and so on."
  (let* ((ps (connector-pins conn))
	 (v 0)
	 (n (1- (length ps))))
    (dolist (i (iota (length ps)))
      (setf v (+ (ash v 1)
		 (pin-state (elt ps (- n i))))))
    v))


(defmethod (setf connector-pins-value) (v (conn connector))
  (let ((ps (connector-pins conn))
	(nv v))
    (dolist (i (iota (length ps)))
      (setf (pin-state (elt ps i)) (logand nv 1))
      (setf nv (ash nv -1)))))
