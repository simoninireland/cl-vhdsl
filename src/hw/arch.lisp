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

;; ---------- Generic functions ----------

(defgeneric name (e)
  (:documentation "Return the user-readable name of element E."))


(defgeneric pins (e)
  (:documentation "Return a list of all the pins on element E."))


(defgeneric state (e)
  (:documentation "Return the state of element E."))


(defgeneric wires (e)
  (:documentation "Return all the wires connected to element E."))


(defgeneric components (e)
  (:documentation "Return all the components connected to element E.

If E is a component itself, this will include all sub-components. "))


(defgeneric width (e)
  (:documentation "Return the width in bits of element E."))


(defgeneric floating-p (e)
  (:documentation "Test whether element E has any floating values.

Floating values are usually (although not necessarily) an error."))


(defgeneric fully-wired-p (e)
  (:method-combination and)
  (:documentation "Test whether element E is fully wired.

Dangling wires are usually (although not necessarily) an error.
Use `ensure-fully-wired' to test a set of elements at once and
signal an error if there are dangling wires.

Additional methods added to this function must use the \"and\"
qualifier, and are treated as a conjunction,")

  ;; the default checks that all pins are wired
  (:method and (e)
    (null (remove-if #'pin-wired-p (pins e)))))


;; ---------- Wires ----------

(defclass wire ()
  ((state
    :documentation "The state of the wire."
    :initform :floating
    :initarg :state
    :reader state)
   (pins
    :documentation "List of pins attached to the wire."
    :initform '()
    :accessor pins)
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


(defmethod wires ((w wire))
  (list w))


(defmethod width ((w wire))
  1)


(defun logic-value-p (v)
  "Test whether V is a logic value.

V can take a logic value (0 or 1) or some other value
indicating a non-asserting (floating) state. Use
`wire-floating-p' to check for floating wires."
  (or (equal v 0)
      (equal v 1)))


(defun other-logic-value (v)
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


(defmethod floating-p ((w wire))
  (eql (state w) :floating))


(defun wire-known-pin-p (w p)
  "Test that P is a pin attached to W."
  (member p (pins w)))


(defun wire-pins-asserting (w v)
  "Return the list of pins of W asserting V."
  (gethash v (wire-pin-assertions w)))


(defun wire-components-with-pins-asserting (w v)
  "Return a list of all components of pins of W asserting V."

  ;; filter-out any nulls from pins without an associated component
  ;; (usually global pins like clocks)
  (remove-if #'null
	     (mapcar #'component (wire-pins-asserting w v))))


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
	 (when (logic-value-p wire-nv)
	   ;; for :reading pins, call their components' change notification
	   (dolist (c (wire-components-with-pins-asserting w :reading))
	     (on-pin-changed c))

	   ;; for :trigger pins, call the pin's change notification
	   (dolist (p (wire-pins-asserting w :trigger))
	     (on-pin-triggered (component p) p wire-nv))))))


(defun wire-add-pin (w p)
  "Add pin P to wire W."
  (let ((v (slot-value p 'state)))
    ;; check we don't already know this pin
    (when (wire-known-pin-p w p)
      (error "Pin ~s is already attached to wire ~s" p w))

    ;; record the pin
    (setf (pins w) (cons p (pins w)))

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
    :reader width)
   (wires
    :documentation "The bus' wires, as a sequence."
    :reader wires))
  (:documentation "A bus consisting of several wires."))


(defmethod initialize-instance :after ((b bus) &rest initargs)
  (declare (ignore initargs))

  ;; create the wires
  (let* ((w (width b))
	 (wires (make-array (list w))))
    (dolist (i (iota w))
      (setf (elt wires i)
	    (make-instance 'wire)))
    (setf (slot-value b 'wires) wires)))


(defmethod floating-p ((b bus))
  (some #'floating-p (wires b)))


;; ---------- Pins ----------

(defclass pin ()
  ((component
    :documentation "The component the pin is attached to."
    :initform nil
    :initarg :component
    :accessor component)
   (wire
    :documentation "The wire the pin connects to."
    :initarg :wire
    :initform nil
    :accessor wire)
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


(defun ensure-pin-state (p state)
  "Ensure it makes sense to use STATE for P."
  (unless (member state '(0 1 :reading :tristate :trigger))
    (error 'illegal-pin-state :pin p :state state)))


(defmethod initialize-instance :after ((p pin) &rest initargs)
  (declare (ignore initargs))

  ;; connect to the underlying wire if there is one
  (when (pin-wired-p p)
    (wire-add-pin (wire p) p)))


(defmethod pins ((p pin))
  (list p))


(defmethod wires ((p pin))
  (list (wire p)))


(defmethod (setf wire) (wire (p pin))
  (setf (slot-value p 'wire) wire)
  (wire-add-pin wire p))


(defun pin-tristated-p (p)
  "Test if P is tristated."
  (equal (slot-value p 'state) :tristate))


(defun pin-wired-p (p)
  "Test whether P is attached to a wire or not."
  (not (null (wire p))))


(defun pin-reading-p (p)
  "Test if P is reading."
  (equal (slot-value p 'state) :reading))


(defun pin-asserted-p (p)
  "Test if P has a value asserted on it."
  (let ((v (slot-value p 'state)))
    (or (equal v 0)
	(equal v 1))))


(defmethod floating-p (p)
  (or (pin-tristated-p p)
      (and (pin-reading-p p)
	   (floating-p (wire p)))))


(defmethod state ((p pin))
  (cond ((pin-asserted-p p)
	 (slot-value p 'state))

	((pin-tristated-p p)
	 (error 'reading-non-reading-pin :pin p))

	((floating-p p)
	 (error 'reading-floating-value :pin p))

	((pin-reading-p p)
	 (state (wire p)))

	;; shouldn't get here, it's a weird state
	(t
	 (error 'reading-non-reading-pin :pin p))))


(defmethod (setf state) (nv (p pin))
  (let ((ov (slot-value p 'state)))
    ;; only update when the state changes
    (when (not (equal ov nv))
      ;; make sure the pin state makes sense
      (ensure-pin-state p nv)

      (setf (slot-value p 'state) nv)
      (when (pin-wired-p p)
	  (setf (wire-state (wire p) p ov) nv)))))


;; ---------- Connectors ----------

(defclass connector ()
  ((width
    :documentation "The width of the connector."
    :initarg :width
    :reader width)
   (pins
    :documentation "The pins of the connector."
    :reader pins-sequence)
   (component
    :documentation "The component this connector's pins are connected to."
    :initarg :component
    :initform nil
    :reader component)
   (role
    :documentation "The role of the pins in this connector."
    :initarg :role
    :initform :io
    :reader role))
  (:documentation "A sequence of pins.

A connector connects a sequence of wires to a component."))


(defmethod initialize-instance :after ((conn connector) &rest initargs)
  (declare (ignore initargs))

  ;; create the pins, without wires
  (let* ((c (component conn))
	 (w (width conn))
	 (role (role conn))
	 (pins (make-array (list w))))
    (dolist (i (iota w))
      (let ((pin (make-instance 'pin)))
	(configure-pin-for-role pin role)
	(setf (elt pins i) pin)
	(setf (component pin) c)))
    (setf (slot-value conn 'pins) pins)))


(defmethod components ((c connector))
  (list (component c)))


(defun connector-pins-floating (conn)
  "Return a list of floating pins in CONN."
  (remove-if-not #'floating-p (coerce (pins conn) 'list)))


(defmethod floating-p ((conn connector))
  (> (length (connector-pins-floating conn)) 0))


(defmethod pins ((conn connector))
  (coerce (pins-sequence conn) 'list))


(defun pin-states (conn)
  "Return a sequence of state values for the pins of CONN."
  (map 'vector #'state (pins conn)))


(defmethod (setf pin-states) (nv (conn connector))
  (map nil (lambda (p)
	     (setf (state p) nv))
       (pins conn)))


(defun pins-value (conn)
  "Move the values of the pins on CONN into an integer.

The first pin in the pins of CONN is moved to the least-significant
bit of the value, and so on."
  (let* ((ps (pins conn))
	 (v 0)
	 (n (1- (length ps))))
    (dolist (i (iota (length ps)))
      (setf v (+ (ash v 1)
		 (state (elt ps (- n i))))))
    v))


(defmethod (setf pins-value) (v (conn connector))
  (let ((ps (pins conn))
	(nv v))
    (dolist (i (iota (length ps)))
      (setf (state (elt ps i)) (logand nv 1))
      (setf nv (ash nv -1)))))
