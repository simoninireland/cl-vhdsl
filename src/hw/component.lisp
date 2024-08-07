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
  ((name
    :documentation "The readable name of the component."
    :initarg :name
    :reader component-name)
   (enable
    :documentation "The component-enable pin."
    :initarg :enable
    :pins 1
    :role :control))
  (:metaclass metacomponent)
  (:documentation "A component in an architecture.

Components encapsulate functions and offer a pin-based interface."))


;; Initialisation of components is split into two parts. We first
;; override the primary method for `initialize-instance' to create the
;; pins for all slots in the pin interface for which we know their
;; width. We then add an :around method that calls
;; `component-pins-changed' to let the component set up its internal
;; state tro be consistent with its initial pin values.
;;
;; The reason to separate these, and for using the :around method, is
;; to support sub-class initialisation. The new primary method runs
;; first and calls the underlying `initialize-instance' method to
;; create and populate other slots. This will run any :before methods,
;; then run the overridden primary method, and then run any :after
;; methods on `initilize-instance' that sub-classes might define.
;; These :after methods will see an object whose pins have been
;; initialised, as expected. The :around method then calls
;; `component-pins-changed' in an environment where the pins *and* any
;; other state has been initialised, which means it sees the component
;; in a sensible state.

;; This method should be decomposed to let the individual elements be
;; called/overridden programmatically

(defmethod initialize-instance ((cc component) &rest initargs)
  (declare (ignore initargs))

  (let* ((c (call-next-method))
	 (slot-defs (class-slots (class-of c))))
    (dolist (slot-def slot-defs)
      (when (slot-in-pin-interface-p slot-def)
	;; slot is in the pin interface
	(let* ((slot (slot-definition-name slot-def))

	       ;; number of wires in the slot
	       (width (let ((w (slot-value slot-def 'pins)))
			(if (and (symbolp w)
				 (not (eql w t))) ;; t = undefined
			    ;; width derived from the value of another slot
			    (let ((nw (slot-value c w)))
			      ;; update the :pins attribute to the constant value
			      (setf (slot-value slot-def 'pins) nw)
			      nw)

			    ;; width as given
			    w)))

	       ;; role the slot fulfills
	       ;; (set by `compute-effective-slot-definition' is omitted)
	       (role (slot-value slot-def 'role))

	       ;; wires the slot's pins should be connected to
	       (wires (if (slot-exists-and-bound-p c slot)
			  (normalise-wires (slot-value c slot)))))

	  ;; if we have wires, make sure we have enough
	  (if wires
	      (cond ((eql width t)
		     ;; no width, set it to the number of wires we've been given
		     (setq width (length wires)))

		    ((not (equal (length wires) width))
		     ;; wrong number of wires, signal an error
		     (error (make-instance 'mismatched-wires
					   :component c
					   :slot slot
					   :expected width
					   :received (length wires))))))

	  ;; create the pins
	  (setf (slot-value c slot)
		(cond ((equal width t)
		       ;; width not yet known, leave unset
		       nil)

		      ((= width 1)
		       ;; slot gets a single pin
		       (let ((pin (make-pin-for-role role)))
			 (if wires
			     (setf (pin-wire pin) (elt wires 0)))
			 (setf (pin-component pin) c)
			 pin))

		      (t
		       ;; slots gets a vector of pins
		       (map 'vector
			    #'(lambda (i)
				(let ((pin (make-pin-for-role role)))
				  (if wires
				      (setf (pin-wire pin) (elt wires i)))
				  (setf (pin-component pin) c)
				  pin))
			    (iota width))))))))

    ;; return the instance
    c))


(defmethod initialize-instance :around ((c component) &rest initargs)
  (declare (ignore initargs))

  ;; do the normal initialisation routines
  (call-next-method)

  ;; make sure we're in a state consistent with our initial pins
  (component-pin-changed c))


;; ---------- Enablement checks ----------

(defgeneric component-enabled-p (c)
  (:documentation "Test whether the component is enabled."))


(defmethod component-enabled-p ((c component))
  (equal (pin-state (slot-value c 'enable)) 1))


;; ---------- Pin interface callbacks ----------

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


(defun component-pins (c)
   "Return all the pins in all the slots of C."
  (let* ((cl (class-of c))
	 (pin-slots (pin-interface cl)))
    (flatten (map 'list #'(lambda (slot)
			    (let ((pins (slot-value c slot)))
			      (typecase pins
				(sequence (coerce pins 'list))
				(list pins)
				(t (list pins)))))
		  pin-slots))))


;; ---------- Mixins for common components ----------

(defclass clocked ()
  ((clock
    :documentation "The component's clock pin."
    :initarg :clock
    :pins 1
    :role :trigger
    :reader component-clock))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that has a clock line.

Clocked components do most of their active work when the clock
transitions, although they can change state at other times too."))


(defclass readwrite ()
  ((write-enable
    :documentation "The component's write-enable pin."
    :initarg :write-enable
    :pins 1
    :role :control
    :reader component-write-enable))
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
