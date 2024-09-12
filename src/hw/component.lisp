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
    :reader name))
  (:metaclass metacomponent)
  (:documentation "A component in an architecture.

Components encapsulate functions and offer a pin-based interface."))


;; We add an :after method for `initialize-instance' to create the pins
;; for all slots in the pin interface for which we know their width.
;; We then wire-up these components and slots by calling
;; `connect-component' and finally call `component-pins-changed' to
;; let the component set up its internal state to be consistent with
;; its initial pin values.
;;
;; If sub-classes add additional :after methods specialised against
;; themselves, these will see a component that's fully wired-up
;; according to any embeded wiring diagram.
;;
;; The :after method should probably be decomposed to let the
;; individual elements be called/overridden programmatically

(defun component-width-of-slot (c slot-def)
  "Return the width of SLOT-DEF on C."
  (let ((w (slot-value slot-def 'pins)))
    (if (and (symbolp w)
	     (not (eql w t))) ;; t = undefined
	;; width derived from the value of another slot
	(slot-value c w)

	;; width as given
	w)))


(defmethod initialize-instance :after ((c component) &rest initargs)
  (declare (ignore initargs))

  ;; create the slots and connectors for the pin interface
  (let* ((slot-defs (class-slots (class-of c)))
	 (pin-slot-defs (remove-if-not #'slot-def-in-pin-interface-p slot-defs)))
    (dolist (slot-def pin-slot-defs)
      (let* ((slot (slot-definition-name slot-def))

	     ;; number of wires in the slot
	     (width (component-width-of-slot c slot-def))

	     ;; role the slot fulfills
	     ;; (set by `compute-effective-slot-definition' if omitted)
	     (role (slot-value slot-def 'role))

	     ;; wires the slot's pins should be connected to
	     (bus (if (slot-exists-and-bound-p c slot)
		      (slot-value c slot))))

	;; sanity check any wiring
	(when bus
	  ;; we accept single wires and single pins
	  (unless (typep bus 'bus)
	    (let ((wire-or-pin bus))
	      (etypecase wire-or-pin
		(wire
		 (setq bus (make-instance 'bus :width 1))
		 (setf (elt (wires bus) 0) wire-or-pin))
		(pin
		 (setq bus (make-instance 'bus :width 1))
		 (setf (elt (wires bus) 0) (wire wire-or-pin))))))

	  ;; check widths match
	  (let ((bus-width (width bus)))
	    (cond ((eql width t)
		   ;; no width, set it to the number of
		   ;; wires on the bus we've been given
		   (setq width bus-width))

		  ;; wrong number of wires, signal an error
		  ((not (equal bus-width width))
		   (error 'mismatched-wires
			  :component c
			  :slot slot
			  :expected width
			  :received bus-width)))))

	;; create the connector
	(setf (slot-value c slot)
	      (if (not (equal width t))
		  (let ((conn (make-instance 'connector :width width
							:component c
							:role role)))

		    ;; if we've been given a bus, connect it
		    (when bus
		      (connect-pins conn bus))

		    conn))))))

  ;; wire-up the slots we created using the wiring diagram
  (connect-component c)

  ;; TODO make sure all sub-component slots that have objects have
  ;; all their slots wired

  ;; make sure we're in a state consistent with our initial pins
  (on-pin-changed c))


;; ---------- Pin interface ----------

(defmethod pin-interface ((c component))
  (pin-interface (class-of c)))


(defun pin-interface-p (c slot)
  "Test whether SLOT is in the pin interface of C.

Returns nil if SLOT either isn't a slot in C's pin
interface, or isn't a slot of C at all."
  (not (null (member slot (pin-interface c)))))


(defun pin-role-for-slot (c slot)
  "Return the role assigned to the pins of SLOT in class CL.

SLOT must be in C's pin interface."
  (let ((slot-def (find-pin-slot-def (class-of c) slot :fatal t)))
    (slot-value slot-def 'role)))


(defun pin-slots-for-roles (c roles)
  "Extract all the slots in the pin interface of C having roles in ROLES.

C may be a comppnent or a component class."
  (remove-if-not #'(lambda (slot)
		     (member (pin-role-for-slot c slot) roles))
		 (pin-interface c)))


;; This includes only the pin on the component, not on its sub-components

(defmethod pins ((c component))
  (let ((pin-slots (pin-interface c)))
    (mapappend #'(lambda (slot)
		   (pins (slot-value c slot)))
	       pin-slots)))


;; ---------- Pin roles ----------

(defgeneric configure-pin-for-role (pin role)
  (:documentation "Set up PIN suitable for ROLE.

Methods can specialise this function to configure pins appropriatly for
new roles.

The standard roles are:

   - `:io' for I/O pins that can be read from and written to
   - `:reading' for pins that can only be read from
   - `:control' for control pins permanently in `:reading' mode
   - `:status' for pins reporting component status
   - `:trigger' for pins that respond to a leading or trailing edge,
     typically clock pins"))


(defmethod configure-pin-for-role (pin (role (eql :io)))
  (setf (state pin) :tristate))

(defmethod configure-pin-for-role (pin (role (eql :reading)))
  (setf (state pin) :reading))

(defmethod configure-pin-for-role (pin (role (eql :control)))
  (setf (state pin) :reading))

(defmethod configure-pin-for-role (pin (role (eql :status)))
  (setf (state pin) 0))

(defmethod configure-pin-for-role (pin (role (eql :trigger)))
  (setf (state pin) :trigger))


;; ---------- Behavioural interface ----------

(defgeneric on-pin-changed (c)
  (:method-combination guarded)
  (:documentation "Callback called when the values asserted on pins of component C change.

This happens for changes on `:reading' and `:control' pins only.
Changes to tristated pins are ignored; changes to `:trigger' pins
cause a `pin-triggered' callback.

The methods on this function are guarded, meaning that methods may
implement the `:if' qualifier to conditionally prevent execution of
the method if the guard evaluates to false.")

  ;; default callback is empty
  (:method ((c component))))


(defgeneric on-pin-triggered (c p v)
  (:method-combination guarded)
  (:documentation "Callback called when trigger pin P on component C transitions to V.

This method can be specialised using `eql' to only be fired on (for example)
rising transitions to 1.

The methods on this function are guarded, meaning that methods may
implement the `:if' qualifier to conditionally prevent execution of
the method if the guard evaluates to false.")

  ;; default callback is empty
  (:method ((c component) p v)))


;; ---------- Sub-components ----------

(defmethod subcomponent-interface ((c component))
  (subcomponent-interface (class-of c)))


(defun subcomponent-p (c slot)
  "Test whether SLOT holds a sub-component of C."
  (not (null (member slot (subcomponent-interface c)))))


(defun subcomponent-type (cl slot)
  "Return the type of the sub-components expected in SLOT of component class CL."
  (let ((slot-def (find slot (class-slots cl) :key #'slot-definition-name)))
    (slot-definition-type slot-def)))


(defmethod components ((c component))
  (mapcar #'(lambda (slot)
	      (slot-value c slot))
	  (subcomponent-interface c)))


;; ---------- Integral wiring diagrams ----------

(defun ensure-wire-description-endpoint (cl endpoint)
  "Parse ENDPOINT against class CL.

Check the endpoint is valid, meaning that it is a symbol
naming a pin slot on CL or a pair of symbols naming a sub-component
slot on CL and a pin slot on that sub-component."
  (or (cond ((symbolp endpoint)
	     ;; slot on this component
	     (pin-interface-p cl endpoint))

	    ((consp endpoint)
	     ;; slot on a sub-component
	     (let* ((cslot (car endpoint))
		    (slot-type (subcomponent-type cl cslot)))
	       (if slot-type
		   (let ((slot (safe-cadr endpoint)))
		     (ensure-wire-description-endpoint (find-class slot-type) slot))))))

      ;; if we fall through, the endpoint was invalid
      (error 'invalid-endpoint :component cl :endpoint endpoint)))


(defun ensure-wire-description (cl wiredesc)
  "Parse WIREDESC against class CL and check validity."
  (every #'(lambda (endpoint)
	     (ensure-wire-description-endpoint cl endpoint))
	 wiredesc))


(defun ensure-wiring-diagram (cl wires)
  "Parse the WIRES give for class CL."
  (if (every #'(lambda (wiredesc)
		 (ensure-wire-description cl wiredesc))
	     wires)
      wires))


(defun endpoint-connector (c endpoint)
  "Return the connector for ENDPOINT on C."
  (if (symbolp endpoint)
      (slot-value c endpoint)
      (slot-value (slot-value c (car endpoint)) (safe-cadr endpoint))))


;; TODO This will fall foul of pin slots with widths of T, which can
;; be connected top any "appropriate" bus.

(defun connect-wire (c w)
  "Connect all the slots in W within C."
  (let (b)
    (dolist (endpoint w)
      (let ((conn (endpoint-connector c endpoint)))
	;; make sure we have a bus to connect to
	(if (null b)
	    (setq b (make-instance 'bus :width (width conn))))

	;; connect the wires
	(connect-pins conn b)))))


(defgeneric connect-component (c)
  (:documentation "Internally wire C according to its wiring diagram.

This function is called automatically by `make-instance' when a component
is instanciated, and wires-up any class-wide wiring diagram. It can be
specialised to add more behaviours."))


;; TODO we should sanity-check the wiring disagram at compile time, from
;; within metacomponent.

(defmethod connect-component ((c component))
  (let* ((cl (class-of c))
	 (diagram (wiring-diagram cl))
	 (wires (ensure-wiring-diagram cl diagram)))
    (dolist (wire wires)
      (connect-wire c wire))))
