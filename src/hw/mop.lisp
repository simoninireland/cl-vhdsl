;; Metaclass for defining pin-oriented interfaces
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

;; ---------- The metaclass of components ----------

;; The metaclass introduces two new slot arguments, :pin and
;; :role, to mark slots as part of the pin interface. These
;; markers are then picked up when an object of the class is
;; instanciated.
;;
;; :pins defined the number of pins in the slot. It should be one of:
;;
;; - an integer
;; - a symbol identifying another slot on the same object that
;;   contains the width
;; - T, to denote a slot with undetermined width
;;
;; If the width is set to the value of a nother slot, this value
;; is treated as a constant and assigned to :pins for later queries.
;;
;; :role defines the intended use of the pins in the slot, and
;; should be one of:
;;
;; - :io for I/O pins
;; - :control for control pins that are set to :reading
;; - :trigger for control trigger pins set to :trigger
;;
;; Further roles can be defined by specialising `make-pin-for-role'
;;
;; If there are any @initform or :initargs for pin interface slots,
;; they should contain the wire or wires that the slot's pins should
;; be connected to.
;;
;; If wires are provided and the :pins value is set to T, the number
;; of wires will be taken as the width of the slot.

(def-extra-options-metaclass metacomponent ()
  ((pins
    :type integer)
   (role
    :type symbol)))


(defgeneric make-pin-for-role (role)
  (:documentation "Create a pin suitable for ROLE.

Methods can specialise to provide appropriate pins for new roles."))


(defmethod make-pin-for-role ((role (eql :io)))
  (make-instance 'pin :state :tristate))
(defmethod make-pin-for-role ((role (eql :control)))
  (make-instance 'pin :state :reading))
(defmethod make-pin-for-role ((role (eql :trigger)))
  (make-instance 'pin :state :trigger))


(defun slot-in-pin-interface (slot-def)
  "Test whether SLOT-DEF defines a slot composed of pins.

This tests for the existence of the :pins slot option in the
slot definition."
  (slot-exists-and-bound-p slot-def 'pins))


;; When the class is instanciated, look at all the slots in their
;; pin interface and create to appropriate pins for them. Other
;; (non-pin) slots are left alone.

(defun normalise-wires (pins-wires-or-bus)
  "Convert PINS-WIRES-OR-BUS into a sequence of wires.

The normalisation process is:

- for a bus, return its wires
- for a single wire, return it in a sequence
- for a single pin, return its wire as a sequence
- for a sequence, normalise each member pin or wire: you
  can't include a bus in the sequence

Any pins provided have to be connected to wire."
  (typecase pins-wires-or-bus
    (bus
     (bus-wires pins-wires-or-bus))

    (wire
     (list pins-wires-or-bus))

    (pin
     (let ((w (pin-wire pins-wires-or-bus)))
       (if (null w)
	   (error "Unconnected pin ~s cannot be used" pins-wires-or-bus)

	   (list (pin-wire pins-wires-or-bus)))))

    (sequence
     (map 'vector (lambda (pin-or-wire)
		    (typecase pin-or-wire
		      (wire
		       pin-or-wire)

		      (pin
		       (let ((w (pin-wire pins-wires-or-bus)))
			 (if (null w)
			     (error "Unconnected pin ~s cannot be used" pins-wires-or-bus)
			     (pin-wire pin-or-wire))))

		      (t
		       (error "Unexpected ~s is not a pin or wire" pin-or-wire))))
	  pins-wires-or-bus))
    (t
     (error "Unexpected ~s is not a pin, wire, bus, or sequence" pins-wires-or-bus))))


(defmethod validate-superclass ((cl standard-class) (super metacomponent))
  t)


;; ---------- Query pin interface ----------

(defun find-pin-slot-def (c slot &key fatal)
  "Return the slot definition for SLOT in C.

If FATAL is set to T an error is raised if SLOT does
not exist or isn't part of the pin interface."
  (flet ((pin-slot-named (slot-def)
	   (and (slot-in-pin-interface slot-def)
		(equal (slot-definition-name slot-def) slot))))
    (if-let ((slot-defs (remove-if-not #'pin-slot-named
				       (class-slots (class-of c)))))
      (car slot-defs)

      (if fatal
	  (error "No slot named ~s in pin interface of ~s"
		 slot c)))))


(defun pin-interface (c)
  "Return a list of all the slots of C comprising its pin interface."
  (let ((slot-defs (remove-if-not #'slot-in-pin-interface
				  (class-slots (class-of c)))))
    (mapcar #'slot-definition-name slot-defs)))


(defun pin-interface-p (c slot)
  "Test whether SLOT is in the pin interface of C.

Returns nil if SLOT either isn't a slot in C's pin
interface, or isn't a slot of C at all."
  (not (null (find-pin-slot-def c slot))))


(defun pins-for-slot (c slot)
  "Return the number of pins on SLOT of C.

SLOT must be in C's pin interface. The default reads the value
of the :pins attribute of SLOT in C's class definition."
  (let ((slot-def (find-pin-slot-def c slot :fatal t)))
    (slot-value slot-def 'pins)))


(defun pin-role-for-slot (c slot)
  "Return the role assigned to the pins of SLOT in C.

SLOT must be in C's pin interface."
  (let ((slot-def (find-pin-slot-def c slot :fatal t)))
    (slot-value slot-def 'role)))


;; ---------- The metaclass of nano-instructions ----------

;; Nano-instructions are constucted from components

;; TBD
