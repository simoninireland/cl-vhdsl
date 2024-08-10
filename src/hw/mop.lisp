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


(defmethod compute-effective-slot-definition ((cl metacomponent) slot slot-defs)
  (let ((slot-def (call-next-method)))
    (when (and (slot-def-in-pin-interface-p slot-def)
	       (not (slot-exists-and-bound-p slot-def 'role)))
      ;; fill in default :role if it is missing
      (setf (slot-value slot-def 'role) :io))

    ;; return the slot definition
    slot-def))


(defmethod validate-superclass ((cl standard-class) (super metacomponent))
  t)


;; ---------- Pin roles ----------

(defgeneric configure-pin-for-role (pin role)
  (:documentation "Set up PIN suitable for ROLE.

Methods can specialise to configure pins appropriatly for new roles."))


(defmethod configure-pin-for-role (pin (role (eql :io)))
  (setf (pin-state pin) :tristate))
(defmethod configure-pin-for-role (pin (role (eql :control)))
  (setf (pin-state pin) :reading))
(defmethod configure-pin-for-role (pin (role (eql :trigger)))
 (setf (pin-state pin) :trigger))


;; ---------- Query pin interface ----------

(defun find-pin-slot-def (cl slot &key fatal)
  "Return the slot definition for SLOT in CL.

If FATAL is set to T an error is raised if SLOT does
not exist or isn't part of the pin interface."
  (flet ((pin-slot-named (slot-def)
	   (and (slot-def-in-pin-interface-p slot-def)
		(equal (slot-definition-name slot-def) slot))))
    (if-let ((slot-defs (remove-if-not #'pin-slot-named
				       (class-slots cl))))
      (car slot-defs)

      (if fatal
	  (error "No slot named ~s in pin interface of ~s"
		 slot cl)))))


(defun slot-def-in-pin-interface-p (slot-def)
  "Test whether SLOT-DEF defines a slot composed of pins.

This tests for the existence of the :pins slot option in the
slot definition."
  (slot-exists-and-bound-p slot-def 'pins))


(defun pin-interface (cl)
  "Return a list of all the slots of class CL comprising its pin interface."
  (let ((slot-defs (remove-if-not #'slot-def-in-pin-interface-p
				  (class-slots cl))))
    (mapcar #'slot-definition-name slot-defs)))


(defun pin-interface-p (cl slot)
  "Test whether SLOT is in the pin interface of class CL.

Returns nil if SLOT either isn't a slot in CL's pin
interface, or isn't a slot of CL at all."
  (not (null (find-pin-slot-def cl slot))))


(defun pin-role-for-slot (cl slot)
  "Return the role assigned to the pins of SLOT in class CL.

SLOT must be in C's pin interface."
  (let ((slot-def (find-pin-slot-def cl slot :fatal t)))
    (slot-value slot-def 'role)))


(defun pin-slots-for-roles (cl roles)
  "Extract all the slots in the pin interface of CL having roles in ROLES."
  (remove-if-not #'(lambda (slot)
		     (member (pin-role-for-slot cl slot) roles))
		 (pin-interface cl)))
