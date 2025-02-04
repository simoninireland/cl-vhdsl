;; Metaclass for defining pin-oriented interfaces
;;
;; Copyright (C) 2024--2025 Simon Dobson
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

(in-package :cl-vhdsl/def)

;; ---------- The metaclass of synthesisable components ----------

;; Create a hidden base metaclass whose "slots" become options
;; available for slots in the final class
(def-extra-options-metaclass pre-synthesisable-component ()
  ((width
    :type integer)
   (exported
    :type boolean)
   (parameter
    :type boolean)
   (as
    :type symbol)
   (direction
    :type symbol)
   (role
    :type symbol)))


;; The slots of a synthesisable-component take extra attributes:
;;
;; - :width -- the width in bits
;; - :as -- the representation, one of :register, :wire, :value, or :parameter
;; - :direction -- dataflow direction, one of :in, :out or :inout
;; - :role -- the role fulfilled by the slot, one of :io, :control; or :trigger
;; - :exported -- whether the slot is available from outside the component
;;
;; They also interpret some standard attributes:
;;
;; - :type -- the type held in the slot
;; - :initarg -- the key used in the constructor
;; - :initform -- the initial value
;; - :reader, :writer, or :accessor -- access methods
;; - :documentation -- the docstring

(defclass synthesisable-component (pre-synthesisable-component)
  ((pin-interface-slots
    :documentation "The slots forming the component's pin interface.

These slots are all marked as :exported in the component."
    :accessor pin-interface-slots)
   (subcomponent-slots
    :documentation "The slots containing sub-components.

These slots all have types that are sub-classes of COMPONENT."
    :accessor subcomponent-slots)
   (parameter-slots
    :documentation "The parameters of the component.

These slots are represented as :parameter in the component."
    :accessor parameter-slots)
   (variable-slots
    :documentation "The local variables of the component."
    :accessor variable-slots)
   (wiring
    :documentation "The wiring diagram."
    :initarg :wiring
    :initform nil
    :reader wiring-diagram))
  (:documentation "The metaclass of synthesisable components."))


(defmethod finalize-inheritance :after ((cl synthesisable-component))
  (let* ((slot-defs (class-slots cl))
	 (pin-slot-defs (remove-if-not #'slot-def-in-pin-interface-p
				       slot-defs))
	 (parameter-slot-defs (remove-if-not #'slot-def-in-parameter-interface-p
					     slot-defs))
	 (subcomponent-slot-defs (remove-if-not #'slot-def-in-subcomponents-p
						slot-defs))
	 (variable-slot-defs (remove-if-not #'slot-def-in-variables-p
					    slot-defs)))
    ;; populate the class' pin interface slots
    (setf (slot-value cl 'pin-interface-slots)
	  (mapcar #'slot-definition-name pin-slot-defs))

    ;; populate the class' parameter slots
    (setf (slot-value cl 'parameter-slots)
	  (mapcar #'slot-definition-name parameter-slot-defs))

    ;; populate the class' subcomponent slots
    (setf (slot-value cl 'subcomponent-slots)
	  (mapcar #'slot-definition-name subcomponent-slot-defs))

    ;; populate the class' variable slots
    (setf (slot-value cl 'variable-slots)
	  (mapcar #'slot-definition-name variable-slot-defs))))


;; ---------- Slot definition kinds ----------

(defun slot-def-in-pin-interface-p (slot-def)
  "Test whether SLOT-DEF defines a slot available outside the component.

This tests whether the slot is marked as :exported."
  (and (slot-exists-and-bound-p slot-def 'exported)
       (slot-value slot-def 'exported)))


(defun slot-def-in-parameter-interface-p (slot-def)
  "Test whether SLOT-DEF defines a parameter to the component..

This tests whether the slot' representation is :parameter."
  (and (slot-exists-and-bound-p slot-def 'as)
       (eql (slot-value slot-def 'as) :parameter)))


(defun slot-def-in-subcomponents-p (slot-def)
  "Test whether SLOT-DEF defines a sub-component.

Sub-components are simply slots that have a type that is
a sub-class of `component'."
  (subtypep (slot-definition-type slot-def)
	    (find-class 'synthesisable-component)))


(defun slot-def-in-variables-p (slot-def)
  "Test whether SLOT-DEF defines a slot available outside the component.

This tests whether the slot's representation is not :value
or :parameter, and not exported."
  (and
   ;; slot is not exported
   (or (and (slot-exists-and-bound-p slot-def 'exported)
	    (not (slot-value slot-def 'exported)))
       (not (slot-exists-and-bound-p slot-def 'exported)))

   ;; slot has a representation, but not :value ;parameter
   (or (and (slot-exists-and-bound-p slot-def 'as)
	    (and (not (member (slot-value slot-def 'as)
			      '(:value :parameter)))))
       (not (slot-exists-and-bound-p slot-def 'as)))))


;; ---------- Interfaces ----------

(defgeneric pin-interface (cl)
  (:documentation "Return the list of slots in the pin interface of CL.")
  (:method ((cl synthesisable-component))
    (slot-value cl 'pin-interface-slots)))


(defgeneric parameters (cl)
  (:documentation "Return the list of slots that hold parameters of CL.")
  (:method ((cl synthesisable-component))
    (slot-value cl 'parameter-slots)))


(defgeneric subcomponents (cl)
  (:documentation "Return the list of slots of CL holding sub-components.")
  (:method ((cl synthesisable-component))
    (slot-value cl 'subcomponent-slots)))


(defgeneric variables (cl)
  (:documentation "Return the list of slots of CL that are variables.")
  (:method ((cl synthesisable-component))
    (slot-value cl 'variable-slots)))


;; ---------- Slot attributes ----------

(defun find-slot-attribute-in-slot-def (slot-def attr)
  "Return the value of ATTR in SLOT-DEF if it is bound."
  (if (slot-boundp slot-def attr)
      (slot-value slot-def attr)))


(defun find-slot-def-in-class (cl slot)
  "Return the slot definition associated with SLOT on class CL.

This is used internally to access slot attributes."
  (let ((slot-defs (class-slots cl)))
    (or (find slot slot-defs :key #'slot-definition-name
			     :test #'string-equal)
	(error "No slot ~a in ~a " slot cl))))


(defun find-slot-attribute-in-class (cl slot attr)
  "Return the attribute ATTR of SLOT in class CL."
  (let ((slot-def (find-slot-def-in-class cl slot)))
    (find-slot-atttribte-in-slot-def slot-def attr)))


(defun slot-attribute (c slot attr)
  "Return the value of ATTR for SLOT in the slots of class or component C."
  (let ((cl (if (eql (class-of c)
		     'synthesisable-component)
		c
		(class-of c))))
    (find-slot-attribute-in-slot-def (find-slot-def-in-class cl slot) attr)))


(defun slot-width (c slot)
  "Return the width of SLOT in class or component C."
  (slot-attribute c slot 'width))


(defun slot-representation (c slot)
  "Return the representation of SLOT in class or component C."
  (slot-attribute c slot 'as))


(defun slot-direction (c slot)
  "Return the direction of SLOT in class or component C."
  (slot-attribute c slot 'direction))
