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

;; Create a hidden base metaclass wose "slots" become options
;; available for slots in the final class
(def-extra-options-metaclass premetacomponent ()
  ((pins
    :type integer)
   (role
    :type symbol)))


(defclass metacomponent (premetacomponent)
  ((pin-interface-slots
    :documentation "The slots forming the component's pin interface.")
   (wiring
    :documentation "The wiring diagram."
    :initarg :wiring
    :initform nil
    :reader wiring-diagram))
  (:documentation "The metaclass of components.

The metaclass introduces two new slot arguments, `:pins' and
`:role', to mark slots as part of the pin interface. These
markers are then picked up when an object of the class is
instanciated.

`:pins' defines the number of pins in the slot. It should be one of:

- an integer
- a symbol identifying another slot on the same object that
  contains the width
- T, to denote a slot with undetermined width

If the width is set to the value of a nother slot, this value
is treated as a constant and assigned to :pins for later queries.

`:role' defines the intended use of the pins in the slot, and can'tbe
any role supported by `configure-pin-for-role'

If there are any `:initform' or :`initargs' for pin interface slots,
they should contain the wire or wires that the slot's pins should
be connected to.

If wires are provided and the `:pins' value is set to T, the number
of wires will be taken as the width of the slot.

Classes with this metaclass can include an argument `:wiring'
that specifies a wiring diagram for instances of the class. They
will also support the `pin-interface' function that extracts
the slots in their pin interface as defined by the slot options."))


;; TODO There's more work to do here in deciding how slots can be
;; re-defined or combined.

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

      ;; no pin slot of that name, either ignored or fatal
      (if fatal
	  (error "No slot named ~a in pin interface of ~a"
		 slot cl)))))


(defun slot-def-in-pin-interface-p (slot-def)
  "Test whether SLOT-DEF defines a slot composed of pins.

This tests for the existence of the :pins slot option in the
slot definition."
  (slot-exists-and-bound-p slot-def 'pins))


(defgeneric pin-interface (c)
  (:documentation "Return the list of slots in the pin interface of C,")
  (:method ((cl metacomponent))
    (slot-value cl 'pin-interface-slots)))


(defmethod finalize-inheritance :after ((class metacomponent))
  ;; populate the class' pin interface cache
  (let* ((slot-defs (class-slots class))
	 (pin-slot-defs (remove-if-not #'slot-def-in-pin-interface-p slot-defs)))
    (setf (slot-value class 'pin-interface-slots)
	  (mapcar #'slot-definition-name pin-slot-defs))))


;; ---------- Enabler helper ----------

(defun call-methods (methods)
  "Return `call-method' forms for all METHODS."
  (mapcar #'(lambda (m)
	      `(call-method ,m))
	  methods))


(define-method-combination guarded (&optional (order :most-specific-first))
  ((arounds (:around))
   (ifs (:if))
   (befores (:before))
   (primaries () :order order :required t)
   (afters (:after)))
    "A method combination that adds `:if' methods to guard a generic function.

When present, `:if' methods are run first to test whether the other
methods in the method stack are run. In other words they behave a
little like `:around' methods, with the difference that `:if' methods
always run first wherever they appear in the effective method stack.
All `:if' methods must return non-nil for the \"functional\" parts of
the method to be executed.

Schematically the method ordering is:

   (if (and ifs)
     (arounds
	(befores)
	(primaries)
	(afters)))

The order of execution for `:if' methods is undefined. However, they
can have side-effects as long as these can happen in any order. One
use case is for a guard to prevent a call to the target method but
redirect the call to another, which is fine as long as there's no
other guard that might inhibit it."

  (let* ((before-form (call-methods befores))
	 (after-form (call-methods afters))
	 (primary-form `(call-method ,(car primaries) ,(cdr primaries)))
	 (core-form (if (or befores afters (cdr primaries))
			`(prog1
			     (progn
			       ,@before-form
			       ,primary-form)
			   ,@after-form)

			;; optimisation for no :before or :after methods and
			;; only one primary method
			`(call-method ,(car primaries))))
	 (around-form (if arounds
			  `(call-method ,(car arounds)
					(,@(cdr arounds)
					 (make-method ,core-form)))

			  ;; optimisation for no :around methods
			  core-form)))

    (if ifs
	`(if (and ,@(call-methods ifs))
	     ,around-form)

	;; optimisation for no :if methods
	around-form)))
