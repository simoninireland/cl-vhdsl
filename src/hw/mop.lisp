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

(def-extra-options-metaclass metacomponent ()
  ((pins
    :type integer)
   (role
    :type symbol)))


(defgeneric make-pin-for-role (role)
  (:documentation "Create a pin suitable for ROLE.

Methods can specialise to provide appropriate pins for new roles."))


(defmethod make-pin-for-role ((role (eql ':io)))
  (make-instance 'pin :state :tristate))
(defmethod make-pin-for-role ((role (eql ':control)))
  (make-instance 'pin :state :reading))
(defmethod make-pin-for-role ((role (eql ':trigger)))
  (make-instance 'pin :state :trigger))


(defun slot-in-pin-interface (slot-def)
  "Test whether SLOT-DEF defines a slot composed of pins.

This tests for the existence of the :pins slot option in the
slot definition."
  (slot-exists-and-bound-p slot-def 'pins))


;; When the class is instanciated, look at all the slots in their
;; pin interface and create to appropriate pins for them. Other
;; (non-pin) slots are left alone.

(defmethod make-instance ((cl metacomponent) &rest initargs)
  (declare (ignore initargs))

  (let ((slot-defs (mappend #'class-slots (class-precedence-list cl)))
	(c (call-next-method)))
    (dolist (slot-def slot-defs)
      (when (slot-in-pin-interface slot-def)
	;; slot is in the pin interface
	(when (or (slot-definition-initform slot-def)
		  (slot-definition-initfunction slot-def))
	  ;; slot shouldn't be initialised
	  (error (make-instance 'initialised-pin-interface-slot
				:class cl
				:name (slot-definition-name slot-def))))

	;; create the pins
	(let* ((slot (slot-definition-name slot-def))
	       (width (slot-value slot-def 'pins))
	       (role (or (and (slot-exists-and-bound-p slot-def 'role)
			      (slot-value slot-def 'role))
			 :io)))	      ;; role defaults to :io
	  (setf (slot-value c slot)
		(if (= width 1)
		    ;; slot gets a single pin
		    (make-pin-for-role role)

		    ;; slots gets a vector of pins
		    (map 'vector
			 #'(lambda (i)
			     (declare (ignore i))
			     (make-pin-for-role role))
			 (iota width)))))))

    ;; return the now-modified instance
    c))
