;; Micro-instructions
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

;; ---------- Micro-instructions ----------

(defclass microinstruction ()
  ((component
    :documentation "The component this microinstruction controls."
    :initarg :component))
  (:documentation "Base class for micro-instructions.

This class isn't used directly. It serves as the base class for
micro-instructions created for specific systems, usually using
the `defmicroinstructions' macro."))


;; ---------- Connecting to the control pin interface ----------

(defun control-pin-interface (cl)
  "Return the control pin interface slots of component class CL."
  (pin-slots-for-roles cl (list :control)))


(defun control-pin-endpoints (cl cslot)
  "Return the control pin endpoints for sub-component CSLOT of class CL."
  (let* ((sctype (subcomponent-type cl cslot))
	 (control-pin-slots (control-pin-interface sctype)))
    (map-product #'list (list cslot) control-pin-slots)))


(defun subcomponent-control-pin-endpoints (cl)
  "Return the control pin endpoints for the sub-components of component class CL.

Each endpoint is a two-element lists consisting of the sub-component
slot and the control pin slot on that sub-component."
  (let ((cslots (subcomponent-interface cl)))
    (apply #'append (mapcar #'(lambda (cslot)
				(control-pin-endpoints cl cslot))
			    cslots))))


;; ---------- Macro interface ----------

(defun declare-mi-pin-slot (name)
  "Return the code declaring a microinstriction's pin slot."
  )

(defun declare-mi-class (cl mi-pin-slot-names)
  "Return the code declaring the micro-instruction class named CL.

MI-PIN-SLOT-NAMES provides the slots to include."
  (let ((mi-pin-slot-decls (mapcar #'declare-mi-pin-slot mi-pin-slot-names)))
    `(defclass ,cl (microinstruction)
       (,@mi-pin-slot-decls)
       (:documentation "Micro-instructions controlling a system."))))


(defun declare-mi-initialize-instance (cl mi-pin-slot-names)
  "Return the code defining `initialize-instance' for the micro-instruction CL."
  (with-gensyms (mi initargs)
    (let ((mi-pin-slot-initargs (mapcar #'(lambda (mi-slot-name)
					    (declare-mi-slot-initarg mi mi-slot-name))
					mi-pin-slot-names)))
      `(defmethod initialize-instance :after ((,mi ,cl)
					      &rest ,initargs
					      &key ,@mi-pin-slot-names
					      &allow-other-keys)
	 (declare (ignore ,initargs))

	 ,@mi-pin-slot-initargs))))


(defmacro defmicroinstruction (cl clo)
  "Define the class CL of micro-instructions for controlling component class CLO."
  (let* ((endpoints (subcomponent-control-pin-endpoints (find-class clo)))
	 (mi-pin-slot-names (flatten (mapcar #'cadr endpoints)))
	 (mi-class (declare-mi-class cl mi-pin-slot-names))
	 (mi-initialize-instance (declare-mi-initialize-instance cl
								 mi-pin-slot-names)))

    `(progn
       ;; define the micro-instruction class
       ,mi-class

       ;; define the initialiser for instances
       ,mi-initialize-instance
	)))
