;; Software-emulted control units
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


;; ---------- Nano-instructions ----------

(defclass nanoinstruction ()
  ((component
    :documentation "The component this nano-instruction controls."
    :initarg :component
    :reader nanoinstruction-component))
  (:documentation "A control word for a component."))


(defun pin-interface-for-roles (c roles)
  "Return all the slots in the pin interface of C with one of the given ROLES."
  (remove-if-not #'(lambda (slot)
		     (not (member (pin-role-for-slot c slot) roles)))
		 (pin-interface c)))


(defmethod initialize-instance :after ((ins nanoinstruction) &rest initargs)
  (declare (ignore initargs))

  (let ((control-slots (pin-interface-with-roles (nanoinstruction-component ins)
						 '(:control))))



    )

  )




;; ---------- Micro-instructions ----------


;; ---------- Control units ----------


(defclass control (component clocked)
  ()
  (:documentation "A control unit.

Control units emit micro-instructions to control the rest of the system.
The micro-instuctions are themselves composed of nano-instructions that
define the states of control lines for components, one nano-instruction
per component.

The control unit itself is a finite-state machine that emits micro-instructions
when triggered by the clock.
"))
