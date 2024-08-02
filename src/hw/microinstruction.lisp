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

;; ---------- Micro-instrutions ----------

(defclass microinstruction ()
  ((system
    :documentation "The system this metainstruction controls."
    :initarg :system)
   (value
    :documentation "The value of the micro-instruction, as a bit field."
    :initform 0
    :accessor microinstruction-bitfield)
   (mi-bus
    :documentation "The control bus on whch micro-instructions are issued."
    :initarg :bus
    :reader microinstruction-bus))
  (:documentation "Base class for micro-instructions.

This class isn't used directly. It serves as the base class for
micro-instructions created for specific systems, usually using
ther `defmicroinstructions' macro."))


(defgeneric run-microinstruction (mi)
  (:documentation "Run the micro-instruction MI against its system.

This places the value of the micro-instruction onto the bus."))


(defmethod run-microinstruction ((mi microinstruction))
  (pins-from-value (microinstruction-bus mi) (microinstruction-bitfield mi)))


;; ---------- The sub-classes constructed for specific systems ----------

(defun component-slot (slot-def)
  "Test whether SLOT-DEF is a slot holding a component.

Component slots have a :type attribute that holds a class that is
a sub-type of `component'.

Returns the type of the component, or nil."
  (if-let ((type (slot-definition-type slot-def)))
    (if (subtypep type 'component)
	;; return the class associated with the type
	(find-class type))))


(defun mi-slot-name-for-slot (pin-slot component-slot-def)
  "Compute the slot name used for a micro-instruction for PIN-SLOT on CL.

The name is constructed for PIN-SLOT on the component held within
COMPONENT-SLOT-DEF. The slot name is interned into the same package
as COMPONENT-SLOT-DEFs name to ensure it's accessed from the same scope."
  (let* ((component-symbol (slot-definition-name component-slot-def))

	 ;; create the name for the new slot
	 (component-name (symbol-name component-symbol))
	 (pin-slot-name (symbol-name pin-slot))
	 (mi-slot-name (str:concat component-name "/" pin-slot-name))

	 ;; create the new slot's name as a symbol interned into the correct package
	 (component-package (symbol-package component-symbol))
	 (mi-slot (intern mi-slot-name component-package)))
    mi-slot))


(defun mi-pin-slots (cl)
  "Return the slot definitions for the micro-instruction pin interface of CL."
  (let ((slot-defs (class-slots cl))
	nano-slots)
    (dolist (slot-def slot-defs)
      ;; construct a new slot per pin interface slot in an appropriate role
      (when-let* ((type (component-slot slot-def)))
	(let ((pin-slots (pin-slots-for-roles type (list :control))))
	  (dolist (pin-slot pin-slots)
	    (let ((mi-slot-name (mi-slot-name-for-slot pin-slot slot-def)))
	      (setf nano-slots (cons mi-slot-name nano-slots)))))))

    ;; return the constructed slots
    nano-slots))


;; ---------- Macro interface ----------

(defun declare-mi-pin-slot (mi-pin-slot-name)
  "Return the code to declare the MI-PIN-SLOT-NAME in the micro-instruction."

  ;; we need to intern the symbol name into th keywords package
  ;; for use an the initial argument
  (let ((mi-pin-slot-keyword (intern (symbol-name mi-pin-slot-name) "KEYWORD")))
    `(,mi-pin-slot-name
      :initarg ,mi-pin-slot-keyword)))


(defun declare-mi-slot-initarg (mi mi-slot-name)
  "Return the code to install the value of MI-SLOT-NAME.

The value is assumed to be passed using the key MI-SLOT-NAME
and is assigned to the object MI, which should be a symbol."
  `(setf (slot-value ,mi ',mi-slot-name) ,mi-slot-name))


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


(defmacro defmicroinstruction (cl (system) &body body)
  "Define the class CL of micro-instructions for controlling SYSTEM."
  (declare (ignore body))

  (let* ((mi-pin-slot-names (mi-pin-slots (find-class system)))
	 (mi-class (declare-mi-class cl mi-pin-slot-names))
	 (mi-initialize-instance (declare-mi-initialize-instance cl
								 mi-pin-slot-names)))

    `(progn
       ;; define the micro-instruction class
       ,mi-class

       ;; define the initialiser for instances
       ,mi-initialize-instance
	)))
