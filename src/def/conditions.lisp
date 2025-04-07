;; Conditions
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


;; ---------- Instructions ----------

(define-condition slot-type-mismatch (vhdsl-condition)
  ((slot
    :documentation "The sub-component slot name."
    :initarg :slot
    :reader slot-name)
   (ty
    :documentation "The type."
    :initarg :type
    :reader slot-type))
   (:report (lambda (c str)
	     (format-condition-context (format nil "Can't give slot ~a type ~a in instruction"
					       (slot-name c)
					       (slot-type c))
				       c str))
  (:documentation "Condition signalled when a slot does not have a proper type.

Slots need to be given fixed width types.")))


;; ---------- Components and sub-components ----------

(define-condition subcomponent-mismatch (vhdsl-condition)
  ((component
    :documentation "The component."
    :initarg :component
    :reader component)
   (slot
    :documentation "The sub-component slot name."
    :initarg :slot
    :reader slot-name))
  (:report (lambda (c str)
	     (format-condition-context (format nil "Can't instanciate sub-component ~a on ~a"
					       (component c)
					       (slot-name c))
				       c str)))
  (:documentation "Condition signalled when a sub-component can't be instanciated.

This is usually because there is no type provided for the
sub-component and no explicit instance given; it can also be because
the type given doesn't refer to a COMPONENT sub-class."))
