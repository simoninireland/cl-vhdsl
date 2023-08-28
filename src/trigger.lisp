;; trigger.lisp: Pins with behaviour
;;
;; Copyright (C) 2023 Simon Dobson
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

(in-package :cl-vhdsl)

;; ---------- Triggers (pins with behaviour) ----------

(defclass trigger (pin)
  ((active :initarg :active
	   :initform 0)
   (behaviour :initarg :does
	      :initarg nil))
  (:documentation "A pin triggered on an edge."))


;; Interface
(defgeneric trigger-set-behaviour (trigger f)
  (:documentation "Set trigger TRIG to fire behaviour F."))

(defgeneric trigger-run-behaviour (trig)
  (:documentation "Run any behaviour associated with trigger TRIG."))


;; Implementation
(defmethod trigger-set-behaviour ((trig trigger) f)
  (setf (slot-value trig 'behaviour) f))

(defmethod trigger-run-behaviour ((trig trigger))
  (let ((f (slot-value trig 'behaviour)))
    (when f
      (funcall f trig))))

(defmethod set-pin-value :after ((trig trigger) v)
  (if (equal (slot-value trig 'active) v)
      (trigger-run-behaviour trig)))


;; Constructors
(defun make-active-low-trigger (&key does)
  "Make an active-low trigger"
  (make-instance 'trigger :active 0 :does does))

(defun make-active-high-trigger (&key does)
  "Make an active-high trigger"
  (make-instance 'trigger :active 1 :does does))
