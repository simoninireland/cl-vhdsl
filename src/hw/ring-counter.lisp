;; Software-emulated ring counter
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


(defclass ring-counter (enabled component clocked)
  ((width
    :documentation "The width of the ring counter."
    :initarg :width
    :reader width)
   (count
    :documentation "The current count."
    :initform 0
    :accessor ring-counter-count)
   (counter-bus
    :documentation "The lines taking the count."
    :initarg :bus
    :pins width
    :role :io))
  (:metaclass metacomponent)
  (:documentation "A ring counter.

This converts a count of clock edges intoan asserted wire on
its counter bus. When reset it asserts line 0; at the next clock pulse
it de-asserts line 0 and asserts line 1; and so on, until its
width is exceeded and it wraps-around back to 0."))


(defmethod initialize-instance :after ((rc ring-counter) &rest initargs)
  (declare (ignore initargs))

  (ring-counter-reset rc))


(defun ring-counter-reset (rc)
  "Reset the counter."
  (setf (ring-counter-count rc) 0))


(defmethod (setf ring-counter-count) (v (rc ring-counter))
  ;; wrap the count if needed
  (if (>= v (width rc))
      (setq v 0)
      (setf (slot-value rc 'count) v))

  ;; update the asserted pin
  (let ((mask (ash 1 v)))
    (setf (pins-value (slot-value rc 'counter-bus)) mask)))


(defmethod pin-triggered ((rc ring-counter) p (v (eql 1)))
  (declare (ignore p))

  ;; increment the count
  (incf (ring-counter-count rc)))
