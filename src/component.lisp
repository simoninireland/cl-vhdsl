;; component.lisp: Component circuits and sub-systems
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

;; ---------- Components ----------

(defclass component ()
  ((pins :initform '()
	 :reader component-pins))
  (:documentation "A logical behaviour with a pin-based interface."))

(defun make-component (&key width)
  (make-instance 'component :width width))

(defgeneric component-add-pin (c n p)
  (:documentation "Add pin P named N to component C."))

(defmethod component-add-pin ((c component) n p)
  (if (member n (component-pins c))
      (error "Component ~S already has a pin ~S" c n))
  (setf (slot-value c 'pins)
	(append (slot-value c 'pins) (list n p))))

(defgeneric component-pin (c n)
  (:documentation "Return pin N from component C."))

(defmethod component-pin ((c component) n)
  (let ((p (member n (component-pins c))))
    (if p
	(cadr p)
	(error "Component ~S has no pin ~S" c n))))
