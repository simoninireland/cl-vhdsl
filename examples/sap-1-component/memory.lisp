;; Memory component
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

(in-package :cl-vhdsl/examples/sap-1-component)


(defclass memory (component clocked resetable rw)
  ((width
    :documentation "Width of each memory element."
    :initform 8
    :as :parameter
    :reader width)
   (depth
    :documentation "Number of  memory elements."
    :initform 16
    :as :parameter
    :reader depth)
   (image
    :documentation "The program image file name."
    :as :parameter
    :reader image-file-name)

   ;; pin interface
   (bus
    :documentation "The memory data bus."
    :exported t
    :width 8
    :as :wire
    :reader data-bus)
   (out
    :documentation "The memory output bus."
    :exported t
    :width 8
    :as :wire
    :writer output-bus)

   ;; internal state
   (mar
    :documentation "The memory address register MAR."
    :width 8
    :as :register
    :reader mar)
   (memory
    :documentation "The memory array."
    :accessor memory))
  (:documentation "The memory.

The memory of the SAP-1 is read-only."))


(defmethod on-reset ((mem memory))
  `(setf (mar mem) 0))


(defmethod on-writing ((mem memory))
  `(setf (mar mem) (bits (bus mem) 3)))


(defmethod setup ((mem memory))
  `(progn
     (setf (memory mem)
	   (make-array (list (depth mem))
		       :element-type `(fixed-width-unsigned ,(width mem))
		       :initial-contents (:file (image-file-name mem))))
     (setf (out mem) (aref (memory mem) (mar mem)))))
