;; Register component
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


(defclass register (component clocked enabled rw resetable)
  ((width
    :documentation "The width of the register and its buses."
    :as :parameter
    :initform 8
    :initarg :width
    :reader width)

   ;; state
   (v
    :documentation "The register's current value."
    :width width
    :as :register
    :initform 0
    :accessor value)

   ;; pin interface
   (bus
    :documentation "The data bus."
    :exported t
    :as :wire
    :width width
    :reader bus)
   (out
    :documentation "The output bus."
    :exported t
    :as :wire
    :width width
    :writer out))
  (:wiring (out v))
  (:documentation "A register component.

Registers connect to both data and ALU buses, with the latter
receiving the stored value constantly. The register loads the value on
the data bus on the rising clock edge when it is enabled and set to
writeable; it places its value on the data bus when readable. A
(synchronous) reset sets the stored value to zero.

The register's width is set by its WIDTH parameter, which sets the
widths of the register itself and both buses.")
  (:metaclass synthesisable-component))


(defmethod on-reset ((reg register))
  `(setf (value reg) 0))


(defmethod on-writing ((reg register))
  `(setf (value reg) (bus reg)))


(defmethod on-reading ((reg register))
  `(setf (bus reg) (value reg)))
