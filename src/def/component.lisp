;; Base class for defining synthesisable components
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


(defclass component ()
  ((name
    :documentation "The readable name of the component."
    :initarg :name
    :reader name))
  (:metaclass synthesisable-component)
  (:documentation "A component in an architecture.

Components encapsulate functions and offer a pin-based interface."))


(defgeneric on-start (c)
  (:method-combination append)
  (:documentation "Behaviour on start-up.

This behaviour happens at initialisation of the component.
It typically involves permanent assignments of pins to
other pins, registers, fixed values, or calculations
performed on registers and constants.

The behaviour should be given in RTLisp."))
