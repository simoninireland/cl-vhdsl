;; 6502 architecture definition
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

(in-package :cl-vhdsl/systems/6502)

(defclass 6502-architecture (architecture)
  ((A
    :documentation "Accumulator."
    :type data-register
    :initform (make-instance 'data-register :name "A" :width 8)
    :accessor A)
   (X
    :documentation "Index register X."
    :type index-register
    :initform (make-instance 'index-register :name "X" :width 8)
    :accessor X)
   (Y
    :documentation "Index register Y."
    :type index-register
    :initform (make-instance 'index-register :name "Y" :width 8)
    :accessor Y)
   (PC
    :documentation "Program counter."
    :type address-register
    :initform (make-instance 'address-register :name "PC" :width 16)
    :accessor PC)
   (SP
    :documentation "Stack pointer."
    :type index-register
    :initform (make-instance 'index-register :name "SP" :width 8)
    :accessor SP)
   (F
    :documentation "Flags."
    :type special-register
    :initform (make-instance 'special-register :name "F" :width 8)
    :accessor F)

   (C
    :documentation "Carry flag."
    :initform (make-instance 'flag
			     :name "C" :register F :bit 0)
    :accessor C)
   (Z
    :documentation "Zero flag."
    :initform (make-instance 'flag
			     :name "Z" :register F :bit 1)
    :accessor Z))
  (:documentation "Architectural description of a 6502."))
