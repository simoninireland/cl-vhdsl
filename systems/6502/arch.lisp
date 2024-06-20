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

;; ---------- Registers ----------

;; Accumulator
(def-register A :width 8 :documentation "Accumulator")

;; Index registers
(def-register X :width 8 :documentation "Index X")
(def-register Y :width 8 :documentation "Index Y")

;; Administrative registers
(def-register PC :width 16 :documentation "Program counter")
(def-register SP :width 8 :documentation "Stack pointer")
(def-register F :width 8 :documentation "Flags")


;; ---------- Helper functions and types ----------

(defun index-register-p (reg)
  "Test whether REG is an index register."
  (member reg (list X Y)))


(deftype index-register ()
  "The type of index registers."
  `(and register
	(satisfies index-register-p)))


;; ---------- Addressing modes ----------

(defclass immediate (addressing-mode)
  ((value
    :documentation "The value, as an unsigned 8-bit byte."
    :type word-8
    :initarg :value
    :reader immediate-value))
  (:documentation "Immediate addressing, with an inline 8-bit value."))


(defmethod print-address ((mode immediate) stream)
  "Print the value of MODE as a hex literal.

The style of #41H is standard for 6502 assemblers."
  (format stream "#~2,0xH" (immediate-value mode)))


(defclass absolute (addressing-mode)
  ((address
    :documentation "The address, a 16-bit word."
    :type word-16
    :initarg :address
    :reader absolute-address))
  (:documentation "Absolute addressing, with an inline 16-bit address."))


(defmethod print-address ((mode absolute) stream)
  "Print the value of MODE as an absolute address.

The style of 3041H is standard for 6502 assemblers."
  (format stream "~4,0xH" (absolute-address mode)))


(defclass absolute-indexed (absolute)
  ((index
    :documentation "The index register to add to the address."
    :type index-register
    :initarg :indexed-by
    :reader absolute-indexed-index))
  (:documentation "An absolute address plus the contents of an index register"))


(defmethod print-address ((mode absolute-indexed) stream)
  "Print the value of MODE as an address follkowed by the index register."
  (format stream "~4,0xH, ~a" (absolute-address mode) (register-name (absolute-indexed-index mode))))



;; ---------- Instructions ----------

(defvar LDA (make-instance 'instruction
			   :documentation "Load accumulator."
			   :addressing-modes '(immediate direct)))
