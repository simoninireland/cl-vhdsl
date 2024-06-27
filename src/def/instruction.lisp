;; Instruction definitions
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

(in-package :cl-vhdsl/def)

;; ---------- Addressing modes ----------

(defclass addressing-mode ()
  ()
  (:documentation "A description of an addressing mode."))


(defgeneric addressing-mode-print (mode stream)
  (:documentation "Print the value of the address MODE on STREAM."))


(defgeneric addressing-mode-bytes (mode)
  (:documentation "Return the bytes that encode the address in MODE."))


(defun list-of-addressing-modes-p (modes)
  "Helper predicate to check a list of addressing mode types (not instances)."
  (and (consp modes)
       (every #'(lambda (mode) (subtypep mode 'addressing-mode)) modes)))


(deftype list-of-addressing-modes ()
  "The type of lists of addressing modes"
  `(satisfies list-of-addressing-modes-p))


;; ---------- Instructions ----------

(defclass instruction ()
  ((mode
    :documentation "The addressing mode (arguments) of the instruction."
    :type addressing-mode
    :initarg :addressing-mode
    :reader instruction-addressing-mode))
  (:documentation "An assembly language instruction."))


(defgeneric instruction-mnemonic (ins)
  (:documentation "The mnemonic for INS."))


(defgeneric instruction-addressing-modes (ins)
  (:documentation "The valid addressing modes for INS."))


(defgeneric instruction-opcode (ins)
  (:documentation "The opcode bytes for the INS."))


(defgeneric instruction-bytes (ins)
  (:documentation "Generate the bytes for INS."))


(defgeneric instruction-assemble (ins)
  (:documentation "Return the bytes constructed from assembling INS."))


;; ---------- Default methods ----------

(defmethod instruction-bytes ((ins instruction))
  (let ((opcode (instruction-opcode ins))
	(bytes (addressing-mode-bytes (instruction-addressing-mode ins))))
    (cond ((consp opcode)
	   (append opcode bytes))
	  (t
	   (cons opcode bytes)))))


;; ---------- Macro interface ----------
