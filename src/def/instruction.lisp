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
(named-readtables:in-readtable :interpol-syntax)


;; ---------- Addressing modes ----------

(defclass addressing-mode ()
  ()
  (:documentation "A description of an addressing mode."))


(defgeneric addressing-mode-regexp (cls)
  (:documentation "Return the regexp used to represent data in addressin mode CLS."))


(defmethod addressing-mode-regexp ((ins addressing-mode))
  (addressing-mode-regexp (class-name (class-of ins))))


(defgeneric addressing-mode-bytes (mode)
  (:documentation "Return the bytes that encode the address in MODE."))


(defun list-of-addressing-modes-p (modes)
  "Helper predicate to check a list of addressing mode types (not instances)."
  (and (consp modes)
       (every #'(lambda (mode) (subtypep mode 'addressing-mode)) modes)))


(deftype list-of-addressing-modes ()
  "The type of lists of addressing modes"
  `(satisfies list-of-addressing-modes-p))


;; ---------- Assembler patterns look-up ----------

(defun assembler-make-addressing-modes-regexp (clns)
  "Convert a list of class names CLNS to a regexp that recognises their address modes."
  (flet ((addressing-mode-re (cln)
	   (let ((mn (addressing-mode-regexp cln)))
	     #?"(${mn})")))
    (let ((pats (mapcar #'addressing-mode-re clns)))
      (format nil "^(?:~{~a~^|~})$" pats))))


(defun assembler-get-addressing-mode (s clns &optional re)
  "Return the class from the list CLNS implementing the addressing mode in S.

If RE is provided it should be a regexp constructed by
`assembler-make-addressing-modes-regexp' which will be used for the matching.
This will not be checked against the list of classes. This is an optimisation
to allow the regexp to be re-used across multiple instructions, rather
than being re-created."
  (when (null re)
    (setq re (assembler-make-addressing-modes-regexp clns)))
  (multiple-value-bind (suc matches) (scan-to-strings re s)
    (when suc
      (let ((i (index-non-nil matches)))
	(elt clns i)))))


;; ---------- Instructions ----------

(defclass instruction ()
  ((mode
    :documentation "The addressing mode (arguments) of the instruction."
    :type addressing-mode
    :initarg :addressing-mode
    :reader instruction-addressing-mode))
  (:documentation "An assembly language instruction."))


(defgeneric instruction-mnemonic (cls)
  (:documentation "Return the mnemonic associated with an instruction class."))


(defgeneric instruction-addressing-modes (cls)
  (:documentation "Return the list of addrssing mode classes associated with an instruction class."))


(defmethod instruction-mnemonic ((ins instruction))
  (instruction-mnemonic (class-name (class-of ins))))


(defmethod instruction-addressing-mode ((ins instruction))
  (instruction-addressing-mode (class-name (class-of ins))))


(defgeneric instruction-opcode (ins)
  (:documentation "The opcode bytes for the INS.

For most processors an opcode is a single byte; some use
multi-byte instructions. This method can return either
a byte or a list of bytes, typically making use of the
addressing mode to constrct the bit pattern."))


(defgeneric instruction-bytes (ins)
  (:documentation "The bytes for INS.

The bytes comprise the opcode plus the addressing mode in binary."))


;; ---------- Instruction lookup ----------

(defun assembler-make-mnemonic-regexp (clns)
  "Convert a list of class names CLNS to a regexp that recognises their mnemonics."
  (flet ((mnemonic-re (cln)
	   (let ((mn (instruction-mnemonic cln)))
	     #?"(${mn})")))
    (let ((pats (mapcar #'mnemonic-re clns)))
      (format nil "^(?:~{~a~^|~})$" pats))))


(defun assembler-get-mnemonic (s clns &optional re)
  "Return the class from the list CLNS implementing the mnemonic in S.

If RE is provided it should be a regexp constructed by
`assembler-make-mnemonic-regexp' which will be used for the matching.
This will not be checked against the list of classes. This is an optimisation
to allow the regexp to be re-used across multiple instructions, rather
than being re-created."
  (when (null re)
    (setq re (assembler-make-mnemonic-regexp clns)))
  (multiple-value-bind (suc matches) (scan-to-strings re s)
    (when suc
      (let ((i (index-non-nil matches)))
	(elt clns i)))))


;; ---------- Default methods ----------

(defmethod instruction-bytes ((ins instruction))
  (let ((opcode (instruction-opcode ins))
	(bytes (addressing-mode-bytes (instruction-addressing-mode ins))))
    (cond ((consp opcode)
	   (append opcode bytes))
	  (t
	   (cons opcode bytes)))))


;; ---------- Macro interface ----------
