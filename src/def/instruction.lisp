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

;; ---------- Instructions ----------

(defclass abstract-instruction ()
  ()
  (:documentation "The base class for 'real' instruction and pseudo-instructions.

Real instructions are those understood by the processor, that can be
converted into machine code. Pseudo-instructions (or directives)
are understood by the assembler and used to provide metadata when
assembling the instruction stream."))


(defgeneric instruction-mnemonic (inscls)
  (:documentation "Return the mnemonic associated with INSCLS.

INSCLS may be a class name (symbol) or an instance of the
`abstract-instruction' class. This applies to both real and
pseudo-instructions, and is used when building an assembler."))


(defmethod instruction-mnemonic ((ins abstract-instruction))
  ;; map instance to its class name
  (instruction-mnemonic (class-name (class-of ins))))


(defclass instruction (abstract-instruction)
  ((mode
    :documentation "The addressing mode (arguments) of the instruction."
    ;;:type addressing-mode
    :initarg :addressing-mode
    :initform nil
    :reader instruction-addressing-mode))
  (:documentation "An assembly language instruction runnable by a core."))


(defgeneric instruction-addressing-modes (inscls)
  (:documentation "Return the list of addressing mode classes for INSCLS.

INSCLS may be a class name (symbol) or an instance of the
`instruction' class."))


(defmethod instruction-addressing-modes ((ins instruction))
  ;; map instance to its class name
  (instruction-addressing-modes (class-name (class-of ins))))


(defgeneric instruction-opcode (ins)
  (:documentation "The opcode bytes for the INS.

For most processors an opcode is a single byte; some use
multi-byte instructions. This method can return either
a byte or a list of bytes, typically making use of the
addressing mode to construct the bit pattern."))


(defgeneric instruction-addressing-mode-bytes (ins)
  (:documentation "Return the bytes representing the addressing mode of INS.

By default this will simply call `addressing-mode-bytes'."))


(defmethod instruction-addressing-mode-bytes ((ins instruction))
  (addressing-mode-bytes (instruction-addressing-mode ins)))


(defgeneric instruction-bytes (ins)
  (:documentation "The bytes for INS.

The bytes comprise the opcode plus the addressing mode in binary.
The default implementatio simply concatenates the results of
`instruction-opcode' and `addressing-mode-bytes'"))


(defmethod instruction-bytes ((ins instruction))
  (let ((opcode (instruction-opcode ins)))
    (if (consp opcode)
	(append opcode (instruction-addressing-mode-bytes ins))
	(cons opcode (instruction-addressing-mode-bytes ins)))))


(defgeneric instruction-check (ins)
  (:documentation "Check that INS is a valid instruction.

This will by default check that the addressing mode is legal, as
defined by `instruction-addressing-modes'. It should be
specialised (generally by :after methods) to provide any further
necessary run-time checks.

The method returns INS unchanged."))


;; Should we force a mode, not assume none is implicit?

(defmethod instruction-check ((ins instruction))
  (let ((mode (instruction-addressing-mode ins))
	(modes (instruction-addressing-modes ins)))
    (if (and (null mode) ;; no addressing mode, implicit addressing
	     (null modes))
	ins
	(if (notany (lambda (m) (typep mode m)) modes)
	    (error (make-instance 'bad-addressing-mode
				  :instruction ins
				  :addressing-mode mode))
	    ins))))


(defgeneric instruction-behaviour (ins c)
  (:documentation "Return the behaviour of INS on core C.

The instruction can access C to get at registers, memory, and so on,
as well as the behaviour of its own addressing mode."))


(defgeneric instruction-argument (ins c)
  (:documentation "Return the argument of INS on core C.

The argument is specified by the addressing mode of INS."))


(defmethod instruction-argument ((ins instruction) c)
  (addressing-mode-behaviour (instruction-addressing-mode ins) c))


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


;; ---------- Macro interface ----------
