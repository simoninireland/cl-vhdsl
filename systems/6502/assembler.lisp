;; 6502 assembler
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
(named-readtables:in-readtable :interpol-syntax)

;; ---------- The regular expressions ----------

(defparameter *assembler-comment*
  ";(\\s+.*)?"
  "A regular expression matching a comment.

Comments consist of a semi-colon and then the rest of the line.")


(defparameter *assembler-label*
  "([A-Za-z0-9_$-])+:?"
  "A regular expression matching a label.

A label is a simple string.")


(defparameter *assembler-directive*
  "\\.[A-Z]+"
  "A reglar expression matching an assembler directive.

Directives start with a dot.")


(defparameter *assembler-opcode*
  "([A-Z]{3})"
  "A regular expression to match opcodes.

An opcode consists of three upper-case letters. This is only
true for the 6502, not for other processors.")


(defparameter *assembler-immediate*
  "#([0-9]+)"
  "Regular expression to match immediate values.

The 6502 convention is for immediate values to consist
of a hash followed by a number.")


;; ---------- Assembler state ----------

(defvar *assembler-instructions* nil
  "The list of instruction classes used by the assembler.")


(defvar *assembler-directives* nil
  "The list of directives used by the assembler.")


(defvar *assembler-symbol-table* (make-hash-table)
  "An hash table of symbols.

A symbol is mapped to one of:

- A pair (:integer . val) representing an integer constant
- A pair (:offset . off) representing a relative offset from PC
- A pair (:address . addr) representing an absolute address
")


(defvar *assembler-pc* 0
  "The assembler's program counter.")



;; ---------- Pseudo-functions and directives ----------

(defclass directive (abstract-instruction)
  ()
  (:documentation "Assembler directives."))


(defgeneric assembler-directive-action (dir)
  (:documentation "The effect of DIR on the assembler's state."))


(defclass equ (directive)
  ((value
    :documentation "The value."
    :type integer
    :initarg :value
    :reader assembler-equ-value))
  (:documentation "A .EQU directive that defines a value."))


(defmethod instruction-mnemonic ((dir (eql 'equ))) ".EQU")


;; ---------- Parsing functions ----------

(defun assembler-uncomment (s)
  "Remove any comments from S.

This also removes trailing whitespace."
  (regex-replace "\\s+$"
		 (regex-replace *assembler-comment* s "")
		 ""))


(defun assembler-parse-instruction (fields)
  "Parse FIELDS as an instruction line."
   (if-let ((inscls (assembler-get-mnemonic (car fields) *assembler-instructions*)))
    (let ((modes (instruction-addressing-modes inscls)))
      (if-let ((addrcls (assembler-get-addressing-mode (cadr fields) modes)))
	;; we have an addressing mode, construct the instruction
	(let* ((modecls (car addrcls))
	       (mode (if modecls
			 (make-instance modecls :parse (cadr addrcls))
			 nil))
	       (ins (make-instance inscls :addressing-mode mode)))
	  ins)

	;; no addressing mode, fail
	(error "Instruction ~a needs an argument" (instruction-mnemonic inscls))))))


(defun assembler-parse-line (s)
  "Assemble the line of machine code in S.

This returns an assembled instruction or nil, and may change the
state of the assembler."
  (let* ((uncommented (assembler-uncomment s))
	 (fields (split "\\s+" uncommented)))
    (if (> (length fields) 0)
	(if (string= (car fields) "")
	    ;; no label, decode instruction
	    (assembler-parse-instruction (cdr fields))

	    ;; a labelled entry, define the label and then parse the rest
	    (let ((label (car fields)))
	      (assembler-parse-labelled-instruction fields))))))


;; ---------- Macro interface ----------

(defun assembler-parse-sexp (x)
  "Parse X as an s-expression defining an assembler (pseudo-)instruction."
  (let ((opcode (car x)))
    ;; look-up the instruction
    (if-let ((inscls (assembler-get-mnemonic x *assembler-instructions*)))
      ;; we have an instruction
      (let ((mi (cons inscls (cdr x))))
	(apply #'make-instance mi))

      ;; not a recognised instruction
      (error (make-instance 'unrecognised-mnemonic :mnemonic opcode)))))
