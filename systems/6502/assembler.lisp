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

(defvar *assembler-symbols-alist* '()
  "An alist of symbols.

A symbol is mapped to one of:

- An integer, representing an offset into the instruction stream
-
")



;; ---------- Helper functions ----------

(defun assembler-uncomment (s)
  "Remove any comments from S.

This also removes trailing whitespace."
  (regex-replace "\\s+$"
		 (regex-replace *assembler-comment* s "")
		 ""))


(defun assembler-parse-instruction (fs)
  "Parse fields FS as an instruction.

An instruction"

  )



(defun assembler-parse-line (s)
  "Assemble the line of machine code in S.

This returns an assembled instruction or nil, and may change the
state of the assembler."
  (let* ((uncommented (assembler-uncomment s))
	 (fields (split "\\s+" uncommented)))
    ()







    )



  (defun assembler (stream)
    "Assemble the 6502 machine code from STR.

This takes a stream and returns a list of instruction objects."
    ))
