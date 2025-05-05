;; The SAP-1 assembler
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

;; ---------- Helper functions ----------

(defun as-byte (b)
  "Force B to be a byte."
  (logand b #2r11111111))


(defun as-halfbyte (b)
  "Force B to be a 4-bit half-byte (nibble)."
  (logand b #2r1111))


(defun make-opcode (ins &optional (data 0))
  "Construct an 8-bit opcode from INS and DATA.

The upper 4 bits are the opcode, with the bottom 4 bits being either
an address or ignored."
  (logior (ash (as-halfbyte ins) 4)
	  (as-halfbyte data)))


;; ---------- Instructions ----------

(defun lda (addr)
  "Load the accumulator from ADDR."
  (make-opcode #2r0000 addr))


(defun add (addr)
  "Add the value in ADDR to the accumulator."
  (make-opcode #2r0001 addr))


(defun sub (addr)
  "Subtract the value in ADDR from the accumulator."
  (make-opcode #2r0010 addr))


(defun out ()
  "Transfer the accumulator to the output register."
  (make-opcode #2r1110))


(defun hlt ()
  "Halt the machine."
  (make-opcode #2r1111))


;; ---------- Pseudo-instructions ----------

(defun data (b)
  "Assemble B as a literal byte."
  (as-byte b))


(defun pad ()
  "Assemble a padding 0 byte."
  (data 0))


(defun assemble (&rest opcodes)
  "Assemble the program defined by OPCODES."
  (let ((l (length opcodes)))
    (if (> l 16)
	(error "Program too long"))

    ;; pad program to the length of the memory
    (let ((padding (mapcar (lambda (i) (pad)) (iota (- 16 l)))))
      (append opcodes padding))))
