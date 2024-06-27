;; Test the 6502 assemler
;;
;; Copyright (C) 2023 Simon Dobson
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

(in-package :cl-vhdsl/systems/6502/test)
(in-suite cl-vhdsl/systems/6502)


(test test-labels
  "Test we can recognise labels."
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "LABEL"))
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "LABEL16"))
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "16LABEL"))
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "LABEL_16"))
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "LABEL-16"))
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "-LABEL"))
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "_LABEL"))
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "LABEL$"))

  ;; accept trailing colons
  (is (scan cl-vhdsl/systems/6502::*assembler-label* "LABEL:")))


(test test-opcodes
  "Test we can recognise assembler opcodes."
  (is (scan cl-vhdsl/systems/6502::*assembler-opcode* "LDA"))
  (is (null (scan cl-vhdsl/systems/6502::*assembler-opcode* "LD"))))


(test test-uncomment
  "Test we can remove comments."
  (dolist (s '(("; one line" "")
	       ("   ; one line" "")
	       ("   ; one line   " "")
	       (" LDA #12    ; one line" " LDA #12")
	       (" LDA #12    ; one line   " " LDA #12")))
    (is (equal (cl-vhdsl/systems/6502::assembler-uncomment (car s)) (cadr s)))))
