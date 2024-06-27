;; 6502 instructions
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

;; ---------- Addressing modes ----------

(defmethod addressing-mode-data ((mode immediate))
  (immediate-value mode))

(defmethod addressing-mode-data ((mode absolute))
  (memory-read-byte mem (absolute-address mode)))


;; ---------- Loads ----------

(defclass LDA (instruction)
  ()
  (:documentation "LoaD Accumulator."))


(defmethod instruction-mnemonic ((ins LDA)) "LDA")
(defmethod instruction-addressing-modes ((ins LDA))
  '(immediate absolute absolute-indexed))
(defmethod instruction-opcode ((ins LDA))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 1 b b b 0 1))
    opcode))


(defclass LDX (instruction)
  ()
  (:documentation "LoaD index register X."))


(defmethod instruction-mnemonic ((ins LDX)) "LDX")
(defmethod instruction-addressing-modes ((ins LDX))
  '(immediate absolute))
(defmethod instruction-opcode ((ins LDA))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 1 b b b 1 0))
    opcode))
(defmethod instruction-behaviour ((ins LDX) )
  `(let ((addr (addressing-mode-)))) (setf X  )
  )


;; ---------- Saves ----------

(defclass STA (instruction)
  ()
  (:documentation "STore Accumulator."))


(defmethod instruction-mnemonic ((ins STA)) "STA")
(defmethod instruction-addressing-modes ((ins STA))
  '(immediate absolute absolute-indexed))
(defmethod instruction-opcode ((ins LDA))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 0 b b b 0 1))
    opcode))
