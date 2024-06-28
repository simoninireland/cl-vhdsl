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

;; ---------- Loads ----------

(defclass LDA (instruction)
  ()
  (:documentation "LoaD Accumulator."))


(defmethod instruction-mnemonic ((cs (eql 'LDA))) "LDA")
(defmethod instruction-addressing-modes ((cls (eql 'LDA)))
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


(defmethod instruction-mnemonic ((ins (eql 'LDX))) "LDX")
(defmethod instruction-addressing-modes ((ins (eql 'LDX)))
  '(immediate absolute))
(defmethod instruction-opcode ((ins LDX))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 1 b b b 1 0))
    opcode))


(defclass LDY (instruction)
  ()
  (:documentation "LoaD index register X."))


(defmethod instruction-mnemonic ((ins (eql 'LDY))) "LDY")
(defmethod instruction-addressing-modes ((ins (eql 'LDY)))
  '(immediate absolute))
(defmethod instruction-opcode ((ins LDY))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 1 b b b 0 0))
    opcode))


;; ---------- Saves ----------

(defclass STA (instruction)
  ()
  (:documentation "STore Accumulator."))


(defmethod instruction-mnemonic ((ins (eql 'STA))) "STA")
(defmethod instruction-addressing-modes ((ins (eql 'STA)))
  '(immediate absolute absolute-indexed))
(defmethod instruction-opcode ((ins LDA))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 0 b b b 0 1))
    opcode))


;; ---------- Increment and decrement----------

(defclass DEX (instruction)
  ()
  (:documentation "DEcrement index register X."))


(defmethod instruction-mnemonic ((ins (eql 'DEX))) "DEX")
(defmethod instruction-addressing-modes ((ins (eql 'DEX)))
  '(implicit))
(defmethod instruction-opcode ((ins LDA))
  #2r11001010)
