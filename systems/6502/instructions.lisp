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

(in-package :cl-vhdsl/6502)

;; ---------- Loads ----------

(defclass LDA (instruction)
  ()
  (:documentation "LoaD Accumulator."))


(defun LDA (&rest args)
  (let ((ins (apply #'make-instance (cons 'LDA args))))
    (instruction-check ins)
    ins))
(defmethod instruction-mnemonic ((cls (eql 'LDA))) "LDA")
(defmethod instruction-addressing-modes ((cls (eql 'LDA)))
  '(immediate absolute absolute-indexed))
(defmethod instruction-opcode ((ins LDA))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 1 b b b 0 1))
    opcode))
(defmethod instruction-behaviour ((ins LDA) c)
  (setf (emu:core-register-value 'A c) (instruction-argument ins c))
  (setf (emu:core-flag-value 'Z c) (= (emu:core-register-value 'A c) 0)))


(defclass LDX (instruction)
  ()
  (:documentation "LoaD index register X."))


(defun LDX (&rest args)
  (let ((ins (apply #'make-instance (cons 'LDX args))))
    (instruction-check ins)
    ins))
(defmethod instruction-mnemonic ((ins (eql 'LDX))) "LDX")
(defmethod instruction-addressing-modes ((ins (eql 'LDX)))
  '(immediate absolute))
(defmethod instruction-opcode ((ins LDX))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 1 b b b 1 0))
    opcode))
(defmethod instruction-behaviour ((ins LDX) c)
  (setf (emu:core-register-value 'X c) (instruction-argument ins c))
  (setf (emu:core-flag-value 'Z c) (= (emu:core-register-value 'X c) 0)))


(defclass LDY (instruction)
  ()
  (:documentation "LoaD index register X."))


(defun LDY (&rest args)
  (let ((ins (apply #'make-instance (cons 'LDY args))))
    (instruction-check ins)
    ins))
(defmethod instruction-mnemonic ((ins (eql 'LDY))) "LDY")
(defmethod instruction-addressing-modes ((ins (eql 'LDY)))
  '(immediate absolute))
(defmethod instruction-opcode ((ins LDY))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 1 b b b 0 0))
    opcode))
(defmethod instruction-behaviour ((ins LDY) c)
  (setf (emu:core-register-value 'Y c) (instruction-argument ins c))
  (setf (emu:core-flag-value 'Z c) (= (emu:core-register-value 'Y c) 0)))


;; ---------- Saves ----------

(defclass STA (instruction)
  ()
  (:documentation "STore Accumulator."))


(defun STA (&rest args)
  (let ((ins (apply #'make-instance (cons 'STA args))))
    (instruction-check ins)
    ins))
(defmethod instruction-mnemonic ((ins (eql 'STA))) "STA")
(defmethod instruction-addressing-modes ((ins (eql 'STA)))
  '(immediate absolute absolute-indexed))
(defmethod instruction-opcode ((ins STA))
  (let* ((mode (instruction-addressing-mode ins))
	 (b (addressing-mode-encode mode))
	 opcode)
    (setf-bitfields opcode (1 0 0 b b b 0 1))
    opcode))
(defmethod instruction-behaviour ((ins STA) c)
  (setf (emu:core-memory-location (instruction-argument ins c) c) (emu:core-register-value 'A c)))


;; ---------- Increment and decrement----------

(defclass DEX (instruction)
  ()
  (:documentation "DEcrement index register X."))


(defun DEX (&rest args)
  (let ((ins (apply #'make-instance (cons 'DEX args))))
    (instruction-check ins)
    ins))
(defmethod instruction-mnemonic ((ins (eql 'DEX))) "DEX")
(defmethod instruction-addressing-modes ((ins (eql 'DEX)))
  '(implicit))
(defmethod instruction-opcode ((ins DEX))
  #2r11001010)
(defmethod instruction-behaviour ((ins DEX) c)
  (decf (emu:core-register-value 'X c))
  (setf (emu:core-flag-value 'Z c) (= (emu:core-register-value 'X c) 0)))


;; ---------- Branches ----------

(defclass BNZ (instruction)
  ()
  (:documentation "Branch if Not Zero"))


(defun BNZ (&rest args)
  (let ((ins (apply #'make-instance (cons 'BNZ args))))
    (instruction-check ins)
    ins))
(defmethod instruction-mnemonic ((ins (eql 'BNZ))) "BNZ")
(defmethod instruction-addressing-modes ((ins (eql 'BNZ)))
  '(relative))
(defmethod instruction-behaviour ((ins BNZ) c)
  (if (= (emu:core-flag-value 'Z c) 0)
      (setf (emu:core-register-value 'PC c) (+ (emu:core-register-value 'PC c)
					       (instruction-argument ins c)))))


;; ---------- Miscellaneous ----------

;; At the moment this isn't the actual BRK instruction, which
;; jumps to an interrupt vector: this version stops execution.

(defclass BRK (instruction)
  ()
  (:documentation "BReaK execution."))

(defmethod instruction-mnemonic ((ins (eql 'BRK))) "BRK")
(defmethod instruction-addressing-modes ((ins (eql 'BRK)))
  '(implicit))
(defmethod instruction-opcode ((ins BRK))
  0)
(defmethod instruction-behaviour ((ins BRK) c)
  (emu:core-end-program c))
