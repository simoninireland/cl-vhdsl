;; SAP-1 architecture
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

;; The SAP-1 ("Simple as Possible - 1") processor is described in:
;;
;; Albert Paul Malvino and Jerald Brown. Digital Computer Electronics.
;; McGraw-Hill. ISBN 0-02-800594-5. 1999.
;;
;; notably in Part 2, p.140.

(in-package :cl-vhdsl/SAP-1)


;; ---------- Architecture ----------

(defparameter *SAP-1-system* (make-instance 'architecture)
  "SAP-1 architecture.")

(defparameter *SAP-1-core* (make-instance 'core)
  "SAP-1 core.")

(dolist (r (list
	    (make-instance 'data-register :name 'A :width 8
					  :documentation "Accumulator.")
	    (make-instance 'data-register :name 'OUT :width 8
					  :documentation "Output register.")
	    (make-instance 'program-counter :name 'PC :width 4
					    :documentation "Program counter.")))
  (setf (gethash (register-name r) (core-registers *SAP-1-core*)) r))


;; ---------- Addressing modes ----------

(defclass absolute (addressing-mode)
  ((addr
    :documentation "The address, a 4-bit word."
    :initarg :address
    :reader absolute-address))
  (:documentation "An absolute address, stored inline."))


(defun absolute (&rest args)
  (apply #'make-instance (cons 'absolute args)))


(defmethod addressing-mode-behaviour ((mode absolute) c)
  (absolute-address mode))


(defmethod addressing-mode-bytes ((mode absolute))
  '())


;; ---------- Instruction set ----------

;; LDA

(defclass LDA (instruction)
  ()
  (:documentation "LoaD Accumulator."))


(defmethod instruction-addressing-mode ((ins (eql 'LDA)))
  '(absolute))


(defmethod instruction-opcode ((ins LDA))
  (let* ((a (absolute-address (instruction-addressing-mode ins)))
	 opcode)
    (setf-bitfields opcode (0 0 0 0 a a a a))
    opcode))


(defmethod instruction-behaviour ((ins LDA) c)
  (setf (emu:core-register-value 'A c)
	(emu:core-memory-location (instruction-argument ins c) c)))


;; ADD

(defclass ADD (instruction)
  ()
  (:documentation "ADD value ad address to the accumulator."))


(defmethod instruction-addressing-mode ((ins (eql 'ADD)))
  '(absolute))


(defmethod instruction-opcode ((ins ADD))
  (let* ((a (absolute-address (instruction-addressing-mode ins)))
	 opcode)
    (setf-bitfields opcode (0 0 0 1 a a a a))
    opcode))


(defmethod instruction-behaviour ((ins ADD) c)
  (setf (emu:core-register-value 'A c)
	(+ (emu:core-register-value 'A c)
	   (emu:core-memory-location (instruction-argument ins c) c))))

;; SUB

(defclass SUB (instruction)
  ()
  (:documentation "SUBtract value at address from the accumulator."))


(defmethod instruction-addressing-mode ((ins (eql 'SUB)))
  '(absolute))


(defmethod instruction-opcode ((ins SUB))
  (let* ((a (absolute-address (instruction-addressing-mode ins)))
	 opcode)
    (setf-bitfields opcode (0 0 1 0 a a a a))
    opcode))


(defmethod instruction-behaviour ((ins SUB) c)
  (setf (emu:core-register-value 'A c)
	(- (emu:core-register-value 'A c)
	   (emu:core-memory-location (instruction-argument ins c) c))))


;; OUT

(defclass OUT (instruction)
  ()
  (:documentation "Copy the accumulator to the OUTput register."))


(defmethod instruction-addressing-mode ((ins (eql 'OUT)))
  '(implicit))


(defmethod instruction-opcode ((ins OUT))
  (let (opcode)
    (setf-bitfields opcode (1 1 1 0 0 0 0 0))
    opcode))


(defmethod instruction-behaviour ((ins OUT) c)
  (setf (emu:core-register-value 'OUT c) (emu:core-register-value 'A c)))


;; HLT

(defclass HLT (instruction)
  ()
  (:documentation "HaLT execution."))


(defmethod instruction-opcode ((ins HLT))
  (let (opcode)
    (setf-bitfields opcode (1 1 1 1 0 0 0 0))
    opcode))


(defmethod instruction-addressing-modes ((ins (eql 'HLT)))
  '(implicit))


(defmethod instruction-behaviour ((ins HLT) c)
  (emu:core-end-program c))
