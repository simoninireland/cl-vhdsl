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

;; The SAP-1 ("Simple as Possible - 1") is described in:
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
  (setf (gethash (register-namer) (core-registers *SAP-1-core*)) r))


;; ---------- Addressing modes ----------

(defclass absolute (addressing-mode)
  ((addr
    :documentation "The address, a 4-bit word."
    :reader absolute-address))
  (:documentation "doc"))


(defmethod addressing-mode-behaviour ((mode absolute))
  (absolute-address mode))


;; ---------- Instruction set ----------

;; LDA

(defclass LDA (instruction)
  ()
  (:documentation "LoaD Accumulator."))


(defmethod instruction-addressing-mode ((ins (eql 'LDA)))
  '(absolute))


(defmethod instruction-opcode ((ins LDA))
  (let* ((a (instruction-argument ins))
	 opcode)
    (setf-bitfields opcode (0 0 0 0 a a a a))
    opcode))


(defmethod instruction-behaviour ((ins LDA) c)
  (setf (core-register-value 'A c)
	(core-memory-location (instruction-argument ins c)) c))


;; ADD

(defclass ADD (instruction)
  ()
  (:documentation "ADD value ad address to the accumultor."))


(defmethod instruction-addressing-mode ((ins (eql 'ADD)))
  '(absolute))


(defmethod instruction-opcode ((ins LADD))
  (let* ((a (instruction-argument ins))
	 opcode)
    (setf-bitfields opcode (0 0 0 1 a a a a))
    opcode))


(defmethod instruction-behaviour ((ins ADD) c)
  (setf (core-register-value 'A c)
	(+ (core-register-value 'A c)
	   (core-memory-location (instruction-argument ins c) c))))

;; SUB

(defclass SUB (instruction)
  ()
  (:documentation "SUBtract value at address from the accumultor."))


(defmethod instruction-addressing-mode ((ins (eql 'SUB))
  '(absolute)))


(defmethod instruction-opcode ((ins SUB))
  (let* ((a (instruction-argument ins))
	 opcode)
    (setf-bitfields opcode (0 0 1 0 a a a a))
    opcode))


(defmethod instruction-behaviour ((ins SUB) c)
  (setf (core-register-value 'A c)
	(- (core-register-value 'A c)
	   (core-memory-location (instruction-argument ins c) c))))

;; OUT

(defclass OUT (instruction)
  ()
  (:documentation "Copy the accumulator to the output register."))


(defmethod instruction-addressing-mode ((ins (eql 'OUT))
  '(implicit)))


(defmethod instruction-opcode ((ins OUT))
  (let (opcode)
    (setf-bitfields opcode (1 1 1 0 0 0 0 0))
    opcode))


(defmethod instruction-behaviour ((ins OUT) c)
  (setf (core-register-value 'OUT c) (core-register-value 'A c)))


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
  (throw 'EOP t))
