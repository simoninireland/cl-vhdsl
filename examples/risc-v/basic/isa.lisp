;; RV32I instruction set architecture
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a Common Lisp DSL for hardware design
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :cl-vhdsl/examples/risc-v)
(declaim (optimize debug))


(defmodule rv32i-soc
    (let ((pc    0 :width 32 :as :register)
	  (instr 0 :width 32 :as :register)

	  (registers (make-array '(32) :element-width 32)))

      ;; decode
      (with-bitfields ((funct7 7) (rs2Id 5) (rs1Id 5) (funct3 3) (rdId 5) (opcode 7))
	instr

	(let ((is-alureg-p (= opcode #2r010010))
	      (is-aluimm-p (= opcode #2r100101)))

	  ;; state machine
	  (@ (posedge clk)
	     (let ((load-instr 0 :as :constant)
		   (load-data  1 :as :constant)
		   (execute    2 :as :constant)
		   (state      0 :as :register))

	       (when reset
		 (setq pc 0)
		 (setq state load-instr))

	       (case state
		 (load-instr
		  (setq state load-data))
		 (load-data
		  (setq state execute))
		 (execute
		  (setq state load-instr)))))

	  ;; load registers
	  (@ (posedge clk)
	     (when (= state load-data)
	       (setq rs1 (areg registers rs1Id))
	       (setq rs2 (areg registers rs2Id))))

	  ;; write back to registers
	  (@ (posedge clk)
	     (when (and write-back-enabke
			(= state execute)
			(/= rdId 0))
	       (setq (areg registers rdId) write-back-data)))



	  ))




      )



  (defclass rv32i-uarch ()
    ()
    (:registers
     (pc 0    :width 32)
     (instr 0 :width 32)

     ;; register file
     (registers (make-array '(32) :element-width 32))

     ;; ALU
     (alu-a-side 0)
     (alu-b-side 0)
     (alu-c-side 0)
     (alu-opcode 0))

    (:state-machine
     fetch-instr
     fetch-data
     execute
     write-back)

    (:predicates
     (is-alu-register-op  (= opcode #2r0110011))
     (is-alu-immediate-op (= opcode #2r0010011))

     (is-add-op      (and (= funct3 #2r000)
			  (= 0 (bit funct 5))))
     (is-sub-op      (and (= funct3 #2r000)
			  (= 1 (bit funct 5))))
     (is-logior-op   (= funct3 #2r110))
     (is-logand-op   (= funct3 #2r111)))

    (:metaclass microarchitecture)
    (:documentation "A RISC-V 32-bit integer architecture.")))


(defclass rv32i-isa ()
  ()

  (:metaclass isa)
  (:documentation "RISC-V 32-bit integer instruction set."))


(defmethod add ((isa rv32i-isa) (arch rv32i-uarch))
  (when (and (or (is-alu-immediate-op is-alu-register-op))
	     is-add-op)

    (on-state execute
	      (u-set-variable alu-opcode #2r000)
	      (u-set-variable alu-a-side (aref registers rd1Id))
	      (u-set-variable alu-b-side (aref registers rd2Id)))

    (on-state write-back
	      (u-set-variable (aref registers rdId) alu-c-side))))


(definstruction add (rd rs1 rs2)
  ((funct7 - 0 - - - - -) (rs2 5) (rs1 5) (rd 5) (funct3 0 0 0) (opcode 0 1 1 0 0 1 1))
  (setf alu-opcode funct3)
  (setf alu-in-a (aref regusters rs1))
  (setf alu-in-b (aref registers rs2))
  (setf (aref registers rd) alu-out))


(definstruction add (rd rs1 rs2)
  ((funct7 - 0 - - - - -) (rs2 5) (rs1 5) (rd 5) (funct3 0 0 0) (opcode 0 1 1 0 0 1 1))
  (in-state load-data
	    (setf alu-opcode funct3)
	    (setf alu-in-a (aref regusters rs1))
	    (setf alu-in-b (aref registers rs2)))
  (in-state execute
	    (setf (aref registers rd) alu-out)))


(definstruction addi (rd rs1 imm12)
  ((imm12 12) (rs1 5) (funct3 0 0 0) (rd 5) (opcode 0 0 1 0 0 1 1))
  (setf alu-opcode funct3)
  (setf alu-in-a (aref regusters rs1))
  (setf alu-in-b imm12)
  (setf (aref registers rd) alu-out))


(defpseudoinstruction mv (rd rs1)
  (addi rd rs1 0))
