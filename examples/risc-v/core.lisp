;; RISC-V core
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

(in-package :cl-vhdsl/examples/risc-v)
(declaim (optimize debug))


(defmodule SOC ((clk-in   :as :wire :width 1 :direction :in)
		(reset-in :as :wire :width 1 :direction :in)
		(leds-out :as :wire :width 5 :direction :out)
		(rxd      :as :wire :width 1 :direction :in)
		(txd      :as :wire :width 1 :direction :out))

  (let ((clk 0   :as :wire)
	(reset 0 :as :wire)

	;; plug in to the output to visualise
	(leds 0 :width 5 :as :register)

	;; core state
	(mem   (make-array '(256) :element-width 32 :element-type (fixed-width-integer 32)))
	(pc    0 :width 32 :as :register)
	(instr 0 :width 32 :as :register)

	;; clock management
	(cw (make-instance 'clockworks :clk-in clk-in
				       :reset-in reset
				       :clk clk
				       :reset reset
				       :slow 19)))



    ;; instruction decoding
    (with-bitfields (i i i i i i i)
	instr
      (let ((isALUreg (= i #2r0110011))
	    (isALUimm (= i #2r0010011))
	    (isBranch (= i #2r1100011))
	    (isJALR   (= i #2r1100111))
	    (isJAL    (= i #2r1101111))
	    (isAIUPC  (= i #2r0010111))
	    (isLUT    (= i #2r0110111))
	    (isLoad   (= i #2r0000011))
	    (isStore  (= i #2r0100011))
	    (isSystem (= i #2r1110011))

	    ;; intermediate formats
	    (Uimm (make-bitfields (bit instr 31)
				  (bits instr 30 :end 12)
				  (repeat-bits 12 0)))
	    (Iimm (make-bitfields (repeat-bits 21 (bit instr 31))
				  (bits instr 30 :end 20)))
	    (Simm (make-bitfields (repeat-bits 21 (bit instr 31))
				  (bits instr 30 :end 25)
				  (bits instr 11 :end 7)))
	    (Bimm (make-bitfields (repeat-bits 20 (bit instr 31))
				  (bit instr 7)
				  (bits instr 30 :end 25)
				  (bits instr 11 :end 8)
				  0))
	    (Jimm (make-bitfields (repeat-bits 12 (bit instr 31))
				  (bits instr 19 :end 12)
				  (bit instr 20)
				  (bits instr 30 :end 21)
				  0))

	    ;; source and destination registers
	    (rs1Id (bits instr 19 :end 15))
	    (rs2Id (bits instr 24 :end 20))
	    (rdId  (bits instr 11 :end 7))

	    ;; function codes
	    (funct3 (bits instr 14 :end 12))
	    (funct7 (bits instr 31 :end 25)))

	;; register bank
	(let ((RegisterBank  (make-array '(32) :element-type (fixed-width-integer 32)))
	      (rs1           0 :width 32)
	      (rs2           0 :width 32)
	      (writeBackData 0 :width 32 :as :wire)
	      (writeBackEn   0 :width 1 :as :wire))

	  ;; the ALU
	  (let ((aluIn1 rs1 :as :wire)
		(aluIn2 (if isALUreg
			    rs2
			    Iimm)
			:as :wire)
		(aluOut 0 :width 32)
		(shamt (if isALUreg
			   (bits rs2 4)
			   (bits instr 24 :end 20))
		       :as :wire))

	    (@ (*)
	       (setq aluOut
		     (case funct3
		       (#2r000
			(if (logand (bit funct7 5)
				    (bit instr 5))
			    (- aluIn1 aluIn2)
			    (+ aluIn1 aluIn2)))

		       (#2r001
			(<< aluIn1 shamt))

		       (#2r010
			(< aluIn1 aluIn2)) ;; signed

		       (#2r011
			(< aluIn1 aluIn2)) ;; unsigned

		       (#2r100
			(logxor aluIn1 aluIn2))

		       (#2r101
			(if (bit funct7 5)
			    (>> aluIn1 shamt)    ;; sign-extended
			    (>> aluIn1 shamt)))  ;; unsigned

		       (#2r110
			(logior aluIn1 aluIn2))

		       (#2r111
			(logand aluIn1 aluIn2)))))

	    ;; the state machine
	    (let ((FETCH-INSTR 0 :as :constant)
		  (FETCH-REGS  1 :as :constant)
		  (EXECUTE     2 :as :constant)
		  (state       0 :width 3))
	      (let ((writeBackData (if (or isJAL isJALR)
				       (+ pc 4)
				       aluOut))
		    (writeBackEn (and (= state 2)
				      (or isALUReg
					  isALUImm
					  isJAL
					  isJALR)))
		    (nextpc (cond (isJAL
				   (+ pc Jimm))
				  (isJALR
				   (+ rs1 Iimm))
				  (t
				   (+ PC 4)))))

		(@ (posedge clk)
		   (if reset
		       (progn
			 (setq pc 0)
			 (setq state FETCH-INSTR))

		       (if (and writeBackEn
				(/= rdId 0))
			   (progn
			     (setf (aref RegisterBank rdId) writeBackData)

			     ;; update the LEDS if writing to R1
			     (if (= rdId 1)
				 (setq leds writeBackData)))))

		   (case state
		     (FETCH-INSTR
		      (setq instr (aref mem (bits pc 32 :end 2)))
		      (setq state FETCH-REGS))

		     (FETCH-REGS
		      (setq rs1 (aref RegisterBank rs1Id))
		      (setq rs2 (aref RegisterBank rs2Id))
		      (setq state EXECUTE))

		     (EXECUTE
		      (if (not isSystem)
			  (setq pc nextpc))
		      (setq state FETCH-INSTR))))))))))))
