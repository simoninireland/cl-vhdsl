;; 32-bit integer-only RISC-V core
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
		;; (reset-in :as :wire :width 1 :direction :in)
		(leds-out :as :wire :width 5 :direction :out)
		(rxd      :as :wire :width 1 :direction :in)
		(txd      :as :wire :width 1 :direction :out))

  (let ((clk   0 :as :wire :width 1)
	(reset 0 :as :wire :width 1)

	;; plug in to the output to visualise
	(leds 0 :width 5 :as :register)

	;; core state
	(mem   (make-array '(256) :element-type (fixed-width-unsigned 32)
				  :initial-contents (:file "firmware.hex")))
	(pc    0 :width 32 :as :register)
	(instr 0 :width 32 :as :register)

	;; clock management
	(cw (make-instance 'clockworks :clk-in clk-in
				       :reset-in 0
				       :clk clk
				       :reset reset
				       :slow 19)))

    ;; wire the LED wires to the output register
    (setq leds-out leds)

    ;; instruction decoding
    (let-wires ((isALUreg (= (bits instr 7) #2r0110011) :width 1)
		(isALUimm (= (bits instr 7) #2r0010011) :width 1)
		(isBranch (= (bits instr 7) #2r1100011) :width 1)
		(isJALR   (= (bits instr 7) #2r1100111) :width 1)
		(isJAL    (= (bits instr 7) #2r1101111) :width 1)
		(isAIUPC  (= (bits instr 7) #2r0010111) :width 1)
		(isLUT    (= (bits instr 7) #2r0110111) :width 1)
		(isLoad   (= (bits instr 7) #2r0000011) :width 1)
		(isStore  (= (bits instr 7) #2r0100011) :width 1)
		(isSystem (= (bits instr 7) #2r1110011) :width 1)

		;; intermediate formats
		(Uimm (make-bitfields (bit instr 31)
				      (bits instr 30 :end 12)
				      (extend-bits 0 12))
		      :width 32)
		(Iimm (make-bitfields (extend-bits (bit instr 31) 21)
				      (bits instr 30 :end 20))
		      :width 32)
		(Simm (make-bitfields (extend-bits (bit instr 31) 21)
				      (bits instr 30 :end 25)
				      (bits instr 11 :end 7))
		      :width 32)
		(Bimm (make-bitfields (extend-bits (bit instr 31) 20)
				      (bit instr 7)
				      (bits instr 30 :end 25)
				      (bits instr 11 :end 8)
				      (extend-bits 0 1))

		      :width 32)
		(Jimm (make-bitfields (extend-bits (bit instr 31) 12)
				      (bits instr 19 :end 12)
				      (bit instr 20)
				      (bits instr 30 :end 21)
				      (extend-bits 0 1))
		      :width 32)

		;; source and destination registers
		(rs1Id (bits instr 19 :end 15) :width 5)
		(rs2Id (bits instr 24 :end 20) :width 5)
		(rdId  (bits instr 11 :end 7)  :width 5)

		;; function codes
		(funct3 (bits instr 14 :end 12) :width 3)
		(funct7 (bits instr 31 :end 25) :width 7))

	       ;; register bank
	       (let ((RegisterBank  (make-array '(32) :element-type (fixed-width-unsigned 32)))
		     (rs1           0 :width 32)
		     (rs2           0 :width 32)
		     (writeBackData 0 :width 32 :as :wire)
		     (writeBackEn   0 :width 1  :as :wire))

		 ;; the ALU
		 (let-wires ((aluIn1 rs1 :width 32)
			     (aluIn2 (if isALUreg
					 rs2
					 Iimm)
				     :width 32)
			     (shamt (if isALUreg
					(bits rs2 4)
					(bits instr 24 :end 20))
				    :width 5))

			    (let-registers ((aluOut 0 :width 32))

					   (@ (*)
					      (case funct3
						(#2r000
						 (if (logand (bit funct7 5)
							     (bit instr 5))
						     (setq aluOut (- aluIn1 aluIn2) :sync t)
						     (setq aluOut (+ aluIn1 aluIn2) :sync t)))

						(#2r001
						 (setq aluOut (<< aluIn1 shamt) :sync t))

						(#2r010
						 (setq aluOut (< aluIn1 aluIn2) :sync t)) ;; signed

						(#2r011
						 (setq aluOut (< aluIn1 aluIn2) :sync t)) ;; unsigned

						(#2r100
						 (setq aluOut (logxor aluIn1 aluIn2) :sync t))

						(#2r101
						 (if (bit funct7 5)
						     (setq aluOut (>> aluIn1 shamt) :sync t) ;; sign-extended
						     (setq aluOut (>> aluIn1 shamt) :sync t))) ;; unsigned

						(#2r110
						 (setq aluOut (logior aluIn1 aluIn2) :sync t))

						(#2r111
						 (setq aluOut (logand aluIn1 aluIn2) :sync t))))

					   ;; the state machine
					   (let ((FETCH-INSTR 0 :as :constant)
						 (FETCH-REGS  1 :as :constant)
						 (EXECUTE     2 :as :constant)
						 (state       0 :width 3))

					     (let ((nextpc (cond (isJAL
								  (+ pc Jimm))
								 (isJALR
								  (+ rs1 Iimm))
								 (t
								  (+ pc 4)))
							   :width 32))

					       (setq writeBackData (if (or isJAL isJALR)
								       (+ pc 4)
								       aluOut))
					       (setq writeBackEn (and (= state EXECUTE)
								      (or isALUReg
									  isALUImm
									  isJAL
									  isJALR)))

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
						     (setq instr (aref mem (bits pc 31 :end 2)))
						     (setq state FETCH-REGS))

						    (FETCH-REGS
						     (setq rs1 (aref RegisterBank rs1Id))
						     (setq rs2 (aref RegisterBank rs2Id))
						     (setq state EXECUTE))

						    (EXECUTE
						     (if (not isSystem)
							 (setq pc nextpc))
						     (setq state FETCH-INSTR))))))))))))
