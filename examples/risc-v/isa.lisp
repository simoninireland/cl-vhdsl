;; RV32I instruction set architecture
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


(defisa rv32i
    (:internal-registers
     (pc     0 :width 32)
     (instr  0 :width 32))

  (:visible-registers
   (registerFile (make-array '(32) :element-type (fixed-width-unsigned 32)
				   :initial-contents '(0 0 0 0 0 0 0 0
						       0 0 0 0 0 0 0 0
						       0 0 0 0 0 0 0 0
						       0 0 0 0 0 0 0 0))))

  (:instructions
   (r-type
    (:pattern ((7 funct7) (5 rsId2) (5 rsId1) (3 funct3) (5 rdId) (7 opcode)))
    (:decode (= opcode #2r0110011))
    (:variants
     (subtract-register-register
      :mnemonic sub
      :decode (and (= funct3 #2r000)
		   (bit funct7 5)
		   (bit instr 5))
      :behaviour (setq (aref registerFile rdId)
		       (- rsId1 rsId2)))
     (subtractadd-register-register
      :mnemonic add
      :decode (and (= funct3 #2r000)
		   (not (and (bit funct7 5)
			     (bit instr 5))))
      :behaviour (setq (aref registerFile rdId)
		       (+ rsId1 rsId2)))
     (exclusive-or-register-register
      :mnemonic xor
      :decode (= funct3 #2r100)
      :behaviour (setq (aref registerFile rdId)
		       (logxor rsId1 rsId2)))
     (inclusive-or-register-register
      :mnemonic or
      :decode (= funct3 #2r110)
      :behaviour (setq (aref registerFile rdId)
		       (logior rsId1 rsId2)))
     (and-register-register
      :mnemonic and
      :decode (= funct3 #2r111)
      :behaviour (setq (aref registerFile rdId)
		       (logand rsId1 rsId2)))))

   (i-type
    (:pattern ((12 imm) (5 rsId1) (3 funct3) (5 rdId) (7 opcode)))
    (:decode (= opcode #2r0010011))
    (:variants
     (add-register-immediate
      :mnemonic addi
      :decode (and (= funct3 #2r000)
		   (not (and (bit funct7 5)
			     (bit instr 5))))
      :behaviour (setq (aref registerFile rdId)
		       (+ rsId1 (the (fixed-width-signed 32) imm))))

     )


    )


   )
