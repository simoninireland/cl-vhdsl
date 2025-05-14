;; 32-bit integer-only RISC-V core in emulation
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

(in-package :cl-vhdsl/examples/risc-v/emu)


(defun alu (a b op add/sub)
  "Perform OP on A and B.

ADD/SUB selects between add (0) and subtract (1), or logical (0) and arithmetic shift (1)."
  (case op
    (#2r000
     ;; add or subtract
     (if (= add/sub 1)
	 (- a b)
	 (+ a b)))

    (#2r001
     ;; left shift
     (ash a b))

    (#2r010
     ;; less-than unsigned
     (< (coerce a '(unsigned-byte 32))    ; TESTME
	(coerce b '(unsigned-byte 32))))

    (#2r011
     ;; less-than signed
     (< (coerce a '(signed-byte 32))      ; TESTME
	(coerce b '(signed-byte 32))))

    (#2r100
     ;; exclusive or
     (logxor a b))

    (#2r101
     ;; right shift, logical or arithmetic
     (if (= add/sub 1)
	 (ash a b)            ; FIXME: get the sdign extension right
	 (ash a (- b))))

    (#2r110
     ;; or
     (logior a b))

    (#2r100
     ;; and
     (logand a b))))


(defun whitespace-char-p (x)
  (or (char= #\space x)
      (not (graphic-char-p x))))


(defun skip-whitespace (str)
  "Skip whitespace in STR."
  (when (whitespace-char-p (peek-char nil str))
    (read-char str)
    (skip-whitespace str)))


(defun load-program (mem filename)
  "Load a program from FILENAME into MEM."
  (let ((pc 0))
    (labels ((read-instruction (str)
	       (let ((instr (read-integer str t t nil :radix 16 :unsigned-number t)))
		 (setf (aref mem (ash pc -2)) instr)
		 (incf pc 4)))

	     (read-instructions (str)
	       (handler-case
		   (progn
		     (skip-whitespace str)
		     (read-instruction str)
		     (read-instructions str))

		 (end-of-file (eof)
		   t))))

      (with-open-file (str filename :direction :input)
	(read-instructions str)))))


(defun rv32i (firmware-name cycles)
  "A RISC-V 32-bit integer core emulator."
  (declare (optimize debug))

  (let ((clk 0)
	(mem (make-array '(256) :element-type '(unsigned-byte 32)
				:initial-element 0))
	(register-file (make-array '(32) :element-type '(unsigned-byte 32)
					 :initial-element 0))
	(pc 0))

    ;; load the firmware
    (load-program mem firmware-name)

    ;; run the core
    (dotimes (c cycles)
      ;; instruction fetch
      (let ((instr (aref mem (ash pc -2)))
	    write-back
	    next-pc)

	;; instruction decoding
	(with-bitfields ((funct7 7) (rs2id 5) (rs1id 5) (funct3 3) (rdid 5) (opcode 7))
	    instr

	  ;; increment PC
	  (setq next-pc (+ pc 4))

	  (case opcode
	    (#2r0110011
	     ;; ALU register-with-register arithmetic
	     (let ((rs1 (aref register-file rs1id))
		   (rs2 (aref register-file rs2id)))

	       (setf write-back
		     (alu rs1 rs2
			  funct3
			  (logand (1bit funct7 5)
				  (1bit instr 5))))))

	    (#2r0010011
	     ;; ALU register-with-immediate arithmetic
	     (let ((rs1 (aref register-file rs1id))
		   (Iimm (coerce (nbits instr 31 :end 20) '(signed-byte 12))))

	       (setf write-back
		     (alu rs1 Iimm
			  funct3
			  (logand (1bit funct7 5)
				  (1bit instr 5))))))

	    (#2r1100011
	     ;; branch
	     )

	    (#2r1100111
	     ;; jump and link relative to register
	     (let ((rs1 (aref register-file rs1id))
		   (Iimm (coerce (nbits instr 31 :end 20) '(signed-byte 12))))

	       (setf write-back (+ pc 4))
	       (setf next-pc (+ rs1 Iimm) )))

	    (#2r1101111
	     ;; jump and link relative to PC
	     (let ((Jimm (make-bitfields (signed-byte 32)
					 ((copy-bit (1bit instr 31) 12) 12)
					 ((nbits instr 19 :end 12) 8)
					 ((1bit instr 20) 1)
					 ((nbits instr 30 :end 21) 10)
					 (0 1))))

	       (setf write-back (+ pc 4))
	       (setf next-pc (+ pc Jimm))))

	    (#2r0010111
	     ;; add upper immediate
	     )

	    (#2r0110111
	     ;; load upper
	     )

	    (#2r0000011
	     ;; load
	     )

	    (#2r0100011
	     ;; store
	     )

	    (#2r1110011
	     ;; system
	     ))

	  ;; write-back register
	  (when (and (/= rdid 0)
		     (or (= opcode #2r0110011)	 ; ALUreg
			 (= opcode #2r0010011)	 ; ALUimm
			 (= opcode #2r1100111)	 ; JALR
			 (= opcode #2r1101111))) ; JAL
	    (setf (aref register-file rdid) write-back)))

	;; update PC
	(setf pc next-pc)))

    ;; return the register file
    register-file))
