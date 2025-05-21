;; 32-bit integer-only RISC-V core in emulation with wires floated and functionalised
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


;; ---------- Supporting components ----------

(defclass Memory ()
  ((words
    :documentation "The size of the memory in 32-bit words."
    :initarg :words)
   (mem
    :documentation "The memory."))
  (:documentation "A RAM."))


(defmethod initialize-instance :after ((mem Memory) &key words &allow-other-keys)
  (setf (slot-value mem 'mem)
	(make-array (list words) :element-type '(unsigned-byte 32)
				 :initial-element 0)))


(defun read-address (ram addr)
  "Return the value in ADDR of RAM.

ADDR must be word-aligned."
  (aref (slot-value ram 'mem) (ash addr -2)))


(defun write-address (ram addr data &key (write-mask 2r1111))
  "Write the value DATA into ADDR of RAM.

ADDR must be word-aligned.

If supplied, WRITE-MASK is a mask that sets which of the bytes in the
word are written to. If omitted or set to #2r1111 then all bytes are
updated; #2r0001 updates only the least-significant byte; and so on."
  (let ((word-addr (ash addr -2)))
    (if (= write-mask 2r1111)
	;; write all bytes in one go
	(setf (aref (slot-value ram 'mem) word-addr) data)

	;; write according to the mask, which is less efficient
	(let ((updated (aref (slot-value ram 'mem) word-addr)))
	  (when (asserted-p (bits write-mask 0))
	    (setq updated (write-bits (bits data 7 :width 8)
				      updated 7 :width 8)))
	  (when (asserted-p (bits write-mask 1))
	    (setq updated (write-bits (bits data 15 :width 8)
				      updated 15 :width 8)))
	  (when (asserted-p (bits write-mask 2))
	    (setq updated (write-bits (bits data 23 :width 8)
				      updated 23 :width 8)))
	  (when (asserted-p (bits write-mask 3))
	    (setq updated (write-bits (bits data 31 :width 8)
				      updated 31 :width 8)))

	  (setf (aref (slot-value ram 'mem) word-addr) updated)))))


(defun whitespace-char-p (c)
  "Test whether C is a whitespace character.

This is the definition used by the standard."
  (or (char= #\space c)
      (not (graphic-char-p c))))


(defun skip-whitespace (str)
  "Skip whitespace in STR."
  (when (whitespace-char-p (peek-char nil str))
    (read-char str)
    (skip-whitespace str)))


(defun load-program (ram filename)
  "Load a program from FILENAME into RAM."
  (let ((pc 0))
    (labels ((read-instruction (str)
	       "Read a single instruction from STR and store it."
	       (let ((instr (read-integer str t t nil :radix 16 :unsigned-number t)))
		 (write-address ram pc instr)
		 (incf pc 4)))

	     (read-instructions (str)
	       "Read all instructions from STR."
	       (handler-case
		   (progn
		     (skip-whitespace str)
		     (read-instruction str)
		     (read-instructions str))

		 (end-of-file (eof)
		   t))))

      (with-open-file (str filename :direction :input)
	(read-instructions str)))))


(defclass ALU ()
  ()
  (:documentation "A simple ALU."))


(defun compute (alu a b op add/sub)
  "Perform OP on A and B. using ALU.

ADD/SUB selects between add (0) and subtract (1), or logical (0) and arithmetic shift (1)."
  (case op
    (#2r000
     ;; add or subtract
     (if (asserted-p add/sub)
	 (- a b)
	 (+ a b)))

    (#2r001
     ;; left shift
     (ash a b))

    (#2r010
     ;; less-than unsigned
     (< (coerce a '(unsigned-byte 32))	; TESTME
	(coerce b '(unsigned-byte 32))))

    (#2r011
     ;; less-than signed
     (< (coerce a '(signed-byte 32))	; TESTME
	(coerce b '(signed-byte 32))))

    (#2r100
     ;; exclusive or
     (logxor a b))

    (#2r101
     ;; right shift, logical or arithmetic
     (if (asserted-p add/sub)
	 (ash a b)		; FIXME: get the sign extension right
	 (ash a (- b))))

    (#2r110
     ;; or
     (logior a b))

    (#2r100
     ;; and
     (logand a b))))


(defclass Comparator ()
  ()
  (:documentation "A simple comparator."))


(defun compare (comparator a b op)
  "Compare A to B under OP."
  (case op
    (#2r000
     ;; equality
     (= a b))

    (#2r001
     ;; inequality
     (/= a b))

    (#2r100
     ;; less-than (signed)
     (< a b))

    (#2r101
     ;; greater-than or equal-to (signed)
     (>= a b))

    (#2r110
     ;; less-than (unsigned)
     (< a b))				; FIXME

    (#2r111
     ;; greater-than or equal-to (unsigned)
     (>= a b))				; FIXME

    (t
     0)))


;; ---------- Core ----------

(defclass RV32I ()
  ((mem
    :documentation "Main RAM."
    :initarg :ram)
   (alu
    :documentation "The ALU."
    :initarg :alu)
   (comparator
    :documentation "The comparator."
    :initarg :comparator)

   ;; pin interface
   (clk
    :documentation "Clock."
    :initform 0)
   (rst
    :documentation "Reset."
    :initform 0)

   ;; core state
   (register-file
    :documentation "The register file.")
   (pc
    :documentation "The program counter."
    :initform 0)
    (instr
    :documentation "The current instruction."
    :initform 0))
  (:documentation "A RISC-V 32-bit integer core."))


(defmethod initialize-instance :after ((core RV32I) &key &allow-other-keys)
  (setf (slot-value core 'register-file) (make-array '(32) ; :element-type '(unsigned-byte 32)
							   :initial-element 0)))


(defun run-cycle (core)
  "Run a single clock cycle on CORE.

This works by asserting and de-asserting the clock line."
  (with-slots (clk)
      core

    (setf clk 1)
    (cycle core)
    (setf clk 0)
    (cycle core)))


(defun run-core (core cycles &key reset)
  "Run CORE for CYCLES clock cycles.

If RESET is non-nil the core is reset before running. This also
adds one more cycle to CYCLES."
  (with-slots (clk rst register-file)
      core

    ;; reset the core initially if requested
    (when reset
      (setf rst 1)
      (run-cycle core)
      (setf rst 0))

    ;; run the core
    (dotimes (i cycles)
      (run-cycle core))

    ;; return the register file
    register-file))


(defun cycle (core)
  "Behaviour of a RISC-V 32-bit integer CORE."
  (declare (optimize debug))

  (with-slots (clk rst mem alu comparator pc instr register-file)
      core

    (let (write-back next-pc)

      (if (asserted-p clk)
	  (if (asserted-p rst)
	      ;; reset line asserted, perform a reset
	      (progn
		(setf pc 0)
		(setf instr 0)
		(dolist (i (iota 32))
		  (setf (aref register-file i) 0)))

	      ;; run main behaviour
	      (progn
		;; instruction fetch
		(setf instr (read-address mem pc))

		;; instruction decoding
		(with-bitfields ((funct7 7) (rs2id 5) (rs1id 5) (funct3 3) (rdid 5) (opcode 7))
		    instr

		  ;; extract the immediate values
		  (flet ((Bimm ()
			   (make-bitfields (signed-byte 32)
					   ((copy-bit (bits instr 31) 20) 20)
					   ((bits instr 7) 1)
					   ((bits instr 30 :end 25) 6)
					   ((bits instr 11 :end 8) 4)
					   (0 1)))

			 (Jimm ()
			   (make-bitfields (signed-byte 32)
					   ((copy-bit (bits instr 31) 12) 12)
					   ((bits instr 19 :end 12) 8)
					   ((bits instr 20) 1)
					   ((bits instr 30 :end 21) 10)
					   (0 1)))

			 (Uimm ()
			   (make-bitfields (signed-byte 32)
					   ((bits instr 31) 1)
					   ((bits instr 20 :end 12) 9)
					   ((copy-bit 0 12) 12)))

			 (Iimm ()
			   (twos-complement (bits instr 31 :end 20) 12))

			 (Simm ()
			   (make-bitfields (signed-byte 32)
					   ((copy-bit (bits instr 31) 21) 21)
					   ((bits instr 30 :end 25) 6)
					   ((bits instr 11 :end 7) 5))))

		    ;; extract the register values
		    (let ((rs1 (aref register-file rs1id))
			  (rs2 (aref register-file rs2id)))

		      ;; increment target PC for next instruction cycle
		      (setq next-pc (+ pc 4))

		      ;; run the instruction
		      (case opcode
			;; ALU register-with-register arithmetic (ALUreg)
			(#2r0110011
			 (setf write-back (compute alu
						   rs1 rs2
						   funct3
						   (logand (bits funct7 5)
							   (bits instr 5)))))

			;; ALU register-with-immediate arithmetic (ALUimm)
			(#2r0010011
			 (setf write-back (compute alu
						   rs1 (Iimm)
						   funct3
						   (logand (bits funct7 5)
							   (bits instr 5)))))

			;; conditional branch (BR)
			(#2r1100011
			 (if (compare comparator
				      rs1 rs2
				      funct3)
			     (setf next-pc (+ pc (Bimm)))))

			;; jump and link relative to register (JALR)
			(#2r1100111
			 (setf write-back (+ pc 4))
			 (setf next-pc (+ rs1 (Iimm)) ))

			;; jump and link relative to PC (JAL)
			(#2r1101111
			 (setf write-back (+ pc 4))
			 (setf next-pc (+ pc (Jimm))))

			;; load upper immediate relative to PC (AUIPC)
			(#2r0010111
			 (setf write-back (+ pc (Uimm))))

			;; load upper immediate (LUI)
			(#2r0110111
			 (setf write-back (Uimm)))

			;; load relative to register (L)
			(#2r0000011
			 (let* ((addr (+ rs1 (Iimm)))
				(data (read-address mem addr))
				(read-type (bits funct3 1 :width 2))
				(sign-extending (asserted-p (bits funct3 2) 1)))

			   ;; extract loaded data from read data
			   (cond ((= read-type #2r00)
				  ;; load byte
				  (if (asserted-p (bits addr 0))
				      (if (asserted-p (bits addr 1))
					  ;; upper byte of upper half-word
					  (setf write-back (bits (bits data 31 :end 16) 15 :end 8))

					  ;; lower byte of upper half-word
					  (setf write-back (bits (bits data 31 :end 16) 7 :end 0)))

				      (if (asserted-p (bits addr 1))
					  ;; upper byte of lower half-word
					  (setf write-back (bits (bits data 15 :end 0) 15 :end 8))

					  ;; lower byte of lower half-word
					  (setf write-back (bits (bits data 15 :end 0) 7 :end 0))))

				  ;; sign-extend if requested
				  (if sign-extending
				      (setf write-back (twos-complement write-back 8))))

				 ((= read-type #2r01)
				  ;; load half-word
				  (if (asserted-p (bits addr 1))
				      ;; upper half-word
				      (setf write-back (bits data 31 :end 16))

				      ;; lower half-word
				      (setf write-back (bits data 15 :end 0)))

				  ;; sign-extend if requested
				  (if sign-extending
				      (setf write-back (twos-complement write-back 16))))

				 (t
				  ;; load word
				  (setf write-back data)))))

			;; store relative to register (S)
			(#2r0100011
			 (let* ((addr (+ rs1 (Simm)))
				(read-type (bits funct3 1 :width 2))
				(write-mask (cond ((= read-type #2r00)
						   ;; store byte
						   (if (asserted-p (bits addr 1))
						       ;; writing to byte in upper half-word
						       (if (asserted-p (bits addr 0))
							   #2r1000
							   #2r0100)

						       ;; writing to byte in lower half-word
						       (if (asserted-p (bits addr 0))
							   #2r0010
							   #2r0001)))

						  ((= read-type #2r01)
						   ;; store half-word
						   (if (asserted-p (bits addr 1))
						       ;; writing to upper half-word
						       #2r1100

						       ;; writing to lower half-word
						       #2r0011))

						  (t
						   ;; store word
						   #2r1111))))

			   ;; store the value
			   (write-address mem addr rs2 :write-mask write-mask)))

			;; system (SYSTEM)
			(#2r1110011
			 ;; for now just stop execution here
			 (setf next-pc pc)))))

		  ;; write-back register
		  (when (and (/= rdid 0)
			     (or (= opcode #2r0110011) ; ALUreg
				 (= opcode #2r0010011) ; ALUimm
				 (= opcode #2r1100111) ; JALR
				 (= opcode #2r1101111) ; JAL
				 (= opcode #2r0010111) ; AUIPC
				 (= opcode #2r0110111) ; LUI
				 (= opcode #2r0000011) ; L
				 ))
		    (setf (aref register-file rdid) write-back))

		  ;; update PC
		  (setf pc next-pc))))))))
