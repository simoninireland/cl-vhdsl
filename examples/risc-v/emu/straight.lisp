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
	  (when (= (1bit write-mask 0) 1)
	    (setq updated (write-bits (nbits data 7 :width 8)
				      updated 7 :width 8)))
	  (when (= (1bit write-mask 1) 1)
	    (setq updated (write-bits (nbits data 15 :width 8)
				      updated 15 :width 8)))
	  (when (= (1bit write-mask 2) 1)
	    (setq updated (write-bits (nbits data 23 :width 8)
				      updated 23 :width 8)))
	  (when (= (1bit write-mask 3) 1)
	    (setq updated (write-bits (nbits data 31 :width 8)
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
	       (let ((instr (read-integer str t t nil :radix 16 :unsigned-number t)))
		 (write-address ram pc instr)
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


(defclass ALU ()
  ()
  (:documentation "A simple ALU."))


(defun compute (alu a b op add/sub)
  "Perform OP on A and B. using ALU.

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
     (if (= add/sub 1)
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


(defun reset-core (core)
  "Reset CORE."
  (with-slots (pc instr register-file)
      core
    (setf pc 0)
    (setf instr 0)
    (dolist (i (iota 32))
      (setf (aref register-file i) 0))))


(defun run-core (core cycles &key reset stop-at-each-cycle)
  "A RISC-V 32-bit integer core emulator that runs CYCLES instructions on CORE.

if RESET is non-nil the core is reset before running. If STOP-AT-EACH-CYCLE is
non-nil the core will enter the debugger after every instuction cycle."
  (declare (optimize debug))

  (when reset
    (reset-core core))

  (with-slots (mem alu comparator pc instr register-file)
      core

    (let ((clk 0))

      ;; run the core
      (dotimes (c cycles)
	(let (write-back
	      next-pc)

	  ;; instruction fetch
	  (setf instr (read-address mem pc))

	  ;; instruction decoding
	  (with-bitfields ((funct7 7) (rs2id 5) (rs1id 5) (funct3 3) (rdid 5) (opcode 7))
	      instr

	    ;; increment target PC for next instruction cycle
	    (setq next-pc (+ pc 4))

	    (case opcode
	      (#2r0110011
	       ;; ALU register-with-register arithmetic
	       (let ((rs1 (aref register-file rs1id))
		     (rs2 (aref register-file rs2id)))

		 (setf write-back (compute alu rs1 rs2
					   funct3
					   (logand (1bit funct7 5)
						   (1bit instr 5))))))

	      (#2r0010011
	       ;; ALU register-with-immediate arithmetic
	       (let ((rs1 (aref register-file rs1id))
		     (Iimm (twos-complement (nbits instr 31 :end 20) 12)))

		 (setf write-back (compute alu
					   rs1 Iimm
					   funct3
					   (logand (1bit funct7 5)
						   (1bit instr 5))))))

	      (#2r1100011
	       ;; branch
	       (let ((Bimm (make-bitfields (signed-byte 32)
					   ((copy-bit (1bit instr 31) 20) 20)
					   ((1bit instr 7) 1)
					   ((nbits instr 30 :end 25) 6)
					   ((nbits instr 11 :end 8) 4)
					   (0 1))))

		 (let ((rs1 (aref register-file rs1id))
		       (rs2 (aref register-file rs2id)))

		   (if (compare comparator
				rs1 rs2
				funct3)
		       (setf next-pc (+ pc Bimm))))))

	      (#2r1100111
	       ;; jump and link relative to register
	       (let ((rs1 (aref register-file rs1id))
		     (Iimm (twos-complement (nbits instr 31 :end 20) 12)))

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
	       (let ((Uimm (make-bitfields (signed-byte 32)
					   ((1bit instr 31) 1)
					   ((nbits instr 20 :end 12) 9)
					   ((copy-bit 0 12 )))))

		 (setf write-back (+ pc Uimm))))

	      (#2r0110111
	       ;; load upper immediate
	       (let ((Uimm (make-bitfields (signed-byte 32)
					   ((1bit instr 31) 1)
					   ((nbits instr 20 :end 12) 9)
					   ((copy-bit 0 12) 12))))

		 (setf write-back Uimm)))

	      (#2r0000011
	       ;; load
	       (let* ((rs1 (aref register-file rs1id))
		      (Iimm (twos-complement (nbits instr 31 :end 20) 12))
		      (addr (+ rs1 Iimm))
		      (data (read-address mem addr))
		      (read-type (nbits funct3 1 :width 2))
		      (sign-extending (= (1bit funct3 2) 1)))

		 ;; extract loaded data from read data
		 (cond ((= read-type #2r00)
			;; load byte
			(if (= (1bit addr 0) 1)
			    (if (= (1bit addr 1) 1)
				;; upper byte of upper half-word
				(setf write-back (nbits (nbits data 31 :end 16) 15 :end 8))

				;; lower byte of upper half-word
				(setf write-back (nbits (nbits data 31 :end 16) 7 :end 0)))

			    (if (= (1bit addr 1) 1)
				;; upper byte of lower half-word
				(setf write-back (nbits (nbits data 15 :end 0) 15 :end 8))

				;; lower byte of lower half-word
				(setf write-back (nbits (nbits data 15 :end 0) 7 :end 0))))

			;; sign-extend if requested
			(if sign-extending
			    (setf write-back (twos-complement write-back 8))))

		       ((= read-type #2r01)
			;; load half-word
			(if (= (1bit addr 1) 1)
			    ;; upper half-word
			    (setf write-back (nbits data 31 :end 16))

			    ;; lower half-word
			    (setf write-back (nbits data 15 :end 0)))

			;; sign-extend if requested
			(if sign-extending
			    (setf write-back (twos-complement write-back 16))))

		       (t
			;; load word
			(setf write-back data)))))


	      (#2r0100011
	       ;; store
	       (let* ((rs1 (aref register-file rs1id))
		      (rs2 (aref register-file rs2id))
		      (Simm (make-bitfields (signed-byte 32)
					    ((copy-bit (1bit instr 31) 21) 21)
					    ((nbits instr 30 :end 25) 6)
					    ((nbits instr 11 :end 7) 5)))
		      (addr (+ rs1 Simm))
		      (read-type (nbits funct3 1 :width 2))
		      (write-mask (cond ((= read-type #2r00)
					 ;; store byte
					 (if (= (1bit addr 1) 1)
					     ;; writing to byte in upper half-word
					     (if (= (1bit addr 0) 1)
						 #2r1000
						 #2r0100)

					     ;; writing to byte in lower half-word
					     (if (= (1bit addr 0) 1)
						 #2r0010
						 #2r0001)))

					((= read-type #2r01)
					 ;; store half-word
					 (if (= (1bit addr 1) 1)
					     ;; writing to upper half-word
					     #2r1100

					     ;; writing to lower half-word
					     #2r0011))

					(t
					 ;; store word
					 #2r1111))))

		 ;; store the value
		 (write-address mem addr rs2 :write-mask write-mask)))

	      (#2r1110011
	       ;; system
	       ))

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
	    (setf pc next-pc))

	  (when stop-at-each-cycle
	    (break)))))

    ;; return the register file
    register-file))
