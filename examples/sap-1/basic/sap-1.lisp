;; The SAP-1 processor in Lisp as an emulator
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

;; ---------- Assembler ----------

(defun as-byte (b)
  "Force B to be a byte."
  (logand b #2r11111111))


(defun as-halfbyte (b)
  "Force B to be a 4-bit half-byte (nibble)."
  (logand b #2r1111))


(defun make-opcode (ins &optional (data 0))
  "Construct an 8-bit opcode from INS and DATA.

The upper 4 bits are the opcode, with the bottom 4 bits being either
an address or ignored."
  (logior (ash (as-halfbyte ins) 4)
	  (as-halfbyte data)))


(defun lda (addr)
  "Load the accumulator from ADDR."
  (make-opcode #2r0000 addr))


(defun add (addr)
  "Add the value in ADDR to the accumulator."
  (make-opcode #2r0001 addr))


(defun sub (addr)
  "Subtract the value in ADDR from the accumulator."
  (make-opcode #2r0010 addr))


(defun out ()
  "Transfer the accumulator to the output register."
  (make-opcode #2r1110))


(defun hlt ()
  "Halt the machine."
  (make-opcode #2r1111))


(defun data (b)
  "Assemble B as a literal byte."
  (as-byte b))


(defun pad ()
  "Assemble a padding 0 byte."
  (data 0))


(defun assemble (&rest opcodes)
  "Assemble the program defined by OPCODES."
  (let ((l (length opcodes)))
    (if (> l 16)
	(error "Program too long"))

    ;; pad program to the length of the memory
    (let ((padding (mapcar (lambda (i) (pad)) (iota (- 16 l)))))
      (append opcodes padding))))


;; ---------- Emulator ----------

(defgeneric compute (c)
  (:documentation "Compute the state of combinatorial component C."))


(defclass memory ()
  ((mem
    :type (array (unsigned-byte 8) (16))
    :initform (make-array '(16) :element-type '(unsigned-byte 8)
				:initial-element 0))
   (addr
    :type (unsigned-byte 4)
    :initform 0)
   (data
    :type (unsigned-byte 8)
    :initform 0))
  (:documentation "A 16-byte read-only memory."))


(defmethod compute ((the-memory memory))
  (with-slots (addr data)
      the-memory
    (setf data (aref (slot-value the-memory 'mem) addr))))


(defmethod (setf addr) (addr (the-memory memory))
  (setf (slot-value the-memory 'addr) addr)
  (compute the-memory))


(defclass alu ()
  ((a
    :type (unsigned-byte 8)
    :initform 0)
   (b
    :type (unsigned-byte 8)
    :initform 0)
   (c
    :type (unsigned-byte 8)
    :initform 0)
   (op
     :type bit
     :initform 0))
  (:documentation "An ALU with only add and subtract."))


(defmethod compute ((the-alu alu))
  (with-slots (a b c op)
      the-alu
    (setf c (case op
	      (0
	       ;; addition
	       (+ a b))

	      (1
	       ;; subtraction
	       (- a b))))))


(defmethod (setf a) (v (the-alu alu))
  (setf (slot-value the-alu 'a) v)
  (compute the-alu))


(defmethod (setf b) (v (the-alu alu))
  (setf (slot-value the-alu 'b) v)
  (compute the-alu))


(defmethod (setf op) (v (the-alu alu))
  (setf (slot-value the-alu 'op) v)
  (compute the-alu))


(defclass core ()
  ((pc
    :type (unsigned-byte 4)
    :initform 0)
   (ir
    :type (unsigned-byte 8)
    :initform 0)
   (a
    :type (unsigned-byte 8)
    :initform 0)
   (out
    :type (unsigned-byte 8)
    :initform 0)
   (tstate
    :type (unsigned-byte 4)
    :initform 0)
   (clk
    :initform 0)
   (rst
    :initform 0)
   (mem
    :initarg :memory)
   (alu
    :initarg :alu))
  (:documentation "A core."))


(defun tick (core)
  "Perform a single clock cycle (down and then up)."
  (dotimes (i 2)
    (setf (slot-value core 'clk)
	  (mod (1+ (slot-value core 'clk)) 2))
    (compute core)))


(defun reset (core)
  "Asynchronously pulse the reset line."
  (setf (slot-value core 'rst) 1)
  (compute core)
  (setf (slot-value core 'rst) 0)
  (compute core))


;; There's a potential timing error here it'd be good to try to find
;; with a debugger: if the reset line is pulsed while the clock
;; happens to be high, then the core will land in a strange state
;; whereby it will re-read the last instruction again rather than
;; starting back at address 0.


(defmethod compute ((the-core core))
  "Perform to core action for a single clock change."
  (with-slots (pc ir a out clk rst tstate mem alu)
      the-core

    (if (= rst 1)
	(progn
	  (setf pc 0)
	  (setf ir 0)
	  (setf tstate 0)
	  (setf out 0)
	  (setf a 0))

	(let ((the-opcode (ash ir -4))
	      (the-address (logand ir #2r1111)))

	  ;; insturction decode
	  (let ((isLDA (= the-opcode #2r0000))
		(isADD (= the-opcode #2r0001))
		(isSUB (= the-opcode #2r0010))
		(isOUT (= the-opcode #2r1110))
		(isHLT (= the-opcode #2r1111)))

	    ;; clock is low
	    (when (= clk 0)
	      (case tstate
		(0
		 (setf (addr mem) pc))

		(2
		 (cond (isLDA
			(setf (addr mem) the-address))

		       (isADD
			(setf (addr mem) the-address))

		       (isSUB
			(setf (addr mem) the-address))))

		(3
		 (cond (isADD
			(setf (a alu) a)
			(setf (op alu) 0))

		       (isSUB
			(setf (a alu) a)
			(setf (op alu) 1))))))

	    ;; clock is high
	    (when (= clk 1)
	      (case tstate
		(0
		 (setf ir (slot-value mem 'data))
		 (incf tstate))

		(1
		 (incf pc)
		 (incf tstate))

		(2
		 (setf (b alu) (slot-value mem 'data))
		 (incf tstate))

		(3
		 (cond (isLDA
			(setf a (slot-value mem 'data))
			(setf tstate 0))

		       (isADD
			(setf a (slot-value alu 'c))
			(setf tstate 0))

		       (isSUB
			(setf a (slot-value alu 'c))
			(setf tstate 0))

		       (isOUT
			(setf out a)
			(setf tstate 0))

		       (isHLT
			(setf tstate 4)))))))))))


;; ---------- Running a program ----------

(defparameter sap-1-memory (make-instance 'memory))
(defparameter sap-1-alu    (make-instance 'alu))
(defparameter sap-1-core   (make-instance 'core :memory sap-1-memory
						:alu sap-1-alu))


(defparameter program
  (assemble
    (lda #16r9)     ;; #16r0
    (add #16rA)
    (add #16rB)
    (sub #16rC)
    (out)
    (hlt)
    (pad)
    (pad)
    (pad)
    (data #16r10)   ;; #16r9
    (data #16r14)   ;; #16rA
    (data #16r18)   ;; #16rB
    (data #16r20))) ;; #16rC


(defun load-program (core program)
  "Load PROGRAM into the memory of CORE."
  (with-slots (mem)
      (slot-value core 'mem)
    (dotimes (addr 16)
      (setf (aref mem addr) (elt program addr)))))


(defun run-program (core ticks)
  "Run the program in CORE's memory for TICKS clock cycles."
  (dotimes (i ticks)
    (tick core)))
