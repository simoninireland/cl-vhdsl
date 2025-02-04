;; The LDA instruction with absolute addressing
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

(in-package :cl-vhdsl/examples/sub6502)
(declaim (optimize debug))


;; ---------- Program counter ----------

(defmodule pc ((clk 0 :width 1 :direction :in)
	       (en :width 1 :direction :in)
	       (addr-bus :width 16 :direction :inout))

  (let ((value #2r0000 :width 16 :as :register)
	(gated #2r0000 :width 16 :as :register))

    ;; on falling edge, latch PC to the gate and onto the address bus
    (@ ((negedge clk))
       (if en
	   (progn
	     ;; place value into gate register
	     (setq gated value :sync t)

	     ;; put gated register into the address bus
	     (setq addr-bus gated :sync t))))

    ;; on rising edge, increment counter
    (@ ((posedge clk))
       (if en
	   ;; increment PC
	   (setq value (+ value 1))))))


;; ---------- Instruction register ----------

(defmodule ir ((clk 0 :width 1 :direction :in)
	       (en :width 1 :direction :in)
	       (data-bus :width 8 :direction :in)
	       (opcode :width 8 :direction :out))

  (let ((value 0 :width 8 :as :register))

    ;; on rising edge, latch from the data bus
    (@ ((posedge clk))
       (if en
	   ;; take value from data bus
	   (setq value data-bus)))

    ;; make the opcode available
    (setq opcode value)))


;; ---------- Accumulator ----------

(defmodule a-register ((clk :width 1 :direction :in)
		       (en :width 1 :direction :in)
		       (rw :width 1 :direction :in)
		       (data-bus :width 1 :direction :inout)
		       (alu-bus :width 8 :direction :out))

  (let ((value 0 :width 8 :as :register))

    ;; on negative edge and reading, put value onto data bus
    (@ ((negedge clk))
       (if (and en
		(= rw 0))
	   (setq data-bus value)))

    ;; on positive edge and writing, latch the data bus into the value
    (@ ((posedge clk))
       (if (and en
		(= rw 1))
	   (setq value data-bus)))

    ;; value always visisble to the ALU
    (setq alu-bus value)))


;; ---------- Address and data buses ----------

(defmodule addr-bus ((pc-en :width 1 :direction :in)
		     (pc-out :width 16 :direction :in)
		     (mar-en :width 1 :direction :in)
		     (mar-out :width 16 :direction :in)
		     (out :width 16 :direction :out))

  (@ (pc_en mar_en)
     (cond (pc_en
	    (setq out pc-out))

	   (mar_en
	    (setq out mar-out))

	   (t
	    (setq out 0)))))


(defmodule data-bus ((a-en :width 1 :direction :in)
		     (a-bus :width 8 :direction :in)
		     (out :width 8 :direction :out))

  (@ (a-en)
     ;; place accumulator value onto data bus
     (setq out a-bus)))


;; ---------- Controller ----------

(defmodule controller ((clk :width 1 :direction :in)
		       (ctrl :width 8 :direction :out)
		       (opcode :width 8 :direction :in))

  (let ((t-state 0 :width 4))

    (with-bitfields (addr_rw
		     pc_en
		     ir_en
		     mar_lo_rw mar_lo_en mar_hi_rw mar_hi_en
		     a_rw a_en)
	ctrl

      (@ ((negedge clk))
	 (setf t-state (+ 1 t-state) :sync t)
	 (setf ctrl 0 :sync t)

	 (case tstate
	   (1
	    ;; fetch the instruction from PC
	    (setf addr_rw 0 :sync t)
	    (setf pc_en 1 :sync t)
	    (setf ir_en 1 :sync t))

	   (2
	    (case opcode
	      (#2r10101101
	       ;; LDA absolute, load low-order byte from PC
	       (setf addr_rw 0 :sync t)
	       (setf pc_en 1 :sync t)
	       (setf mar_lo_rw 1 :sync t)
	       (setf mar_lo_en 1 :sync t))))

	   (3
	    ;; LDA absolute, load high-order byte from PC
	    (setf addr_rw 0 :sync t)
	    (setf pc_en 1 :sync t)
	    (setf mar_hi_rw 1 :sync t)
	    (setf mar_hi_en 1 :sync t))

	   (4
	    ;; LDA absolute, load data from MAR into A
	    (setf addr_rw 0 :sync t)
	    (setf mar_lo_rw 0 :sync t)
	    (setf mar_hi_rw 0 :sync) t
	    (setf mar_lo_en 1 :sync t)
	    (setf mar_hi_en 1 :sync) t
	    (setf a_rw 1 :sync t)
	    (setf a_en 1 :sync t)
	    (setq t-state 0 :sync t)))))))


;; ---------- Top module testbench ----------

(defmodule test-tb ((clk :width 1 :direction :in))

  (let ((ctrl 0 :width 8)

	;; buses
	(addr-bus :width 16 :as :wire)
	(data-bus :width 8 :as :wire)

	;; bus lines
	(pc-out :width 16 :as :wire)
	(ir-in :width 8 :as :wire)
	(opcode :width 8 :as :wire))

    (with-bitfields (pc-en
		     ir-en
		     a-en a-rw)
	ctrl

      (let ((ir (make-instance 'ir :clk clk :en ir_en :data-bus data-bus :opcode opcode))
	    (pc (make-instance 'pc :clk clk :en pc_en :addr-bus addr-bus))
	    (a (make-instance 'a-register :clk clk) :en a-en :rw a-rw
						    :data-bus data-bus)
	    (address-bus (make-instance addr-bus :pc-en pc-en :pc-out addr-bus
						 ))
	    (data-bus (make-instance data-bus :ir-en ir-en))
	    (controller (make-instance 'controller :clk clk :ctrl ctrl :opcode opcode))))))



  )
