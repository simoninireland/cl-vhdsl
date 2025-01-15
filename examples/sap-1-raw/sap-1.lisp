;; The SAP-1 processor in RTLisp
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

;; Derived from the Verilog code of Austin Morlan
;;
;; https://austinmorlan.com/posts/fpga_computer_sap1/
;;
;; which is itself based on the book
;;
;; Albert Paul Malvino and Jerald Brown. Digital computer electronics.
;; McGraw-Hill. 1999. ISBN 0-02-800594-5.

(in-package :cl-vhdsl/examples/sap-1-raw)


;; ---------- Clock ----------

(defmodule clock ((hlt :width 1 :direction :in)
		  (clk_in :width 1 :direction :in)
		  (clk_out :width 1 :direction :out))

  (setq clk_out (if hlt
		    #2r0
		    clk_in)))


;; ---------- Program counter ----------

(defmodule pc ((hlt :width 1 :direction :in)
	       (clk :width 1 :direction :in)
	       (inc :width 1 :direction :in)
	       (out :width 8 :direction :out))

  (let ((pc 0 :width 3))

    (q@ ((posedge clk) (posedge rst))
       (cond (rst
	      (setq pc #2r0))
	     (inc
	      (incf pc))))

    (setf out pc)))


;; ---------- Registers ----------

(defmodule reg_a ((hlt :width 1 :direction :in)
		  (clk :width 1 :direction :in)
		  (load :width 1 :direction :in)
		  (bus :width 8 :direction :in)
		  (out :width 8 :direction :out))

  (let ((reg_a 0 :width 8))

    (@ ((posedge clk) (posedge rst))
       (cond (rst
	      (setq reg_a #2r0))
	     (load
	      (setf reg_a bus))))

    (setq out reg_a)))


(defmodule reg_b ((hlt :width 1 :direction :in)
		  (clk :width 1 :direction :in)
		  (load :width 1 :direction :in)
		  (bus :width 8 :direction :in)
		  (out :width 8 :direction :out))

  (let ((reg_b 0 :width 8))

    (@ ((posedge clk) (posedge rst))
       (cond (rst
	      (setq reg_b #2r0))
	     (load
	      (setf reg_b bus))))

    (setq out reg_b)))


;; ---------- ALU ----------

(defmodule adder ((a :width 8 :direction :in)
		  (b :width 8 :direction :in)
		  (sub :width 1 :direction :in)
		  (out :width 8 :direction :out))

  (setf out (if sub
		(- a b)
		(+ a b))))



;; ---------- Memory ----------

(defmodule memory ((hlt :width 1 :direction :in)
		   (clk :width 1 :direction :in)
		   (load :width 1 :direction :in)
		   (bus :width 8 :direction :in)
		   (out :width 8 :direction :out))

  (let ((mar 0 :width 4)
	(ram (make-array '(16)
			 :element-type '(fixed-width-unsigned 8))
	     :width 8))

    (@ ((posedge clk) (posedge rst))
       (cond (rst
	      (setq mar #2r0))
	     (load
	      (setf mar (bitwidth bus 3)))))

    (setq out (aref ram mar))))


;; ---------- Instruction register----------

(defmodule ir ((hlt :width 1 :direction :in)
	       (clk :width 1 :direction :in)
	       (load :width 1 :direction :in)
	       (bus :width 8 :direction :in)
	       (out :width 8 :direction :out))

  (let ((ir 0 :width 8))

    (@ ((posedge clk) (posedge rst))
       (cond (rst
	      (setq pc #2r0))
	     (load
	      (setq ir bus))))

    (setq out ir)))


;; ---------- Controller ----------

(defmodule controller ((clk :width 1 :direction :in)
		       (rst :width 1 :direction :in)
		       (opcode :width 4 :direction :in)
		       (out :width 12 :direction :out))

  (let ((sig_hlt       11 :as :constant)
	(sig_pc_int    10 :as :constant)
	(sig_pc_en      9 :as :constant)
	(sig_mem_load   8 :as :constant)
	(sig_mem_en     7 :as :constant)
	(sig_ir_load    6 :as :constant)
	(sig_ir_en      5 :as :constant)
	(sig_a_load     4 :as :constant)
	(sig_a_en       3 :as :constant)
	(sig_b_load     2 :as :constant)
	(sig_adder_sub  1 :as :constant)
	(sig_adder_en   0 :as :constant)

	(op_lda #2r0000 :as :constant)
	(op_add #2r0001 :as :constant)
	(op_sub #2r0010 :as :constant)
	(op_hlt #2r1111 :as :constant)

	(stage 0 :width 3)
	(ctrl_word 0 :width 12))

    (@ ((negedge clk) (posedge rst))
       (cond (rst
	      (setf stage 0))
	     ((= stage 5)
	      (setq stage 0)
	      (incf stage))))

    (@ (stage)
       (setq ctrl_word 0 :sync t)

       (case stage
	 (0
	  (setq (aref ctrl_word sig_pc_en) 1)
	  (setq (aref ctrl_word sig_mem_load) 1))
	 (1
	  (setq (aref ctrl_word sig_pc_inc) 1))
	 (2
	  (setq (aref ctrl_word sig_mem_en) 1)
	  (setq (aref ctrl_word sig_ir_load) 1))
	 (3
	  (case opcode
	    (op_lda
	     (setq (aref ctrl_word sig_ir_en) 1)
	     (setq (aref ctrl_word sig_mem_load) 1))
	    (op_add
	     (setq (aref ctrl_word sig_ir_en) 1)
	     (setq (aref ctrl_word sig_mem_load) 1))
	    (op_sub
	     (setq (aref ctrl_word sig_ir_en) 1)
	     (setq (aref ctrl_word sig_mem_load) 1))
	    (op_hlt
	     (setq (aref ctrl_word sig_hlt) 1))))
	 (4
	  (case opcode
	    (op_lda
	     (setq (aref ctrl_word sig_mem_en) 1)
	     (setq (aref ctrl_word sig_a_load) 1))
	    (op_add
	     (setq (aref ctrl_word sig_mem_en) 1)
	     (setq (aref ctrl_word sig_b_load) 1))
	    (op_sub
	     (setq (aref ctrl_word sig_mem_en) 1)
	     (setq (aref ctrl_word sig_b_load) 1))))
	 (5
	  (case opcode
	    (op_add
	     (setq (aref ctrl_word sig_adder_en) 1)
	     (setq (aref ctrl_word sig_a_load) 1))
	    (op_sub
	     (setq (aref ctrl_word sig_adder_sub) 1)
	     (setq (aref ctrl_word sig_adder_en) 1)
	     (setq (aref ctrl_word sig_a_load) 1))))))

    (setq out ctrl_word)))


;; ---------- Top ----------

(defmodule top ((clk_in :width 1 :direction :in :as :wire))

  (let ((rst :width 1 :as :wire)
	(hlt :width 1 :as :wire)
	(clk :width 1 :as :wire)
	(bus :width 8 :as :wire)
	(ctrl :width 13 :as :wire))
    (with-bitfields (hlt pc_inc pc_en mar_load mem_en ir_load
		     ir_en a_load a_en b_load adder_sub adder_en))

    (make-instance 'clock :hlt hlt :rst rst :clk_in clk_in :clk_out clk)

    (let ((pc_out :width 8 :as :wire))
      (make-instance 'pc :clk clk :rst rst :inc pc_inc :out pc_out))

    (let ((mem_out :width 8  :as :wire))
      (make-instance 'memory :clk clk :rst rst
			     :load mar_load :bus bus :out mem_out))

    (let ((a_out :width 8  :as :wire))
      (make-instance 'reg_a :clk clk :rst rst
			    :load a_load :bus bus :out a_out))

    (let ((b_out :width 8  :as :wire))
      (make-instance 'reg_b :clk clk :rst rst
			    :load b_load :bus bus :out b_out))

    (make-instance 'adder :a a_out :b b_out
			  :sub adder_sub :out adder_out)

    (let ((ir_out :width 8  :as :wire))
      (make-instance 'memory :clk clk :rst rst
			     :load ir_load :bus bus :out ir_out))

    (make-instance 'controller :clk clk :rst rst
			       :opcode (bits ir_out 7 4)
			       :out ctrl)

    (@ (ir_en addr_en a_en mem_en pc_en)
       (cond (ir_en
	      (setq bus ir_out))
	     (addr_en
	      (setq bus addr_out))
	     (a_en
	      (setq bus a_out))
	     (mem_en
	      (setq bus mem_out))
	     (pc_en
	      (setq bus pc_out))
	     (t
	      (setq bus 0))))))
