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

;; ---------- Memory ----------

(defmodule memory ((addr :width 4 :direction :in)
		   (data :width 8 :direction :out))

  (let ((mem (make-array '(16) :element-type (unsigned-byte 8)
			       :initial-element 0)))

    ;; data always holds the value addressed by addr
    (setf data (aref mem addr))))


;; ---------- ALU ----------

(defmodule alu ((a  :width 8 :direction :in)
		(b  :width 8 :direction :in)
		(op :width 1 :direction :in)
		(c  :width 8 :direction :out))

  ;; c always holds the result of op on a and b
  (setf c (case op
	    (0
	     (+ a b))

	    (1
	     (- a b)))))



;; ---------- Core ----------

(defmodule core ((clk :width 1  :direction :in)
		 (rst :width 1  :direction :in)
		 (leds :width 8 :direction :out))

  (let ((pc       0 :width 4)
	(ir       0 :width 8)
	(tstate   0 :width 3)
	(a        0 :width 8)
	(alu-a    0 :width 8)
	(alu-b    0 :width 8)
	(alu-op   0 :width 1)
	(alu-c    0 :width 8)
	(out      0 :width 8)
	(mem-addr 0 :width 4)
	(mem-data 0 :width 8))

    ;; components
    (let ((mem (make-instance 'memory :addr mem-addr
				      :data mem-data))
	  (alu (make-instance 'alu :a  alu-a
				   :b  alu-b
				   :op alu-op
				   :c  alu-c)))

      ;; wire the out register to the LEDs
      (setq leds out)

      (if rst
	  ;; if reseting, set all the necessary registers to 0
	  (progn
	    (setf pc 0)
	    (setf ir 0)
	    (setf tstate 0)
	    (setf out 0)
	    (setf a 0))

	  (let ((the-opcode (>> ir 4))
		(the-address (logand ir #2r1111)))

	    ;; instruction decode
	    (let ((isLDA (= the-opcode #2r0000))
		  (isADD (= the-opcode #2r0001))
		  (isSUB (= the-opcode #2r0010))
		  (isOUT (= the-opcode #2r1110))
		  (isHLT (= the-opcode #2r1111)))

	      ;; preparation step
	      (@ (negedge clk)
		 (case tstate
		   (0
		    (setf mem-addr pc))

		   (2
		    (cond (isLDA
			   (setf mem-addr the-address))

			  (isADD
			   (setf mem-addr the-address))

			  (isSUB
			   (setf mem-addr the-address))))

		   (3
		    (cond (isADD
			   (setf alu-a a)
			   (setf alu-op 0))

			  (isSUB
			   (setf alu-a a)
			   (setf alu-op 1))))))

	      ;; execution step
	      (@ (posedge clk)
		 (case tstate
		   (0
		    (setf ir mem-data)
		    (incf tstate))

		   (1
		    (incf pc)
		    (incf tstate))

		   (2
		    (setf alu-b mem-data)
		    (incf tstate))

		   (3
		    (cond (isLDA
			   (setf a mem-data)
			   (setf tstate 0))

			  (isADD
			   (setf a alu-c)
			   (setf tstate 0))

			  (isSUB
			   (setf a alu-c)
			   (setf tstate 0))

			  (isOUT
			   (setf out a)
			   (setf tstate 0))

			  (isHLT
			   (setf tstate 4))))))))))))
