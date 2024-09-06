;; Tests of latches and an ALU
;;
;; Copyright (C) 2024 Simon Dobson
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

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl)


;; ---------- ALU operations ----------

(test test-alu-ops
  "Test ALU operations."
  (let* ((data-bus (make-instance 'hw:bus :width 8))
	 (data-bus-connector (make-instance 'hw:connector
					    :width 8))
	 (op-bus (make-instance 'hw:bus :width 3))
	 (op-bus-connector (make-instance 'hw:connector
					  :width 3))
	 (clk (make-instance 'hw:pin :state 0
				     :wire (make-instance 'hw:wire)))
	 (en-a (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (en-b (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (en-u (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (wr-a (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (wr-b (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (a (make-instance 'hw:latch :width 8
				     :clock (hw:wire clk)
				     :enable (hw:wire en-a)
				     :bus data-bus
				     :write-enable (hw:wire wr-a)))
	 (b (make-instance 'hw:latch :width 8
				     :clock (hw:wire clk)
				     :enable (hw:wire en-b)
				     :bus data-bus
				     :write-enable (hw:wire wr-b)))
	 (alu (make-instance 'hw:alu :width 8
				     :enable (hw:wire en-u)
				     :op-bus op-bus
				     :c-bus data-bus)))

    ;; connect the data and ops bus connectors
    (hw:connect-pins data-bus-connector data-bus)
    (hw:connect-pins op-bus-connector op-bus)

    ;; wire the latches to the ALU
    (hw:connect-slots a 'hw::latched-bus alu 'hw::a-bus)
    (hw:connect-slots b 'hw::latched-bus alu 'hw::b-bus)

    ;; sanity check
    (hw:ensure-fully-wired alu)

    ;; place some data into the registers
    (setf (hw:pins-value data-bus-connector) #2r101101)
    (setf (hw:state en-a) 1)
    (setf (hw:state wr-a) 1)
    (setf (hw:state clk) 1)
    (setf (hw:state clk) 0)
    (setf (hw:state en-a) 0)

    (setf (hw:pins-value data-bus-connector) #2r100100)
    (setf (hw:state en-b) 1)
    (setf (hw:state wr-b) 1)
    (setf (hw:state clk) 1)
    (setf (hw:state clk) 0)
    (setf (hw:state en-b) 0)

    ;; check we put the values onto the latches
    (is (equal (hw:register-value a)
	       (hw:pins-value (hw::alu-a-bus alu))))
    (is (equal (hw:register-value a) #2r101101))
    (is (equal (hw:register-value b)
	       (hw:pins-value (hw::alu-b-bus alu))))
    (is (equal (hw:register-value b) #2r100100))

    ;; run the operations
    (setf (hw:pins-value op-bus-connector) #2r000) ;; avoid floating inputs for ops
    (setf (hw:state en-u) 1)
    (let ((a (hw:pins-value (hw::alu-a-bus alu)))
	  (b (hw:pins-value (hw::alu-b-bus alu))))

      ;; pass-through a
      (setf (hw:pins-value op-bus-connector) #2r000)
      (is (equal (hw:pins-value (hw::alu-c-bus alu)) a))

      ;; a and b
      (setf (hw:pins-value op-bus-connector) #2r001)
      (is (equal (hw:pins-value (hw::alu-c-bus alu)) (logand a b)))

      ;; a or b
      (setf (hw:pins-value op-bus-connector) #2r010)
      (is (equal (hw:pins-value (hw::alu-c-bus alu)) (logior a b)))

      ;; not a
      (setf (hw:pins-value op-bus-connector) #2r011)
      ;; need to keep within 8 bits
      (let ((8-bit-mask (round (1- (ash 1 8)))))
	(is (equal (hw:pins-value (hw::alu-c-bus alu)) (logand (lognot a)
							       8-bit-mask))))

      ;; a + b
      (setf (hw:pins-value op-bus-connector) #2r100)
      (is (equal (hw:pins-value (hw::alu-c-bus alu)) (+ a b)))

      ;; a - b
      (setf (hw:pins-value op-bus-connector) #2r101)
      (is (equal (hw:pins-value (hw::alu-c-bus alu)) (- a b)))

      ;; a + 1
      (setf (hw:pins-value op-bus-connector) #2r110)
      (is (equal (hw:pins-value (hw::alu-c-bus alu)) (1+ a)))

      ;; a - 1
      (setf (hw:pins-value op-bus-connector) #2r111)
      (let ((r (hw:pins-value (hw::alu-c-bus alu))))
	(is (equal r (1- a)))

	;; value remains on ALU output when ALU disabled
	(setf (hw:state en-u) 0)
	(is (equal (hw:pins-value (hw::alu-c-bus alu)) r))

	;; valuedoesn't change when ALU disabled
	(setf (hw:pins-value op-bus-connector) #2r100)
	(is (equal (hw:pins-value (hw::alu-c-bus alu)) r)))) ))
