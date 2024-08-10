;; Tests of register operations
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


;; ---------- Registers ----------

(test test-register-load
  "Test a register loads values off its bus correctly."
  (let* ((data-bus (make-instance 'hw:bus :width 8))
	 (data-bus-connector (make-instance 'hw:connector
					    :width 8))
	 (clk (make-instance 'hw:pin :state 0))
	 (en (make-instance 'hw:pin :state 0))
	 (wr (make-instance 'hw:pin :state 0))
	 (reg (make-instance 'hw:register :width 8
					  :clock clk
					  :enable en
					  :bus data-bus
					  :write-enable wr)))

    ;; connect the connector
    (hw:connector-pins-connect data-bus-connector data-bus)

    ;; put a value on the bus
    (setf (hw:connector-pins-value data-bus-connector) #2r1101)

    ;; enable the register and set its value as writeable from the bus
    (setf (hw:pin-state en) 1)
    (setf (hw:pin-state wr) 1)

    ;; clock the register
    (setf (hw:pin-state clk) 1)

    ;; check we loaded the value
    (is (equal (hw:register-value reg) #2r1101))))


(test test-register-save
  "Test a register puts values onto its bus correctly."
  (let* ((data-bus (make-instance 'hw:bus :width 8))
	 (data-bus-connector (make-instance 'hw:connector :width 8))
	 (clk (make-instance 'hw:pin :state 0))
	 (en (make-instance 'hw:pin :state 0))
	 (wr (make-instance 'hw:pin :state 0))
	 (reg (make-instance 'hw:register :width 8
					  :clock clk
					  :enable en
					  :bus data-bus
					  :write-enable wr)))

    ;; connect the connector
    (hw:connector-pins-connect data-bus-connector data-bus)
    (setf (hw:connector-pin-states data-bus-connector) :reading)

    ;; put a value into the register
    (setf (hw:register-value reg) #2r10110)

    ;; enable the register and set its value as readable from the bus
    (setf (hw:pin-state en) 1)
    (setf (hw:pin-state wr) 0)

    ;; clock the register
    (setf (hw:pin-state clk) 1)

    ;; check we place the value on the bus
    (let ((v (hw:register-value reg))
	  (rv (hw:connector-pins-value data-bus-connector)))
      (is (equal v #2r10110))
      (is (equal v rv)))))
