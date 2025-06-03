;; Tests of ring counters
;;
;; Copyright (C) 2024 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp)


(test test-rc-initial
  "Test the initial state of the counter."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (make-instance 'hw:connector :width 4))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus)))
    ;; connect the connector
    (hw:connect-pins bus-connector bus)
    (setf (hw:pin-states bus-connector) :reading)

    (is (equal (hw:pins-value bus-connector) #2r0001))))


(test test-rc-inc
  "Test the counter increments correctly."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (make-instance 'hw:connector :width 4))
	 (en (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (clk (make-instance 'hw:pin :state 0
				     :wire (make-instance 'hw:wire)))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock (hw:wire clk)
					     :enable (hw:wire en))))
    ;; connect the connector
    (hw:connect-pins bus-connector bus)
    (setf (hw:pin-states bus-connector) :reading)

    ;; clock the counter twice
    (setf (hw:state en) 1)
    (dotimes (i 2)
      (setf (hw:state clk) 1)
      (setf (hw:state clk) 0))

    (is (equal (hw:pins-value bus-connector) #2r0100))))


(test test-rc-inc-disabled
  "Test the counter doesn't increment when disabled."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (make-instance 'hw:connector :width 4))
	 (en (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (clk (make-instance 'hw:pin :state 0
				     :wire (make-instance 'hw:wire)))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock (hw:wire clk)
					     :enable (hw:wire en))))
    ;; connect the connector
    (hw:connect-pins bus-connector bus)
    (setf (hw:pin-states bus-connector) :reading)

    ;; clock the counter twice
    ;; don't enable
    (dotimes (i 2)
      (setf (hw:state clk) 1)
      (setf (hw:state clk) 0))

    (is (equal (hw:pins-value bus-connector) #2r0001))))


(test test-rc-wrap
  "Test the counter wraps-around."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (make-instance 'hw:connector :width 4))
	 (clk (make-instance 'hw:pin :state 0
				     :wire (make-instance 'hw:wire)))
	 (en (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock (hw:wire clk)
					     :enable (hw:wire en))))
    ;; connect the connector
    (hw:connect-pins bus-connector bus)
    (setf (hw:pin-states bus-connector) :reading)

    ;; clock the counter four times
    (setf (hw:state en) 1)
    (dotimes (i 4)
      (setf (hw:state clk) 1)
      (setf (hw:state clk) 0))

    (is (equal (hw:pins-value bus-connector) #2r0001))))


(test test-rc-bounded
  "Test the counter can't be set too high."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (make-instance 'hw:connector :width 4))
	 (clk (make-instance 'hw:pin :state 0
				     :wire (make-instance 'hw:wire)))
	 (en (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock (hw:wire clk)
					     :enable (hw:wire en))))
    ;; connect the connector
    (hw:connect-pins bus-connector bus)
    (setf (hw:pin-states bus-connector) :reading)

    (setf (hw::ring-counter-count rc) 9)
    (is (equal (hw:pins-value bus-connector) #2r0001))))


(test test-rc-reset
  "Test the counter resets correctly."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (make-instance 'hw:connector :width 4))
	 (clk (make-instance 'hw:pin :state 0
				     :wire (make-instance 'hw:wire)))
	 (en (make-instance 'hw:pin :state 0
				    :wire (make-instance 'hw:wire)))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock (hw:wire clk)
					     :enable (hw:wire en))))
    ;; connect the connector
    (hw:connect-pins bus-connector bus)
    (setf (hw:pin-states bus-connector) :reading)

    ;; clock the counter twice
    (setf (hw:state en) 1)
    (is (equal (hw:pins-value bus-connector) #2r0001))
    (dotimes (i 2)
      (setf (hw:state clk) 1)
      (setf (hw:state clk) 0))
    (is (equal (hw:pins-value bus-connector) #2r0100))

    ;; then reset
    (hw::ring-counter-reset rc)
    (is (equal (hw:pins-value bus-connector) #2r0001))))
