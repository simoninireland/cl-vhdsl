;; Tests of controllers and micro-instructions
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


(defclass test-core (component clocked)
  ((A
    :type hw:register)
   (address-bus
    :pins 4)
   )
  (:wiring
   ((A clock) (ram clock) clock)
   ((A data-bus) (ram data-bus)))
  (:metaclass hw:metacomponent))


(test test-controller-read-ram
  "Test we can load a register from RAM."
  (let* ((data-bus (make-instance 'hw:bus :width 8))
	 (data-bus-connector (make-instance 'hw:connector :width 8))
	 (address-bus (make-instance 'hw:bus :width 16))
	 (address-bus-connector (make-instance 'hw:connector :width 16))
	 (clk (make-instance 'hw:pin :state 0
				     :wire (make-instance 'hw:wire)))
	 (reg-en (make-instance 'hw:pin :state 0
					:wire (make-instance 'hw:wire)))
	 (reg-wr (make-instance 'hw:pin :state 0
					:wire (make-instance 'hw:wire)))
	 (ram-en (make-instance 'hw:pin :state 0
					:wire (make-instance 'hw:wire)))
	 (ram-wr (make-instance 'hw:pin :state 0
					:wire (make-instance 'hw:wire)))
	 (reg (make-instance 'hw:register :width 8
					  :clock (hw:wire clk)
					  :enable (hw:wire reg-en)
					  :bus data-bus
					  :write-enable (hw:wire reg-wr)))
	 (ram (make-instance 'hw:ram :address-bus-width 16
				     :data-bus-width 8
				     :address-bus address-bus
				     :data-bus data-bus
				     :clock (hw:wire clk)
				     :enable (hw:wire ram-en)
				     :write-enable (hw:wire ram-wr))))
    ;; connect the connectors
    (hw:connect-pins data-bus-connector data-bus)
    (setf (hw:pin-states data-bus-connector) :reading)
    (hw:connect-pins address-bus-connector address-bus)
    (hw:ensure-fully-wired reg ram)

    ;; put some values into RAM
    (setf (aref (hw:ram-elements ram) #16rFF) 123)

    ;; set up the register to be written to
    (setf (hw:state reg-en) 1)
    (setf (hw:state reg-wr) 1)

    ;; set up the RAM to be read from
    (setf (hw:state ram-wr) 0)
    (setf (hw:state ram-en) 1)

    ;; put the address onto the address bus
    (setf (hw:pins-value address-bus-connector) #16rFF)

    ;; clock the system
    (setf (hw:state clk) 1)
    (setf (hw:state clk) 0)

    ;; check we loaded the value
    (is (equal (aref (hw:ram-elements ram) #16rFF) 123))
    (is (equal (hw:register-value reg) 123))))
