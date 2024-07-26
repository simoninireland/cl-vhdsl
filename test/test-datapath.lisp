;; Tests of composed datapaths
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


(test test-read-ram
  "Test we can load a register from RAM."
  (let* ((data-bus (make-instance 'hw:bus :width 8))
	 (data-bus-connector (map 'vector
				  (lambda (w)
				    (make-instance 'hw:pin :wire w))
				  (hw:bus-wires data-bus)))
	 (address-bus (make-instance 'hw:bus :width 16))
	 (address-bus-connector (map 'vector
				     (lambda (w)
				       (make-instance 'hw:pin :wire w))
				     (hw:bus-wires address-bus)))
	 (clk (make-instance 'hw:pin :state 0))
	 (reg-en (make-instance 'hw:pin :state 0))
	 (reg-wr (make-instance 'hw:pin :state 0))
	 (ram-en (make-instance 'hw:pin :state 0))
	 (ram-wr (make-instance 'hw:pin :state 0))
	 (reg (make-instance 'hw:register :width 8
					  :clock clk
					  :enable reg-en
					  :bus data-bus
					  :write-enable reg-wr))
	 (ram (make-instance 'hw:ram :address-bus-width 16
				     :data-bus-width 8
				     :address-bus address-bus
				     :data-bus data-bus
				     :clock clk
				     :enable ram-en
				     :write-enable ram-wr)))

    ;; put some values into RAM
    (setf (aref (hw:ram-elements ram) #16rFF) 123)

    ;; set up the register to be written to
    (setf (hw:pin-state reg-en) 1)
    (setf (hw:pin-state reg-wr) 1)

    ;; set up the RAM to be read from
    (setf (hw:pin-state ram-wr) 0)
    (setf (hw:pin-state ram-en) 1)

    ;; put the addres onto the address bus
    (hw:pins-from-value address-bus-connector #16rFF)

    ;; clock the system
    (setf (hw:pin-state clk) 1)

    ;; check we loaded the value
    (is (equal (aref (hw:ram-elements ram) #16rFF) 123))))
