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


;; ---------- Registers ----------

(let* ((data-bus (make-instance 'hw:bus :width 8))
       (data-bus-connector (map 'vector
				(lambda (w)
				  (make-instance 'hw:pin :wire w :state 0))
				(hw:bus-wires data-bus)))
       (clk (make-instance 'hw:pin :wire (make-instance 'hw:wire)))
       (en (make-instance 'hw:pin :wire (make-instance 'hw:wire)))
       (wr (make-instance 'hw:pin :wire (make-instance 'hw:wire)))
       (reg (make-instance'hw:register :width 8
				       :bus data-bus
				       :clock (hw:pin-wire clk)
				       :enable (hw:pin-wire en)
				       :write-enable (hw:pin-wire wr))))

  ;; set all the control pins to the right values
  (setf (hw:pin-state clk) 0)
  (setf (hw:pin-state en) 0)
  (setf (hw:pin-state wr) 0)

  ;; put a value on the bus
  (setf (hw:pin-state (elt data-bus-connector 0)) 1)
  (setf (hw:pin-state (elt data-bus-connector 2)) 1)

  ;; set up write enable and enable the register
  (setf (hw:pin-state en) 1)
  (setf (hw:pin-state wr) 1)

  )
