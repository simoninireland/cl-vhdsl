;; Tests of micro-instructions
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


(defclass test-system (hw:component hw:clocked)
  ((data-bus
    :initarg :data-bus
    :reader data-bus)
   (address-bus
    :initarg :address-bus
    :reader address-bus)
   (reg
    :type hw:register
    :initarg :register
    :accessor register)
   (memory
    :type hw:ram
    :initarg :memory
    :accessor memory))
  (:metaclass hw:metacomponent))


(defmethod initialize-instance :after ((ts test-system) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))

  (let* ((clk (make-instance 'hw:pin :state 0))
	 (reg-en (make-instance 'hw:pin :state 0))
	 (reg-wr (make-instance 'hw:pin :state 0))
	 (ram-en (make-instance 'hw:pin :state 0))
	 (ram-wr (make-instance 'hw:pin :state 0))
	 (reg (make-instance 'hw:register :width 8
					  :clock clk
					  :enable reg-en
					  :bus (data-bus ts)
					  :write-enable reg-wr))
	 (ram (make-instance 'hw:ram :address-bus-width 16
				     :data-bus-width 8
				     :address-bus (address-bus ts)
				     :data-bus (data-bus ts)
				     :clock clk
				     :enable ram-en
				     :write-enable ram-wr)))
    (setf (register ts) reg)
    (setf (memory ts) ram)))


;; This code is needed to ensure that the classes exist (and
;; can be examined) even when they're built in the same file.
;; It can be avoided by instanciating an instance, which does
;; fianlisation implicitly -- but we'd be likely to generate
;; a system and then its micro-instructions without necessarily
;; instanciating first, so maybe `defmicroinstruction' should
;; implicitly finalise its base class as well?
(c2mop:ensure-finalized (find-class 'test-system))
(hw:defmicroinstruction tsm (test-system))
(c2mop:ensure-finalized (find-class 'tsm))


(test test-microinstructions
  "Test we can derive the micro-instructions for the test system."
  ;; check the micro-instruction slots all exist (there are other slots too)
  (let ((mi-slots (mapcar #'c2mop:slot-definition-name
			  (c2mop:class-slots (find-class 'tsm)))))
    (dolist (mi-slot '(reg/enable reg/write-enable memory/enable memory/write-enable))
      (is (member mi-slot mi-slots)))))


(test test-mi-load-register
  "Test we can crerate and run a micro-instruction that loads the register from RAM."
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
	 (ts (make-instance 'test-system :address-bus address-bus
					 :data-bus data-bus))
	 (clk (hw:component-clock ts)))
    ;; create the micro-instuction
    (let ((mi (make-instance 'tsm :reg/enable 1
				  :reg/write-enable 1
				  :memory/enable 1
				  :memory/write-enable 0)))

      ;; put some values into RAM
      (setf (aref (hw:ram-elements (memory ts)) #16rFF) 123)

      ;; put the address onto the address bus
      (setf (hw:connector-pins-value address-bus-connector) #16rFF)

      ;; clock-cycle the system
      (setf (hw:pin-state clk) 0)
      (hw:run-microinstruction mi)
      (setf (hw:pin-state clk) 1)

      ;; check we loaded the value
      (is (equal (aref (hw:ram-elements (memory ts)) #16rFF) 123))
      (is (equal (hw:register-value (register ts)) 123)))))
