;; Tests of micro-instructions
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


(defclass test-system-microinstruction (hw:microinstruction)
  ()
  (:documentation "Microinstruction for test-system class."))

;;(hw:defmicroinstruction test-system-mi test-system)


(test test-mi-control-pin-interface
  "Test we can extract the control pin interface from a component."
  (let* ((tc (make-instance 'test-system
			    :memory (make-instance 'hw:ram :address-bus-width 8
							   :data-bus-width 8)
			    :register (make-instance 'hw:register :width 8)))
	 (cl (class-of tc)))
    (is (set-equal (hw::control-pin-endpoints cl 'reg)
		   '((reg hw:enable)
		     (reg hw::write-enable))
		   :test #'equal))
    (is (set-equal (hw::control-pin-endpoints cl 'memory)
		   '((memory hw:enable)
		     (memory hw::write-enable))
		   :test #'equal))
    (is (set-equal (hw::subcomponent-control-pin-endpoints cl)
		   '((reg hw:enable)
		     (reg hw::write-enable)
		     (memory hw:enable)
		     (memory hw::write-enable))
		   :test #'equal))))


(test test-mi-microinstruction
  "Test we can create microinstructions to control a component."
  (let* ((tc (make-instance 'test-system
			    :memory (make-instance 'hw:ram :address-bus-width 8
							   :data-bus-width 8)
			    :register (make-instance 'hw:register :width 8))))
    (hw:defmicroinstruction test-u test-system)

    )
  )
