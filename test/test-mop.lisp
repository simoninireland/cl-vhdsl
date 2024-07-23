;; Tests of metaclass for componens
;;
;; Copyright (C) 2023 Simon Dobson
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


(defclass test-component ()
  ((data-bus
    :initarg :data-bus
    :pins 8
    :role :io)
   (address-bus
    :pins 16
    :role :io)
   (clk
    :pins 1
    :role :trigger)
   (write-enable
    :pins 1
    :role :control)
   (io
    :pins 2)
   (other
    :initform 32))
  (:metaclass hw:metacomponent))


(test test-pins-created
  "Test we create pin for the slots on the pin interface"
  (let ((tc  (make-instance 'test-component)))
    (with-slots (data-bus address-bus clk write-enable io other) tc
      ;; 8-bit data bus, tristated
      (is (length data-bus) 8)
      (dolist (i (iota 8))
	(is (equal (slot-value (elt data-bus i) 'hw::state) :tristate)))

      ;; 16-bit address bus, tristated
      (is (length address-bus) 8)
      (dolist (i (iota 16))
	(is (equal (slot-value (elt address-bus i) 'hw::state) :tristate)))

      ;; clock pin, triggering
      (equal (slot-value clk 'hw::state) :trigger)

      ;; write-enable pin, reading
      (equal (slot-value write-enable 'hw::state) :reading)

      ;; 2 io lines, tristated
      (is (length io) 2)
      (dolist (i (iota 2))
	(is (equal (slot-value (elt io i) 'hw::state) :tristate)))

      ;; other left alone
      (equal other 32))))
