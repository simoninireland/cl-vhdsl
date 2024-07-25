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
    :initarg :io
    :pins 2)
   (other
    :initform 32))
  (:metaclass hw:metacomponent))


(defclass test-component-var ()
  ((width
    :initarg :width)
   (bus
    :pins width))
  (:metaclass hw:metacomponent))


(test test-pins-created
  "Test we create pins for the slots on the pin interface"
  (let ((tc (make-instance 'test-component)))
    (with-slots (data-bus address-bus clk write-enable io other) tc
      ;; 8-bit data bus, tristated
      (is (equal (length data-bus) 8))
      (is (equal (hw:pin-role-for-slot tc 'data-bus) :io))
      (is (equal (hw:pins-for-slot tc 'data-bus) 8))
      (dolist (i (iota 8))
	(is (equal (slot-value (elt data-bus i) 'hw::state) :tristate)))

      ;; 16-bit address bus, tristated
      (is (equal (length address-bus) 16))
      (is (equal (hw:pin-role-for-slot tc 'address-bus) :io))
      (is (equal (hw:pins-for-slot tc 'address-bus) 16))
      (dolist (i (iota 16))
	(is (equal (slot-value (elt address-bus i) 'hw::state) :tristate)))

      ;; clock pin, triggering
      (equal (slot-value clk 'hw::state) :trigger)
      (is (equal (hw:pin-role-for-slot tc 'clk) :trigger))

      ;; write-enable pin, reading
      (equal (slot-value write-enable 'hw::state) :reading)
      (is (equal (hw:pin-role-for-slot tc 'write-enable) :control))

      ;; 2 io lines, tristated
      (is (equal (length io) 2))
      (is (equal (hw:pin-role-for-slot tc 'io) :io))
      (is (equal (hw:pins-for-slot tc 'io) 2))
      (dolist (i (iota 2))
	(is (equal (slot-value (elt io i) 'hw::state) :tristate)))

      ;; other left alone
      (equal other 32))))


(test test-pin-interface
  "Test we can extract the pin interface."
  (let* ((tc (make-instance 'test-component))
	 (slots (hw:pin-interface tc)))
    (is (equal (length slots) 5))
    (dolist (s '(data-bus address-bus clk write-enable io))
      (is (member s slots)))))


(test test-pin-interface-p
  "Test we can test that a slot is in the pin interface."
  (let ((tc (make-instance 'test-component)))
    (is (hw:pin-interface-p tc 'data-bus))
    (is (not (hw:pin-interface-p  tc 'other)))

    ;; doesn't distinguish non-slots from non-pin slots
    (is (not (hw:pin-interface-p  tc 'not-a-slot-at-all)))))


(test test-pins-slot
  "Test we can extract the pins from slots."
  (let ((tc (make-instance 'test-component)))
    (is (equal (hw:pins-for-slot tc 'data-bus) 8))
    (is (equal (hw:pins-for-slot tc 'address-bus) 16))
    (is (equal (hw:pins-for-slot tc 'clk) 1))
    (is (equal (hw:pins-for-slot tc 'write-enable) 1))
    (is (equal (hw:pins-for-slot tc 'io) 2))
    (signals (error)
      (hw:pins-for-slot tc 'other))))


(test test-pins-from-slot
  "Test we can set the number of pins from the value of another slot."
  (let ((tc (make-instance 'test-component-var :width 8)))
    (is (equal (hw:pins-for-slot tc 'bus) (slot-value tc 'width)))))
