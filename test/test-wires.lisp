;; Tests of wire and pin operations
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


(test test-new-wire
  "Test a new wire is initially floating."
  (let ((w (make-instance 'hw:wire)))
    (is (equal (hw:wire-state w) :floating))))


(test test-assert-pin-floating
  "Test asserting a pin to an initial floating value."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (is (eql (hw:pin-wire p) w))
    (is (equal (slot-value p 'hw::state) :tristate))
    (is (equal (hw:wire-state w) :floating))
    (is (hw::wire-pin-asserting-p w p :tristate))))


(test test-add-pin-twice
  "Test we can't add a pin again to the same wire."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (signals (error)
      (setf (hw:pin-wire p) p))))


(test test-assert-pin
  "Test asserting a pin to a logic value."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p) 0)
    (is (equal (slot-value p 'hw::state) 0))
    (is (equal (hw:wire-state w) 0))))


(test test-assert-pin-same
  "Test asserting a pin to the same value again."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p) 0)
    (setf (hw:pin-state p) 0)
    (is (equal (slot-value p 'hw::state) 0))
    (is (equal (hw:wire-state w) 0))))


(test test-assert-pin-different
  "Test asserting a pin to a different value."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p) 0)
    (setf (hw:pin-state p) 1)
    (is (equal (slot-value p 'hw::state) 1))
    (is (equal (hw:wire-state w) 1))))


(test test-assert-pin-tristate
  "Test tri-stating the pin."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p) 0)
    (setf (hw:pin-state p) :tristate)
    (is (equal (slot-value p 'hw::state) :tristate))
    (is (equal (hw:wire-state w) :floating))))


(test test-assert-pin-reading
  "Test setting the pin to reading."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p) 0)
    (setf (hw:pin-state p) :reading)
    (is (equal (slot-value p 'hw::state) :reading))
    (is (equal (hw:wire-state w) :floating))))


(test test-assert-pins-same
  "Test asserting two pins to the same value."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p1) 0)
    (setf (hw:pin-state p2) 0)
    (is (equal (slot-value p1 'hw::state) 0))
    (is (equal (slot-value p2 'hw::state) 0))
    (is (equal (hw:wire-state w) 0))))


(test test-assert-pins-different
  "Test asserting two pins to different values."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w))
	 (p3 (make-instance 'hw:pin :wire w :state :reading)))
    (setf (hw:pin-state p1) 0)
    (setf (hw:pin-state p2) 1)   ; this send the wire floating
    (signals (hw:reading-floating-value)
      (hw:pin-state p3))))


(test test-assert-pins-tristated
  "Test asserting a pin when the other is tri-stated."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (slot-value p1 'hw::state) 1)))


(test test-assert-pins-reading
  "Test asserting a pin when the other is reading."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (slot-value p1 'hw::state) :reading)
    (setf (slot-value p2 'hw::state) 1)))


(test test-pin-read
  "Test reading a pin's state."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p1) 0)
    (setf (hw:pin-state p2) :reading)

    ;; state of the pin is the state of the wire
    (is (equal (hw:pin-state p2) 0))))


(test test-pin-not-read
  "Test reading the state of a non-reading pin."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p1) 0)

    ;; can only read from a :reading pin
    (dolist (v '(0 1 :tristate))
      (setf (hw:pin-state p2) v)
      (signals (error)
	(hw:pin-state p2)))))


(test test-pin-read-floating
  "Test reading a pin's state when its wire is floating."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (hw:pin-state p2) :reading)
    (signals (hw:reading-floating-value)
      (hw:pin-state p2))))


(test test-pins-floating
  "Test we can determine when any of a set of pins is floating."
  (let* ((w1 (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w1))
	 (w2 (make-instance 'hw:wire))
	 (p2 (make-instance 'hw:pin :wire w2))
	 (p3 (make-instance 'hw:pin :wire w2)))
    (setf (hw:pin-state p1) 1)
    (is (null (hw:pins-floating (list p1))))
    (is (hw:pins-floating (list p2 p3)))
    (is (hw:pins-floating (list p1 p2 p3)))

    (setf (hw:pin-state p2) 0)
    (is (null (hw:pins-floating (list p1 p2 p3))))))
