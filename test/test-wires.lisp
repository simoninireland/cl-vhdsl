;; Tests of wire and pin operations
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


(test test-new-wire
  "Test a new wire is initially floating."
  (let ((w (make-instance 'hw:wire)))
    (is (equal (hw:state w) :floating))))


(test test-assert-pin-floating
  "Test asserting a pin to an initial floating value."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (is (eql (hw:wire p) w))
    (is (equal (slot-value p 'hw::state) :tristate))
    (is (equal (hw:state w) :floating))
    (is (hw::wire-pin-asserting-p w p :tristate))))


(test test-add-pin-twice
  "Test we can't add a pin again to the same wire."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (signals (error)
      (setf (hw:wire p) p))))


(test test-assert-pin
  "Test asserting a pin to a logic value."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:state p) 0)
    (is (equal (slot-value p 'hw::state) 0))
    (is (equal (hw:state w) 0))))


(test test-assert-pin-same
  "Test asserting a pin to the same value again."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:state p) 0)
    (setf (hw:state p) 0)
    (is (equal (slot-value p 'hw::state) 0))
    (is (equal (hw:state w) 0))))


(test test-assert-pin-different
  "Test asserting a pin to a different value."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:state p) 0)
    (setf (hw:state p) 1)
    (is (equal (slot-value p 'hw::state) 1))
    (is (equal (hw:state w) 1))))


(test test-assert-pin-tristate
  "Test tri-stating the pin."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:state p) 0)
    (setf (hw:state p) :tristate)
    (is (equal (slot-value p 'hw::state) :tristate))
    (is (equal (hw:state w) :floating))))


(test test-assert-pin-reading
  "Test setting the pin to reading."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (setf (hw:state p) 0)
    (setf (hw:state p) :reading)
    (is (equal (slot-value p 'hw::state) :reading))
    (is (equal (hw:state w) :floating))))


(test test-assert-pins-same
  "Test asserting two pins to the same value."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (hw:state p1) 0)
    (setf (hw:state p2) 0)
    (is (equal (slot-value p1 'hw::state) 0))
    (is (equal (slot-value p2 'hw::state) 0))
    (is (equal (hw:state w) 0))))


(test test-assert-pins-different
  "Test asserting two pins to different values."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w))
	 (p3 (make-instance 'hw:pin :wire w :state :reading)))
    (setf (hw:state p1) 0)
    (setf (hw:state p2) 1)   ; this send the wire floating
    (signals (hw:reading-floating-value)
      (hw:state p3))))


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
    (setf (hw:state p1) 0)
    (setf (hw:state p2) :reading)

    ;; state of the pin is the state of the wire
    (is (equal (hw:state p2) 0))))


(test test-pin-not-read
  "Test reading the state of a non-reading pin."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (hw:state p1) 0)

    ;; test we can't read a :tristated pin
    (setf (hw:state p2) :tristate)
    (signals (hw:reading-non-reading-pin)
      (hw:state p2))))


(test test-pin-read-floating
  "Test reading a pin's state when its wire is floating."
  (let* ((w (make-instance 'hw:wire))
	 (p1 (make-instance 'hw:pin :wire w))
	 (p2 (make-instance 'hw:pin :wire w)))
    (setf (hw:state p2) :reading)

    ;; test we see the signal
    (signals (hw:reading-floating-value)
      (hw:state p2))))


(test test-pins-floating
  "Test we can determine when any of a set of pins is floating."
  (let* ((w1 (make-instance 'hw:wire))
	 (w2 (make-instance 'hw:wire))
	 (conn (make-instance 'hw:connector :width 3))
	 (ps (hw:pins conn)))
    (setf (hw:wire (elt ps 0)) w1)
    (setf (hw:wire (elt ps 1)) w2)
    (setf (hw:wire (elt ps 2)) w2)

    (setf (hw:state (elt ps 0)) 1)
    (is (hw:floating-p conn))

    (setf (hw:state (elt ps 1)) 0)
    (setf (hw:state (elt ps 2)) :reading)
    (is (null (hw:floating-p conn)))))
