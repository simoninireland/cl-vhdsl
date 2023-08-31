;; test-bus.lisp: Tests of wires and buses
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

;; ---------- Bitfields to and from lists ----------

(test test-explode-bitfields
  "Test we can convert numbers to lists."
  (is (equal (cl-vhdsl::explode-bitfield 0)
	     '(0)))
  (is (equal (cl-vhdsl::explode-bitfield 1)
	     '(1)))
  (is (equal (cl-vhdsl::explode-bitfield #2r10)
	     '(0 1)))
  (is (equal (cl-vhdsl::explode-bitfield #2r1011)
	     '(1 1 0 1))))


(test test-implode-bitfields
  "Test we can create numbers from lists."
  (is (equal (cl-vhdsl::implode-bitfield '())
	     0))
  (is (equal (cl-vhdsl::implode-bitfield '(0))
	     0))
  (is (equal (cl-vhdsl::implode-bitfield '(1))
	     1))
  (is (equal (cl-vhdsl::implode-bitfield '(0 1))
	     #2r10))
  (is (equal (cl-vhdsl::implode-bitfield '(1 0 1 1 0))
	     #2r1101)))


(test test-implode-explode-bitfields
  "Test that implosion is the inverse of explosion."
  (is (equal (cl-vhdsl::implode-bitfield (cl-vhdsl::explode-bitfield 0))
	     0))
  (is (equal (cl-vhdsl::implode-bitfield (cl-vhdsl::explode-bitfield 1))
	     1))
  (is (equal (cl-vhdsl::implode-bitfield (cl-vhdsl::explode-bitfield #2r1011101101))
	     #2r1011101101)))


(test test-implode-explode-indices
  "Test we can implode and explode specific bit indices."
  ;; exploding -- extracting specific bits
  (is (equal (cl-vhdsl::explode-bitfield-indices #2r10110 '())
	     '()))
  (is (equal (cl-vhdsl::explode-bitfield-indices #2r10110 '(0 1))
	     '(0 1)))
  (is (equal (cl-vhdsl::explode-bitfield-indices #2r10110 '(0 4 5))
	     '(0 1 0)))

  ;; imploding -- setting specific bits (making others 0)
  (is (equal (cl-vhdsl::implode-bitfield-indices '(0 1 0 0 1) '())
	     0))
  (is (equal (cl-vhdsl::implode-bitfield-indices '(0 1 0 0 1) '(1))
	     #2r10))
  (is (equal (cl-vhdsl::implode-bitfield-indices '(0 1 0 0 1) '(1 2))
	     #2r10))
  (is (equal (cl-vhdsl::implode-bitfield-indices '(0 1 0 0 1) '(1 2 4))
	     #2r10010)))


(test test-fix-bitfield-width
  "Test we can fix bitfield widths."
  (is (equal (cl-vhdsl::fix-bitfield-width '(0) 1)
	     '(0)))
  (is (equal (cl-vhdsl::fix-bitfield-width '(1) 1)
	     '(1)))
  (is (equal (cl-vhdsl::fix-bitfield-width '(1 0) 1)
	     '(1 0)))
  (is (equal (cl-vhdsl::fix-bitfield-width '(1 0 1) 5)
	     '(1 0 1 0 0))))

(test test-change-bit-indices
  "Test we can find the bits that have changed between two numbers."
  (is (equal (cl-vhdsl::changed-bit-indices #2r1 #2r1)
	     '()))
  (is (equal (cl-vhdsl::changed-bit-indices #2r1 #2r0)
	     '(0)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r101)
	     '()))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r100)
	     '(0)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r111)
	     '(1)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r110)
	     '(0 1)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r010)
	     '(0 1 2)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r001)
	     '(2)))
  (is (equal (cl-vhdsl::changed-bit-indices #2r101 #2r1101)
	     '(3))))

(test test-get-set-bits
  "Test we can get and set individual bits."
  (is (equal (cl-vhdsl::get-bit #2r1010 0)
	     0))
  (is (equal (cl-vhdsl::get-bit #2r1010 1)
	     1))
  (is (equal (cl-vhdsl::get-bit #2r1010 7)
	     0))
  (is (equal (cl-vhdsl::set-bit #2r1010 0 0)
	     #2r1010))
  (is (equal (cl-vhdsl::set-bit #2r1010 0 1)
	     #2r1011))
  (is (equal (cl-vhdsl::set-bit #2r1010 1 0)
	     #2r1000))
  (is (equal (cl-vhdsl::set-bit #2r1010 3 0)
	     #2r10))
  (is (equal (cl-vhdsl::set-bit #2r1010 3 1)
	     #2r1010)))

(test test-update-bit-indices
  "Test we can update individually-indexed bits of a number."
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '() '())
	     #2r1010))
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '(1) '(0))
	     #2r1011))
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '(1 0) '(0 1))
	     #2r1001))
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '(1) '(2))
	     #2r1110))
  (is (equal (cl-vhdsl::update-bit-indices #2r1010 '(1) '(3))
	     #2r1010)))


;; ---------- Bus wires and names ----------

(test test-bus-indices
  "Test we create pin indices correctly."
  (let ((b (make-instance 'bus :width 8)))
    (is (equal (cl-vhdsl::bus-wire-indices b)
	       (alexandria:iota 8 :start 0))))
  (let ((b (make-instance 'bus :width 16)))
    (is (equal (cl-vhdsl::bus-wire-indices b)
	       (alexandria:iota 16 :start 0)))))


(test test-bus-wire-names
  "Test we can refer to wires by name."
  (let ((b (make-instance 'bus :width 4)))
    (setf (bus-wire-names b) '(d0 d1 d2 d3))

    ;; wire name retrieval
    (is (equal (bus-wire-names b)
	       '(d0 d1 d2 d3)))

    ;; single name lookup
    (is (equal (cl-vhdsl::bus-wire-name-index b 'd0) 0))

    ;; index lookup returns the index
    (is (equal (cl-vhdsl::bus-wire-name-index b 0) 0))
    (is (equal (cl-vhdsl::bus-wire-name-index b 3) 3))

    ;; out-of-range indices and unknown names
    (signals error
      (cl-vhdsl::bus-wire-name-index b 4))
    (signals error
      (cl-vhdsl::bus-wire-name-index b 'd5))

    ;; multiple name lookup
    (is (equal (cl-vhdsl::bus-wire-name-indices b '(d0 d1 d3 d2))
	       '(0 1 3 2)))))


(test test-bus-pin-wire-assignments
  "Test we can connect pins to wires"
  ;; no pins connected
  (let ((b (make-instance 'bus :width 8)))
    (is (equal (bus-pins-connected-to-wire b 0)
	       '())))

  ;; simple connections
  (let ((b (make-instance 'bus :width 8))
	(p (make-instance 'pin)))
    (bus-connect b 0 p)
    (is (equal (bus-pins-connected-to-wire b 0)
	       (list p))))
  (let ((b (make-instance 'bus :width 8))
	(p0 (make-instance 'pin))
	(p1 (make-instance 'pin)))
    (bus-connect b 0 p0)
    (bus-connect b 0 p1)
    (is (equal (bus-pins-connected-to-wire b 0)
	       (list p0 p1))))
  (let ((b (make-instance 'bus :width 8))
	(p0 (make-instance 'pin))
	(p1 (make-instance 'pin)))
    (bus-connect b 0 p0)
    (bus-connect b 1 p1)
    (is (equal (bus-pins-connected-to-wire b 0)
	       (list p0)))
    (is (equal (bus-pins-connected-to-wire b 1)
	       (list p1))))

  ;; connections to named pins
  (let ((b (make-instance 'bus :width 4))
	(p0 (make-instance 'pin))
	(p1 (make-instance 'pin)))
    (setf (bus-wire-names b) '(d0 d1 d2 d3))
    (bus-connect b 'd0 p0)
    (bus-connect b 'd2 p1)
    (is (equal (bus-pins-connected-to-wire b 'd0)
	       (list p0)))
    (is (equal (bus-pins-connected-to-wire b 'd1)
	       '()))
    (is (equal (bus-pins-connected-to-wire b 'd2)
	       (list p1)))
    (is (equal (bus-pins-connected-to-wire b 'd3)
	       '()))))


(test test-bus-set-wires-simple
  "Test we can set the values on wires. No propagation."
  (let ((b (make-instance 'bus :width 4)))

    ;; set value in one go
    (bus-set-wire-value b #2r1100)
    (is (equal (slot-value b 'cl-vhdsl::wires) #2r1100))

    ;; no update
    (bus-set-wire-values b '() '())
    (is (equal (slot-value b 'cl-vhdsl::wires) #2r1100))
    (bus-set-wire-values b '(0 0) '(0 1))
    (is (equal (slot-value b 'cl-vhdsl::wires) #2r1100))

    ;; update individual wires
    (bus-set-wire-values b '(0 1) '(2 0))
    (is (equal (slot-value b 'cl-vhdsl::wires) #2r1001))))


(test test-bus-set-wires
  "Test we can set wire values and have them propagate to connected pins."
  (let ((b (make-instance 'bus :width 4))
	(p0 (make-instance 'pin))
	(p1 (make-instance 'pin))
	(p2 (make-instance 'pin)))
    (bus-connect b 0 p0)
    (bus-connect b 2 p1)
    (bus-connect b 2 p2)

    (bus-set-wire-value b #2r1)
    (is (equal (pin-value p0) 1))
    (is (equal (pin-value p1) 0))
    (is (equal (pin-value p2) 0))

    (bus-set-wire-value b #2r101)
    (is (equal (pin-value p0) 1))
    (is (equal (pin-value p1) 1))
    (is (equal (pin-value p2) 1))))


(test bus-set-wire-triggers
  "Test we can set bus wire values and only trigger on change of value."
  ;; simple trigger
  (let ((b (make-instance 'bus :width 4))
	(p0 (make-active-high-trigger))
	(v 0))
    (bus-connect b 0 p0)
    (trigger-set-behaviour p0 (lambda (trig)
				(incf v)
				trig))

    ;; no change when wire set again to 0
    (bus-set-wire-value b #2r0)
    (is (equal v 0))

    ;; positive edge causes increment
    (bus-set-wire-value b #2r1)
    (is (equal v 1))

    ;; negative edge does notiong
    (bus-set-wire-value b #2r0)
    (is (equal v 1))

    ;; positive edge causes increment
    (bus-set-wire-value b #2r1)
    (is (equal v 2)))

  ;; alternating triggers on the same pin
  (let ((b (make-instance 'bus :width 4))
	(p0 (make-active-high-trigger))
	(p1 (make-active-low-trigger))
	(v 0)
	(w 0))
    (bus-connect b 0 p0)
    (trigger-set-behaviour p0 (lambda (trig)
				(incf v)
				trig))
    (bus-connect b 0 p1)
    (trigger-set-behaviour p1 (lambda (trig)
				(incf w)
				trig))

    ;; clock low (again -- no update)
    (bus-set-wire-value b #2r0)
    (is (equal (list v w) '(0 0)))

    ;; clock high
    (bus-set-wire-value b #2r1)
    (is (equal (list v w) '(1 0)))

    ;; clock low
    (bus-set-wire-value b #2r0)
    (is (equal (list v w) '(1 1)))

    ;; clock high
    (bus-set-wire-value b #2r1)
    (is (equal (list v w) '(2 1))))

  ;; two triggers on different pins
  (let ((b (make-instance 'bus :width 4))
	(p0 (make-active-high-trigger))
	(p1 (make-active-high-trigger))
	(v 0)
	(w 0))
    (bus-connect b 0 p0)
    (trigger-set-behaviour p0 (lambda (trig)
				(incf v)
				trig))
    (bus-connect b 2 p1)
    (trigger-set-behaviour p1 (lambda (trig)
				(incf w)
				trig))

    ;; no update
    (bus-set-wire-value b #2r0)
    (is (equal (list v w) '(0 0)))

    ;; fire first trigger
    (bus-set-wire-value b #2r1)
    (is (equal (list v w) '(1 0)))

    ;; fire second trigger
    (bus-set-wire-value b #2r100)
    (is (equal (list v w) '(1 1)))

    ;; re-fire second trigger
    (bus-set-wire-value b #2r0)
    (is (equal (slot-value b 'cl-vhdsl::wires) #2r0))
    (bus-set-wire-value b #2r100)
    (is (equal (slot-value b 'cl-vhdsl::wires) #2r100))
    (is (equal (list v w) '(1 2)))))
