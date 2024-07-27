;; Tests of ring counters
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


(test test-rc-initial
  "Test the initial state of the counter."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (map 'vector
			     (lambda (w)
			       (make-instance 'hw:pin :wire w :state :reading))
			     (hw:bus-wires bus)))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus)))
    (is (equal (hw:pins-to-value bus-connector) #2r0001))))


(test test-rc-inc
  "Test the counter increments correctly."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (map 'vector
			     (lambda (w)
			       (make-instance 'hw:pin :wire w :state :reading))
			     (hw:bus-wires bus)))
	 (clk (make-instance 'hw:pin :state 0))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock clk)))

    ;; clock the counter twice
    (dotimes (i 2)
      (setf (hw:pin-state clk) 1)
      (setf (hw:pin-state clk) 0))

    (is (equal (hw:pins-to-value bus-connector) #2r0100))))


(test test-rc-wrap
  "Test the counter wraps-around."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (map 'vector
			     (lambda (w)
			       (make-instance 'hw:pin :wire w :state :reading))
			     (hw:bus-wires bus)))
	 (clk (make-instance 'hw:pin :state 0))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock clk)))

    ;; clock the counter four times
    (dotimes (i 4)
      (setf (hw:pin-state clk) 1)
      (setf (hw:pin-state clk) 0))

    (is (equal (hw:pins-to-value bus-connector) #2r0001))))


(test test-rc-bounded
  "Test the counter can't be set too high."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (map 'vector
			     (lambda (w)
			       (make-instance 'hw:pin :wire w :state :reading))
			     (hw:bus-wires bus)))
	 (clk (make-instance 'hw:pin :state 0))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock clk)))

    (setf (hw::ring-counter-count rc) 9)
    (is (equal (hw:pins-to-value bus-connector) #2r0001))))


(test test-rc-reset
  "Test the counter resets correctly."
  (let* ((bus (make-instance 'hw:bus :width 4))
	 (bus-connector (map 'vector
			     (lambda (w)
			       (make-instance 'hw:pin :wire w :state :reading))
			     (hw:bus-wires bus)))
	 (clk (make-instance 'hw:pin :state 0))
	 (rc (make-instance 'hw:ring-counter :width 4
					     :bus bus
					     :clock clk)))

    ;; clock the counter twice
    (dotimes (i 2)
      (setf (hw:pin-state clk) 1)
      (setf (hw:pin-state clk) 0))
    (is (equal (hw:pins-to-value bus-connector) #2r0100))

    ;; then reset
    (hw:ring-counter-reset rc)
    (is (equal (hw:pins-to-value bus-connector) #2r0001))))
