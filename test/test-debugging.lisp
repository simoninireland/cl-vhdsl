;; Tests of the debugging functions
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


;; ---------- Component visibility ----------

(defclass test-of-wire (hw:component)
  ((pin
    :initarg :pin
    :pins 1
    :role :io))
  (:metaclass hw:metacomponent))


(test test-seen-wire
  "Test we can see the components attached to a wire."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w))
	 (tw (make-instance 'test-of-wire :pin p)))
    (let ((seen (hw:components-seen-by w)))
      (is (equal (length seen) 1))
      (is (member tw seen)))))


(defclass test-of-bus (hw:component)
  ((bus
    :initarg :bus
    :pins t
    :role :io))
  (:metaclass hw:metacomponent))


(test test-seen-bus
  "Test we can see the components attached to a bus."
  (let* ((b (make-instance 'hw:bus :width 4))
	 (p (make-instance 'hw:pin :wire (aref (hw:bus-wires b) 0)))
	 (tw1 (make-instance 'test-of-bus :bus b))
	 (tw2 (make-instance 'test-of-bus :bus b)))

    ;; test what the bus sees
    (let ((seen (hw:components-seen-by b)))
      (is (equal (length seen) 2))
      (is (member tw1 seen))
      (is (member tw2 seen)))

    ;; test the pin sees the same components
    (let ((seen (hw:components-seen-by p)))
      (is (equal (length seen) 2))
      (is (member tw1 seen))
      (is (member tw2 seen)))

    ;; test what the components see
    (let ((seen (hw:components-seen-by tw1)))
      (is (equal (length seen) 1))
      (is (member tw2 seen)))
    (let ((seen (hw:components-seen-by tw2)))
      (is (equal (length seen) 1))
      (is (member tw1 seen)))
    ))
