;; Tests of metaclass for componens
;;
;; Copyright (C) 2023 Simon Dobson
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


;; ---------- Components ----------

(defclass test-component (hw:component hw:enabled hw:clocked)
  ((data-bus
    :initarg :data-bus
    :pins 8
    :role :io)
   (address-bus
    :pins 16
    :role :io)
   (write-enable
    :pins 1
    :role :control)
   (io
    :initarg :io
    :pins 2)
   (other
    :initform 32))
  (:metaclass hw:metacomponent))


(defclass test-component-var (hw:component hw:enabled)
  ((width
    :initarg :width)
   (bus
    :pins width))
  (:metaclass hw:metacomponent))


(test test-pins-created
  "Test we create pins for the slots on the pin interface"
  (let ((tc (make-instance 'test-component)))
    (with-slots (data-bus address-bus hw::clock write-enable io other) tc
      ;; 8-bit data bus, tristated
      (is (equal (hw:width data-bus) 8))
      (dolist (i (iota 8))
	(is (equal (slot-value (elt (hw:pins data-bus) i) 'hw::state) :tristate)))

      ;; 16-bit address bus, tristated
      (is (equal (hw:width address-bus) 16))
      (dolist (i (iota 16))
	(is (equal (slot-value (elt (hw:pins address-bus) i) 'hw::state) :tristate)))

      ;; clock pin, triggering
      (equal (slot-value (elt (hw:pins hw::clock) 0) 'hw::state) :trigger)

      ;; write-enable pin, reading
      (equal (slot-value (elt (hw:pins write-enable) 0) 'hw::state) :reading)

      ;; 2 io lines, tristated
      (is (equal (hw:width io) 2))
      (dolist (i (iota 2))
	(is (equal (slot-value (elt (hw:pins io) i) 'hw::state) :tristate)))

      ;; other left alone
      (equal other 32))))


(test test-pin-interface
  "Test we can extract the pin interface."
  (let* ((tc (make-instance 'test-component))
	 (slots (hw:pin-interface (class-of tc))))
    (is (equal (length slots) 6))
    (dolist (s '(data-bus address-bus hw:enable hw::clock hw::enable write-enable io))
      (is (member s slots)))))


(test test-pin-interface-p
  "Test we can test that a slot is in the pin interface."
  (let ((tc (make-instance 'test-component)))
    (is (hw:pin-interface-p (class-of tc) 'data-bus))
    (is (not (hw:pin-interface-p  (class-of tc) 'other)))

    ;; doesn't distinguish non-slots from non-pin slots
    (is (not (hw:pin-interface-p (class-of tc) 'not-a-slot-at-all)))))


(test test-pins-slot
  "Test we can extract the pins from slots."
  (let ((tc (make-instance 'test-component)))
    (is (equal (hw:width (slot-value tc 'data-bus)) 8))
    (is (equal (hw:width (slot-value tc 'address-bus)) 16))
    (is (equal (hw:width (slot-value tc 'hw:clock)) 1))
    (is (equal (hw:width (slot-value tc 'write-enable)) 1))
    (is (equal (hw:width (slot-value tc 'io)) 2))))


(test test-pins-from-slot
  "Test we can set the number of pins from the value of another slot."
  (let ((tc (make-instance 'test-component-var :width 8)))
    (is (equal (hw:width (slot-value tc 'bus)) (slot-value tc 'width)))))


;; ----------  :if method combination ----------

(test test-guard-single-unguarded
  "Test an guarded method without a guard behaves as standard."
  (defgeneric test-unguarded (v)
    (:method-combination hw:guarded))

  (defmethod test-unguarded ((v integer))
    (+ v 12))

  (is (equal (test-unguarded 1) 13))
  (is (equal (test-unguarded 12) 24)))


(test test-guard-single-primary
  "Test we can guard a single primary method."
  (defgeneric test-single-primary (v)
    (:method-combination hw:guarded))

  (defmethod test-single-primary ((v integer))
    (+ v 12))

  (defmethod test-single-primary :if ((v integer))
    (> v 10))

  (is (null (test-single-primary 1)))
  (is (equal (test-single-primary 12) 24)))


(test test-guard-nested-primary
  "Test we can guard a nested primary method."
  (defgeneric test-nested-primary (v)
    (:method-combination hw:guarded))

  (defmethod test-nested-primary ((v integer))
    (call-next-method (+ v 12)))

  (defmethod test-nested-primary ((v number))
    (* v 1.5))

  (defmethod test-nested-primary :if ((v integer))
    (> v 10))

  (is (null (test-nested-primary 1)))
  (is (equal (test-nested-primary 12.0) 18.0))
  (is (equal (test-nested-primary 12) 36.0)))


(test test-guard-around
  "Test we can guard a method with :around methods."
  (defgeneric test-around (v)
    (:method-combination hw:guarded))

  (defmethod test-around ((v integer))
    (+ v 12))

  (defmethod test-around :around ((v integer))
    (call-next-method (* v 10)))

  (defmethod test-around :if ((v integer))
    (> v 10))

  (is (null (test-around 1)))
  (is (equal (test-around 12) 132)))


(test test-guard-before-after
  "Test we can guard :before and :after methods"
  (defparameter test-before-after-b 0)
  (defparameter test-before-after-a 0)

  (defgeneric test-before-after (v)
    (:method-combination hw:guarded))

  (defmethod test-before-after ((v integer))
    (+ v 12))

  (defmethod test-before-after :before ((v integer))
    (setq test-before-after-b v))

  (defmethod test-before-after :after ((v integer))
    (setq test-before-after-a v))

  (defmethod test-before-after :if ((v integer))
    (> v 10))

  (is (null (test-before-after 1)))
  (is (= test-before-after-b 0))
  (is (= test-before-after-a 0))

  (is (equal (test-before-after 12) 24))
  (is (= test-before-after-b 12))
  (is (= test-before-after-a 12)))


(test test-guard-before-after-around
  "Test we can compose :around with :before and :after"
  (defparameter test-before-after-around-b 0)
  (defparameter test-before-after-around-a 0)

  (defgeneric test-before-after-around (v)
    (:method-combination hw:guarded))

  (defmethod test-before-after-around ((v integer))
    (+ v 12))

  (defmethod test-before-after-around :before ((v integer))
    (setq test-before-after-around-b v))

  (defmethod test-before-after-around :after ((v integer))
    (setq test-before-after-around-a v))

  (defmethod test-before-after-around :around ((v integer))
    (call-next-method (* v 10)))

  (defmethod test-before-after-around :if ((v integer))
    (> v 10))

  (is (null (test-before-after-around 1)))
  (is (= test-before-after-around-b 0))
  (is (= test-before-after-around-a 0))

  (is (equal (test-before-after-around 12) 132))
  (is (= test-before-after-around-b 120))
  (is (= test-before-after-around-a 120)))


(test test-guarded-several
  "Test we can have several guards on the same method."
  (defgeneric test-several (v)
    (:method-combination hw:guarded))

  (defmethod test-several ((v integer))
    (+ v 12))

  ;; methods need to be specialised against different types,
  ;; so this adds two guards that will be selected by an integer
  ;; value
  (defmethod test-several :if ((v integer))
    (> v 10))
  (defmethod test-several :if ((v number))
    (< v 100))

  (is (null (test-several 1)))
  (is (null (test-several 101)))
  (is (equal (test-several 12) 24)))


(test test-guarded-hier
  "Test we can acquire :if guards from up and down an inheritance hierarchy."
  (defclass test-hier-c ()
    ((value
      :initform 9
      :initarg :value)))
  (defclass test-hier-c-1 (test-hier-c) ())
  (defclass test-hier-c-2 (test-hier-c-1) ())

  (defgeneric test-hier (v)
    (:method-combination hw:guarded))

  (defmethod test-hier ((v test-hier-c))
    (slot-value v 'value))

  (defmethod test-hier ((v test-hier-c-1))
    (+ (call-next-method) 26))

  (defmethod test-hier ((v test-hier-c-2))
    (+ (call-next-method) 1000))

  (defmethod test-hier :if ((v test-hier-c-2))
    (< (slot-value v 'value) 100))

  (defmethod test-hier :if ((v test-hier-c))
    (= (slot-value v 'value) 9))

  (is (equal (test-hier (make-instance 'test-hier-c)) 9))
  (is (equal (test-hier (make-instance 'test-hier-c-1)) 35))
  (is (equal (test-hier (make-instance 'test-hier-c-2)) 1035))

  (is (null (test-hier (make-instance 'test-hier-c-2 :value 200))))
  (is (null (test-hier (make-instance 'test-hier-c-2 :value 25)))))
