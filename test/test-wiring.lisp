;; Tests of wiring components
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

;; ---------- Accessing qualified slots ----------

(defclass test-slotted (hw:component)
  ((one
    :pins 8)
   (two
    :pins 8
    :role :control)
   (three
    :pins 16
    :role :io))
  (:metaclass hw:metacomponent))


(defclass test-subcomponents (hw:component)
  ((four
    :pins 8)
   (c
    :initarg :c
    :type test-slotted
    :initarg :sub)
   (other
    :initform 24))
  (:metaclass hw:metacomponent))


;; ---------- Sub-components ----------

(test test-subcomponents
  "Test we can identify sub-component and non-sub-component slots."
  (let ((tc (make-instance 'test-subcomponents)))
    (is (hw:subcomponent-p (class-of tc) 'c))

    ;; test against non-component slots (including pin slots)
    (is (not (hw:subcomponent-p (class-of tc) 'other)))
    (is (not (hw:subcomponent-p (class-of tc) 'four)))))


(test test-subcomponent-interface
  "Test we can extract the sub-component interface."
  (let ((tc (make-instance 'test-subcomponents)))
    (is (equal (hw:subcomponent-interface (class-of tc))
	       (list 'c)))))


(test test-empty-subcomponent-interface
  "Test we can extract an empty sub-component interface."
  (let ((tc (make-instance 'test-slotted)))
    (is (null (hw:subcomponent-interface (class-of tc))))))


(test test-subcomponents-of-object
  "Test we can extract the sub-components of an object."
  (let* ((tc1 (make-instance 'test-slotted))
	 (tc2 (make-instance 'test-subcomponents :c tc1)))
    (is (equal (hw:components tc2)
	       (list tc1)))

    ;; test against the sub-component as well
    (is (null (hw:components tc1)))))


;; ---------- Wiring connectors and buses ----------

(test test-wire-slots-same-component
  "Test we can wire two pin interface slots on the same class together."
  (let ((tc (make-instance 'test-slotted))
	(b (make-instance 'hw:bus :width 8)))
    (hw:connect-pins (slot-value tc 'one) b)
    (hw:connect-pins (slot-value tc 'two) b)

    ;; bus can see the component
    (let ((cs (hw:components-seen-by b)))
      (is (equal (length cs) 1))
      (is (member tc cs)))

    ;; component doesn't see itself
    (is (null (hw:components-seen-by tc)))))


(test test-wire-slots-different-components
  "Test we can wire two slots on different components together."
  (let ((tc1 (make-instance 'test-slotted ))
	(tc2 (make-instance 'test-slotted))
	(b (make-instance 'hw:bus :width 8)))
    (hw:connect-pins (slot-value tc1 'one) b)
    (hw:connect-pins (slot-value tc2 'two) b)

    ;; bus can see both component
    (let ((cs (hw:components-seen-by b)))
      (is (equal (length cs) 2))
      (is (member tc1 cs))
      (is (member tc2 cs)))

    ;; components see each other, but not themselves
    (is (equal (hw:components-seen-by tc1) (list tc2)))
    (is (equal (hw:components-seen-by tc2) (list tc1)))))


(test test-wire-incomptible-widths
  "Test we can't connect slots with unequal widths."
  (let ((tc (make-instance 'test-slotted))
	(b (make-instance 'hw:bus :width 8)))
    (hw:connect-pins (slot-value tc 'one) b)
    (signals (hw:incompatible-pin-widths)
      (hw:connect-pins (slot-value tc 'three) b))))


;; ---------- Wiring slots ----------

(test test-wire-slots
  "Test we can wire two compatible slots."
  (let ((tc (make-instance 'test-slotted)))
    (hw:connect-slots tc (list 'one)
		      tc (list 'two))

    ;; component doesn't see itself
    (is (null (hw:components-seen-by tc)))))


(test test-wire-slots-sub
  "Test we can wire-up slots on sub-components."
  (let* ((tc1 (make-instance 'test-slotted ))
	 (tc2 (make-instance 'test-subcomponents :sub tc1)))
    (hw:connect-slots tc1 (list 'one)
		      tc2 (list 'c 'two))))


(test test-wire-slots-super-sub
  "Test we can wire-up slots between component and sub-somponent."
  (let* ((tc1 (make-instance 'test-slotted ))
	 (tc2 (make-instance 'test-subcomponents :sub tc1)))
    (hw:connect-slots tc2 (list 'four)
		      tc2 (list 'c 'two))))


(test test-wire-slots-widths
  "Test we can't wire slots with incompatible widths."
  (let ((tc (make-instance 'test-slotted)))
    (signals (hw:incompatible-pin-widths)
      (hw:connect-slots tc (list 'one)
			tc (list 'three)))))


(test test-wire-slots-widths-sub
  "Test we can't wire-up slots on sub-components with incompatible widths."
  (let* ((tc1 (make-instance 'test-slotted ))
	 (tc2 (make-instance 'test-subcomponents :sub tc1)))
    (signals (hw:incompatible-pin-widths)
      (hw:connect-slots tc1 (list 'one)
			tc2 (list 'c 'three)))))


(test test-wire-slots-missing
  "Test we can't wire missing slots."
  (let ((tc (make-instance 'test-subcomponents)))
    (signals (hw:non-pin-interface-slot)
      (hw:connect-slots tc (list 'four)
			tc (list 'one)))))


(test test-wire-slots-other
  "Test we can't wire slots to non-pin-slots."
  (let ((tc (make-instance 'test-subcomponents)))
    (signals (hw:non-pin-interface-slot)
      (hw:connect-slots tc (list 'four)
			tc (list 'other)))))


;; ---------- Fully wired ----------

(test test-fully-wired-pin
  "Test we can detect a fully-wired pin."
  (let* ((w (make-instance 'hw:wire))
	 (p (make-instance 'hw:pin :wire w)))
    (hw:ensure-fully-wired p)))


(test test-non-fully-wired-pin
  "Test we can detect an unwired pin."
  (let* ((p (make-instance 'hw:pin)))
    (signals (hw:not-fully-wired)
      (hw:ensure-fully-wired p))))


(test test-fully-wired-connector
  "Test we can detect a fully-wired connector."
  (let* ((b (make-instance 'hw:bus :width 4))
	 (conn (make-instance 'hw:connector :width 4)))
    (hw:connect-pins conn b)
    (hw:ensure-fully-wired conn)))


(test test-non-fully-wired-connector
  "Test we can detect an unwired connector."
  (let* ( (conn (make-instance 'hw:connector :width 4)))
    (signals (hw:not-fully-wired)
      (hw:ensure-fully-wired conn))))


(test test-fully-wired-component
  "Test we can detet a fully-wired component."
  (let* ((db (make-instance 'hw:bus :width 8))
	 (ab (make-instance 'hw:bus :width 16))
	 (en (make-instance 'hw:pin :wire (make-instance 'hw:wire)))
	 (tc (make-instance 'test-slotted
			    :enable (hw:wire en))))
    (hw:connect-pins (slot-value tc 'three) ab)
    (hw:connect-pins (slot-value tc 'one) db)
    (hw:connect-pins (slot-value tc 'two) db)

    (hw:ensure-fully-wired tc)))


(test test-non-fully-wired-component
  "Test we can detet an unwired component."
  (let* ((db (make-instance 'hw:bus :width 8))
	 (ab (make-instance 'hw:bus :width 16))
	 (tc (make-instance 'test-slotted)))
    (hw:connect-pins (slot-value tc 'three) ab)
    (hw:connect-pins (slot-value tc 'one) db)
    (hw:connect-pins (slot-value tc 'two) db)

    (signals (hw:not-fully-wired)
      (hw:ensure-fully-wired tc))))


(test test-fully-wired-subcomponent
  "Test we can detet a fully-wired component with sub-components."
  (let* ((db (make-instance 'hw:bus :width 8))
	 (ab (make-instance 'hw:bus :width 16))
	 (en (make-instance 'hw:pin :wire (make-instance 'hw:wire)))
	 (tc1 (make-instance 'test-slotted
			     :enable (hw:wire en)))
	 (tc2 (make-instance 'test-subcomponents
			     :enable (hw:wire en)
			     :c tc1)))
    (hw:connect-pins (slot-value tc1 'three) ab)
    (hw:connect-pins (slot-value tc1 'one) db)
    (hw:connect-pins (slot-value tc1 'two) db)
    (hw:connect-pins (slot-value tc2 'four) db)

    (hw:ensure-fully-wired tc2)))


(test test-non-fully-wired-subcomponent
  "Test we can detet an unwired sub-component."
  (let* ((db (make-instance 'hw:bus :width 8))
	 (ab (make-instance 'hw:bus :width 16))
	 (en (make-instance 'hw:pin :wire (make-instance 'hw:wire)))
	 (tc1 (make-instance 'test-slotted
			     :enable (hw:wire en)))
	 (tc2 (make-instance 'test-subcomponents
			     :c tc1)))
    (hw:connect-pins (slot-value tc1 'three) ab)
    (hw:connect-pins (slot-value tc1 'one) db)
    (hw:connect-pins (slot-value tc1 'two) db)
    (hw:connect-pins (slot-value tc2 'four) db)

    (signals (hw:not-fully-wired)
      (hw:ensure-fully-wired tc2))))


;; ---------- Self-wiring components ----------

(hw:defcomponent test-selfwired ()
  ((tc1
    :type test-slotted
    :initarg :one)
   (tc2
    :type test-slotted
    :initarg :two)
   (three
    :pins 16))
  ;; (:wiring ((tc1 one) (tc2 one))
  ;;	   ((tc1 two) (tc2 two))
  ;;	   (three (tc1 three) (tc2 three))))
  )
