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


;;(c2mop:ensure-finalized (find-class 'test-slotted))
;;(c2mop:ensure-finalized (find-class 'test-subcomponents))


;; ---------- Wiring connectors and buses ----------

(test test-wire-slots-same-component
  "Test we can wire two pin interface slots on the same class together."
  (let ((tc (make-instance 'test-slotted))
	(b (make-instance 'hw:bus :width 8)))
    (hw:connector-pins-connect (slot-value tc 'one) b)
    (hw:connector-pins-connect (slot-value tc 'two) b)

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
    (hw:connector-pins-connect (slot-value tc1 'one) b)
    (hw:connector-pins-connect (slot-value tc2 'two) b)

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
    (hw:connector-pins-connect (slot-value tc 'one) b)
    (signals (hw:incompatible-pin-widths)
      (hw:connector-pins-connect (slot-value tc 'three) b))))


;; ---------- Wiring slots ----------

(test test-wire-slots
  "Test we can wire two compatible slots."
  (let ((tc (make-instance 'test-slotted)))
    (hw:connector-slots-connect tc (list 'one)
				tc (list 'two))

    ;; component doesn't see itself
    (is (null (hw:components-seen-by tc)))))


(test test-wire-slots-sub
  "Test we can wire-up slots on sub-components."
  (let* ((tc1 (make-instance 'test-slotted ))
	 (tc2 (make-instance 'test-subcomponents :sub tc1)))
    (hw:connector-slots-connect tc1 (list 'one)
				tc2 (list 'c 'two))))


(test test-wire-slots-super-sub
  "Test we can wire-up slots between component and sub-somponent."
  (let* ((tc1 (make-instance 'test-slotted ))
	 (tc2 (make-instance 'test-subcomponents :sub tc1)))
    (hw:connector-slots-connect tc2 (list 'four)
				tc2 (list 'c 'two))))


(test test-wire-slots-widths
  "Test we can't wire slots with incompatible widths."
  (let ((tc (make-instance 'test-slotted)))
    (signals (hw:incompatible-pin-widths)
      (hw:connector-slots-connect tc (list 'one)
				  tc (list 'three)))))


(test test-wire-slots-widths-sub
  "Test we can't wire-up slots on sub-components with incompatible widths."
  (let* ((tc1 (make-instance 'test-slotted ))
	 (tc2 (make-instance 'test-subcomponents :sub tc1)))
    (signals (hw:incompatible-pin-widths)
      (hw:connector-slots-connect tc1 (list 'one)
				  tc2 (list 'c 'three)))))


(test test-wire-slots-missing
  "Test we can't wire missing slots."
  (let ((tc (make-instance 'test-subcomponents)))
    (signals (hw:non-pin-interface-slot)
      (hw:connector-slots-connect tc (list 'four)
				  tc (list 'one)))))


(test test-wire-slots-other
  "Test we can't wire slots to non-pin-slots."
  (let ((tc (make-instance 'test-subcomponents)))
    (signals (hw:non-pin-interface-slot)
      (hw:connector-slots-connect tc (list 'four)
				  tc (list 'other)))))


;; ---------- Integral wiring diagrams ----------

(defun wiring-diagram-p (arg)
  "Test is ARG is a wiring diagram.

Wiring diagrams are class arguments that start with
`:wiring'."
  (equal (car arg) :wiring))


(defun ensure-wiring-diagram-valid (wiring)
  "Test that "
  )

(defun generate-wiring-method (name wiring)
  "Generate a wiring method from the given WIRING diagram."
  (with-gensyms (c)
    (flet ((generate-connect (wires)
	     (destructuring-bind (from to)
		 wires
	       `(hw:connector-slots-connect ,c ',(ensure-list from)
					    ,c ',(ensure-list to)))))

      (let ((wirings (mapcar #'generate-connect (cdr wiring))))
	`(defmethod component-wire ((,c ,name))
	   ,@wirings)))))


(defmacro defcomponent (name superclasses slots &rest args)
  "Define a new component."

  ;; components default to sub-classes of `component'
  (when (null superclasses)
    (setq superclasses (list 'hw:component)))

  ;; if we have a wiring diagram, generate the wiring method
  (let ((wiring (find-if #'wiring-diagram-p args))
	wiring-method)
    (when wiring
      (setq args (remove-if #'wiring-diagram-p
			    args))
      (setq wiring-method (generate-wiring-method name wiring)))

    `(progn
       ;; class definition
       (defclass ,name (,@superclasses)
	 ,slots
	 (:metaclass hw:metacomponent)
	 ,@args)

       ;; wiring method definition (if any)
       ,wiring-method
       )))


(defgeneric component-wire (c)
  (:documentation "Internally wire C according to its wiring diagram."))


(defcomponent test-wired ()
  ((four
    :pins 8)
   (c1
    :initarg :c
    :type test-slotted
    :initarg :sub1)
   (c2
    :initarg :c
    :type test-slotted
    :initarg :sub2))
  (:wiring (four (c1 one))
	   ((c2 two) (c1 two))))

(let ((c (make-instance 'test-wired :sub1 (make-instance 'test-slotted)
				    :sub2 (make-instance 'test-slotted))))
  (component-wire c)
  (hw:fully-wired-p c)
  )
