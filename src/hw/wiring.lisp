;; Wiring-up the pin interfaces of components
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

(in-package :cl-vhdsl/hw)


;; ---------- Slot access ----------

(defun ensure-subcomponent (c slot)
  "Return the sub-component in SLOT of C.

SLOT is checked to make sure it is a component."
  (let ((c1 (slot-value c slot)))
    (if (typep c1 'component)
	c1
	(error 'non-component-type :type (class-of c1)))))


(defun ensure-pin-slot (c slot)
  "Return the connector of SLOT on C.

SLOT is checked to ensure it's in the pin interface of C."
  (if (pin-interface-p (class-of c) slot)
      (slot-value c slot)
      (error 'non-pin-interface-slot :component c :slot slot)))


(defun component-slot-connector (c s)
  "Return the connector for S on C.

S should be a one- or two-element list."

  ;; extract the right slot, either drectly or on the sub-component
  (when (> (length s) 1)
    (setq c (ensure-subcomponent c (car s)))
    (setq s (cdr s)))

  ;; return the connector
  (ensure-pin-slot c (car s)))


;; ---------- Wiring state tests ----------

(defgeneric fully-wired-p (c)
  (:documentation "Test whether C is fully wired.

Calling this function checks that all the necessary wiring has been
carried out. Unconnected wires are typically (but not necessarily)
errors."))


(defmethod fully-wired-p ((p pin))
  (not (null (pin-wire p))))


(defmethod fully-wired-p ((conn connector))
  (let ((pins (connector-pins conn))
	(w (connector-width conn)))
    (dolist (i (iota w))
      (fully-wired-p (elt pins i)))))


(defmethod fully-wired-p ((c component))
  (let ((pin-slots (pin-interface (class-of c))))
    (dolist (slot pin-slots)
      (fully-wired-p (slot-value c slot)))))


(defun ensure-fully-wired (&rest args)
  "Ensure that all elements of ARGS are fully wired.

The elements of ARGS will typically be components, but can also
be pins, connectors, or anything accepted by `fully-wired-p'.
If the elements are not fully wired, a `not-fully-wired' error
is signalled containing all the elements that fail the test."
  (let (not-wired)
    (dolist (c args)
      (when (not (fully-wired-p c))
	(setq not-wired (append (list c) not-wired))))

    (when (not (null not-wired))
      (error 'not-fully-wired :elements not-wired))))


;; ---------- Wiring interface ----------

(defun connector-pins-connect (conn &optional bus)
  "Connect the pins of CONN to the wires of BUS."

  ;; widths have to match
  (if (not (equal (connector-width conn) (bus-width bus)))
      (error 'incompatible-pin-widths :connector conn
				      :bus bus))

  ;; wire-up the pins
  (let ((ps (connector-pins conn))
	(ws (bus-wires bus)))
    (dolist (i (iota (connector-width conn)))
      (setf (pin-wire (elt ps i)) (elt ws i)))))


(defun connector-slots-connect (c1 s1 c2 s2)
  "Connect S1 on C1 to S2 C1.

Both S1 and S2 should be lists of one or two symbols. A list of the
form `(slot)' identifies a slot named `slot' on the corresponding
component: `slot' must be in the pin interface. A list of the form
`(slot subslot)' identifies a slot `subslot' of a component held in
the slot `slot' of corresponding component: again, `subslot' must be a
slot in the pin interface of that component. The pin slots identified
must have compatible widths."
  (let* ((conn1 (component-slot-connector c1 s1))
	 (conn2 (component-slot-connector c2 s2))
	 (w1 (connector-width conn1))
	 (w2 (connector-width conn2)))
    (if (= w1 w2)
	(let ((b (make-instance 'bus :width w1)))
	  (connector-pins-connect conn1 b)
	  (connector-pins-connect conn2 b))

	(error 'incompatible-pin-widths))))
