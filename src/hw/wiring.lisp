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

S should be a one- or two-element list identifying either a
pin-slot of C or a pin-slot of a sub-component of C held
in one of its slots."

  ;; extract the right slot, either drectly or on the sub-component
  (when (> (length s) 1)
    (setq c (ensure-subcomponent c (car s)))
    (setq s (cdr s)))

  ;; return the connector
  (ensure-pin-slot c (car s)))


;; ---------- Wiring state tests ----------

(defun ensure-fully-wired (&rest args)
  "Ensure that all elements of ARGS are fully wired.

The elements of ARGS will typically be components, but can also
be pins, connectors, or anything accepted by `fully-wired-p'.

If the elements are not fully wired, a `not-fully-wired' error
is signalled containing all the pins that fail the test."
  (let (not-wired)
    (dolist (c args)
      (dolist (p (coerce (pins c) 'list))
	(when (not (fully-wired-p p))
	  (setq not-wired (append (list p) not-wired)))))

    (when (not (null not-wired))
      (error 'not-fully-wired :elements not-wired))))


;; ---------- Wiring interface ----------

(defun connect-pins (conn b)

  ;; widths have to match
  (if (not (equal (width conn) (width b)))
      (error 'incompatible-pin-widths :connector conn
				      :bus b))

  ;; wire-up the pins
  (let ((ps (pins conn))
	(ws (wires b)))
    (dolist (i (iota (width conn)))
      (setf (wire (elt ps i)) (elt ws i)))))


(defun connect-slots (c1 s1 c2 s2)
  (let* ((conn1 (component-slot-connector c1 s1))
	 (conn2 (component-slot-connector c2 s2))
	 (w1 (width conn1))
	 (w2 (width conn2)))
    (if (= w1 w2)
	(let ((b (make-instance 'bus :width w1)))
	  (connect-pins conn1 b)
	  (connect-pins conn2 b))

	(error 'incompatible-pin-widths))))
