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


(defun direct-pin-slot-def (c slot-name)
  "Return the definition og SLOT-NAME on C."
  (let ((slot-defs (class-slots (class-of c))))
    (find slot-name slot-defs :key #'slot-definition-name)))


(defun qualified-pin-slot-def (c q-slot-name slot-name)
  "Return the definition of SLOT-NAME on the component in Q-SLOT-NAME in C."
  (let ((c (slot-value c q-slot-name)))
    (if (typep c 'component)
	(direct-pin-slot-def (class-of c) slot-name)

	;; only makes sense to look for pin slots on components
	(error (make-instance 'non-component-type :type (class-of c))))))


(defun pin-slot-def (c slot-name)
  "Retrieve the definition of SLOT-NAME on C.

SLOT-NAME should be either a symbol or a list of two symbols."
  (if (consp slot-name)
      (qualified-pin-slot-def c (car slot-name) (cadr slot-name))
      (direct-pin-slot-def c slot-name)))


;; ---------- Slot widths ----------

(defun pins-width-compatible (pins1 pins2)
  "Check that PINS1 and PINS2 have compatible widths."
  (cond ((and (typep pins1 'pin)
	      (typep pins2 'pin))
	 (let ((w (make-instance 'wire)))
	   (setf (pin-wire pins1) w)
	   (setf (pin-wire pins2) w)))

	((and (vectorp pins1)
	      (vectorp pins2)
	      (equal (length pins1) (length pins2)))
	 (let* ((w (length pins1))
		(b (make-instance 'bus :width w))
		(ws (bus-wires b)))
	   (dolist (i (iota w))
	     (setf (pin-wire (elt pins1 i)) (elt ws i))
	     (setf (pin-wire (elt pins2 i)) (elt ws i)))))

	(t
	 (error (make-instance 'incompatible-pin-slot-widths)))))


;; ---------- Wiring interface ----------

(defun connector-pins-connect (conn bus)
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
