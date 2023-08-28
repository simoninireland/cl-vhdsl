;; bus.lisp: Wires and buses
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

(in-package :cl-vhdsl)

;; ---------- Buses and wires ----------

(defclass bus ()
  ((width :initarg :width
	  :reader bus-width)
   (wirenames :initform nil
	      :accessor bus-wire-names)
   (wires :initform 0
	  :accessor bus-wires)
   (connections :initform '()
		:accessor bus-connections))
  (:documentation "A collection of wires, each attached to one or more pins."))

(defmethod initialize-instance :after ((b bus) &key)
  "Create an empty alist of connections to the wires of B."
  (setf (bus-connections b)
	(mapcar (lambda (i) (list i))
		(iota (bus-width b) :start 0))))


;; Interface
(defgeneric bus-wire-name-index (b n)
  (:documentation "Return the index of named wire N on bus B.

N can be an index or a name. The former is checked for range. The
latter is looked-up in the names list."))

(defgeneric bus-connect (b n p)
  (:documentation "Connect a pin P to wire N of B."))

(defgeneric bus-pins-connected-to-wire (b n)
  (:documentation "Return the pins connected to wire N of B."))

(defgeneric bus-propagate (b oldv newv)
  (:documentation "Propagate changes to pins attached to B.

The value on the bus is changing from OLDV to NEWV. Any wires whose
values have changed cause a change to be propagated to all the pins
connected to that wire."))


;; Helper functions

;; We treat binary bitfields as lists by placing the low-order bit at the
;; start of the list. This matches the use of functions like nth, in that
;; (nth 0 l) will extract the 0'th bit from an exploded number. It is
;; however unnatural with respect to how we write numbers, where the
;; low-order bit comes at the end. It also doesn't match the representation
;; used in cl-bitfields, which matches the way numbers are written.

(defun explode-bitfield (v)
  "Explode the bits of V into a list.

The low-order bit appears at the start of the list."
  (labels ((expl-bit (v)
	     (if (equal v 0)
		 '()
		 (append (list (logand v 1))
			 (expl-bit (ash v -1))))))
    (let ((bs (expl-bit v)))
      (if (null bs)
	  '(0)
	  bs))))

(defun implode-bitfield (bs)
  "Implode the bits in BS into a value.

The low-order bit appears at the start of the list."
  (labels ((add-bits (bs)
	     (if (null bs)
		 0
		 (+ (car bs)
		    (ash  (add-bits (cdr bs)) 1)))))
    (add-bits bs)))

(defun implode-bitfield-indices (vs is)
  "Create a bitfield with the bits in VS at positions indexed in IS.

The low-order bit is number 0. Bits not indexed and given a value are
set to 0."
  (let ((f 0))
    (mapc (lambda (i v)
	    (if (equal v 1)
		(setf f (logior f (ash 1 i)))))
	  is vs)
    f))

(defun fix-bitfield-width (bf w)
  "Fix the width of bitfield BF to W bits by padding with zeros in the high-order bit positions.

The bitfield is returned unchanged  if it is already of at least width W."
  (let ((zs (- w (length bf))))
    (if (> zs 0)
	(append bf (make-list zs :initial-element 0))
	bf)))

(defun changed-bit-indices (o n)
  "Return a list of indices of bits changed between O and N."
  (flet ((make-equal-length (p q)
	   (let ((pq (max (length p) (length q))))
	     (list (fix-bitfield-width p pq)
		   (fix-bitfield-width q pq)))))
    (let* ((obs (explode-bitfield o))
	   (nbs (explode-bitfield n))
	   (onbs (make-equal-length obs nbs))
	   (is (iota (length (car onbs)) :start 0))
	   (changed (mapcar (lambda (ob nb i)
			      (if (not (equal ob nb))
				  (list i)
				  '()))
			    (car onbs) (cadr onbs) is)))
      (apply #'append changed))))


;; Implementaton
(defmethod bus-wire-name-index ((b bus) n)
  "Return the index associated with wire N on bus B.

If the name is a number it is returned unchanged. Otherwise, the name
is looked-up in the name mapping. An error is signalled if the named
wire does't exist."
  (cond ((numberp n)
	 (if (and (>= n 0)
		  (< n (bus-width b)))
	     n
	     (error "Bus has no wire with index ~S" n)))
	((symbolp n)
	 (let ((na (assoc n (bus-wire-names b))))
	   (if (null na)
	       (error "Bus has no wire named ~S" n)
	       (cadr na))))))

(defmethod bus-wire-value ((b bus) n)
  "Return the value of wire N on bus B."
  (let ((i (bus-wire-name-index b n)))
    (logand (ash (slot-value b 'wires) (- i)) 1)))

(defmethod set-bus-wire-value ((b bus) n v)
  "Set the value of wire N on bus B to V."
  (let ((i (bus-wire-name-index b n)))
    (setf (slot-value b 'wires)
	  (logior (slot-value b 'wires)
		  (ash 1 i)))))

(defmethod set-bus-wire-values ((b bus) ns vs)
  "Set the values of the wires named NS on B to VS."
  (let* ((v (logior (bus-wires b) (implode-bitfield-indices is vs))))
    (setf (bus-wires b) v)))

(defmethod bus-connect ((b bus) n p)
  (let ((w (assoc (bus-wire-name-index b n) (bus-connections b))))
    (if (null (cdr w))
	(setf (cdr w) (list p))
	(appendf (cdr w) (list p)))))

    (defmethod bus-pins-connected-to-wire ((b bus) n)
      (let ((w (assoc (bus-wire-name-index b n) (bus-connections b))))
    (cdr w)))

;; (defmethod bus-propagate ((b bus) oldv newv)


;;   )
