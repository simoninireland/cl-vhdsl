;; Memory with cached pre-decoded instructions
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

(in-package :cl-vhdsl/emu)


(defclass cached-memory (memory)
  ()
  (:documentation "A memory with pre-decoded instruction cache.

Each location contains a byte of \"real\" content and an optional
function that holds the behaviour of the location based at that location.
This saves decoding at run-time."))


;; We re-define the access functions to access the car of the pair
;; at each location, and add `memory-instruction' as an accessor
;; for the cdr.
;;
;; As an alternative to making the instructions setf-able like this
;; we could specialise `memory-location' to set depending on the
;; type of its value argument (byte or function), and add a
;; different method for accessing the cached instruction.

(defmethod memory-initialise ((mem cached-memory))
  (setf (memory-locations mem)
	(make-array (list (memory-size mem))))
  (let ((locs (memory-locations mem)))
    (dolist (addr (iota (memory-size mem)))
      (let ((cell (cons 0 nil)))
	(setf (aref locs addr) cell)))))


(defmethod memory-location ((mem cached-memory) addr)
  (car (aref (memory-locations mem) addr)))


(defmethod (setf memory-location) (v (mem cached-memory) addr)
  (setf (car (aref (memory-locations mem) addr)) v))


(defgeneric memory-instruction (mem addr)
  (:documentation "Return the instruction cached at location ADDR in MEM."))


(defmethod memory-instruction ((mem cached-memory) addr)
  (cdr (aref (memory-locations mem) addr)))


(defmethod (setf memory-instruction) (v (mem cached-memory) addr)
  (setf (cdr (aref (memory-locations mem) addr)) v))
