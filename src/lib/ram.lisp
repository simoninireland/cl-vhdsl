;; RAM components
;;
;; Copyright (C) 2023--2025 Simon Dobson
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

(in-package :verilisp/lib)


;; ---------- Single-ported RAM ----------

(defclass ram (component clocked rw)
  ((width
    :documentation "The width of words in the memory."
    :initform 8
    :initarg :width
    :as :parameter
    :reader width)
   (depth
    :documentation "The number of words in the memory."
    :initarg :depth
    :as :parameter
    :reader depth)

   ;; state
   (mem
    :documentation "The memory."
    :initform (make-array depth :initial-value 0 :element-width width)
    :accessor mem)

   ;; pin interface
   (addr
    :documentation "The address to access."
    :width (bit-width depth)
    :as :wire
    :reader addr)
   (data
    :documentation "The data to read or write."
    :width width
    :as :wire
    :accessor data))
  (:documentation "A simple single-ported RAM component")
  (:metaclass synthesisable-component))


(defmethod on-clock-rising ((c ram))
  '(if rw
    ;; we're writing, fetch data from data bus and store
    (setf (aref em addr) data)))


(defmethod on-clock-falling ((c ram))
  '(if (0= rw)
    ;; we're reading, put the current address onto the data bus
    (setf data (aref mem addr))))
