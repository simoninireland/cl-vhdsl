;; Adder component
;;
;; Copyright (C) 2024--2025 Simon Dobson
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

(in-package :cl-vhdsl/examples/sap-1-component)


(defclass adder (component)
  ((a
    :documentation "The adder's A bus (left operand)."
    :exported t
    :width 8
    :as :wire
    :reader a)
   (b
    :documentation "The adder's B bus (right operand)."
    :exported t
    :width 8
    :as :wire
    :reader b)
   (out
    :documentation "The adder's C bus (output)."
    :exported t
    :width 8
    :as :wire
    :writer out)
   (sub
    :documentation "Subtract control."
    :exported t
    :width 1
    :as :wire
    :reader sub))
  (:documentation "The adder."))


(defmethod behaviour ((add adder))
  `(setf (out add) (if (sub add)
		     (- (a add) (b add))
		     (+ (a add) (b add)))))
