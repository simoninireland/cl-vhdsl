;; A simple LED blinker as a component
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

(in-package :cl-vhdsl/examples/blink-component)


(defclass blink (component clocked)
  ((counter
    :documentation "Counter including blink and delay."
    :width (+ bits delay)
    :accessor counter)
   (out
    :documentation "Connector to the LEDs."
    :width bits
    :as :wire
    :accessor out)

   ;; exported slots
   (leds
    :documentation "Connector for the LEDs."
    :exported t
    :width bits
    :direction :out
    :writer leds
    :as :wire)

   ;; parameters
   (bits
    :documentation "Number of LEDs being blinked."
    :as :parameter
    :initarg :bits
    :initform 5
    :reader bits)
   (delay
    :documentation "Number of bits of delay between blinks."
    :as :parameter
    :initarg :delay
    :initform 22
    :reader delay))

  (:wiring (leds out))

  (:documentation "An LED blinking component."))


(defmethod on-clock ((c blink))
  `(progn
     (incf (counter c))
     (setf (out c) (>> (counter c) (delay c)))))
