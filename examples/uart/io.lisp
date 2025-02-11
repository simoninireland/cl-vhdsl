;; Read from a UART and show on LEDs
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

(in-package :cl-vhdsl/examples/uart)


(defmodule io ((clk  :width 1 :direction :in)
	       (rx :width 1 :direction :in)
	       (leds :width bits :direction :out)
	       &key (bits 4))

  (let ((char :width 8)
	(out :width bits)
	(received :width 1)
	(char :width 8)
	(uart (make-instance 'uart :clk clk
				   :rx rx
				   :rx-byte char
				   :received received)))

    (@ (posedge clk)
       (if received
	   (setq out (bits rx-byte (- 8 bits 1)))))

    (setf leds out)))
