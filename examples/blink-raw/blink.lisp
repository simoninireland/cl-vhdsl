;; A simple LED blinker in close-to-Verilog
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

(in-package :cl-vhdsl/examples/blink-raw)


(let ((p '(module blink
	   ((clk  :width 1 :direction :in)
	    (leds :width 5 :direction :out))

	   (let ((counter 0 :width (+ 5 22))
		 (out 0 :width 5))

	     (@ (posedge clk)
		(setf counter (+ counter 1))
		(setf out (>> counter 22)))

	     (setf leds out)))))

  ;;(typecheck p (empty-environment))

  (with-open-file (str #p"blink.v" :direction :output
				   :if-exists :supersede)
    (let ((*synthesis-stream* str))
      (synthesise p :toplevel))))
