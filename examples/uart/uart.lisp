;; A USB UART in close-to-Verilog RTLisp
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

;; This is a transliteration into RTLisp of the "Documented Verilog UART" from
;; https://github.com/cyrozap/osdvu/blob/master/uart.v
;;
;; Copyright (C) 2010 Timothy Goddard (tim@goddard.net.nz)
;; Distributed under the MIT licence.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(in-package :cl-vhdsl/examples/uart)


(defmodule uart ((clk             :width 1 :direction :in)
		 (rst             :width 1 :direction :in)
		 (rx              :width 1 :direction :in)
		 (tx              :width 1 :direction :out)
		 (transmit        :width 1 :direction :in)
		 (tx-byte         :width 8 :direction :in)
		 (received        :width 1 :direction :out)
		 (rx-byte         :width 8 :direction :out)
		 (is-receiving    :width 1 :direction :out)
		 (is-transmitting :width 1 :direction :out)
		 (recv-error      :width 1 :direction :out)
		 &key
		 (clk-divide 1302))   ;; (/ clock-rate (* baud-rate 4))

  ;; states for state machines
  (let ((rx-idle 0 :as :constant)
	(rx-check-start 1 :as :constant)
	(rx-read-bits 2 :as :constant)
	(rx-check-stop 3 :as :constant)
	(rx-delay-restart 4 :as :constant)
	(rx-error 5 :as :constant)
	(rx-received 6 :as :constant)

	(tx-idle 0 :as :constant)
	(tx-sending 1 :as :constant)
	(tx-delay-restart 2 :as :constant))

    ;; working registers
    (let ((rx-clk-divider 0 :width 11)
	  (tx-clk-divider 0 :width 11)

	  (rx-state rx-idle :width 3)
	  (rx-countdown 0 :width 6)
	  (rx-bits-remaining 0 :width 4)
	  (rx-data 0 :width 8)

	  (tx-state tx-idle :width 3)
	  (tx-countdown 0 :width 6)
	  (tx-bits-remaining 0 :width 4)
	  (tx-data 0 :width 8))

      ;; make status visible
      (setq received (= rx-state rx-received))
      (setq recv-error (= rx-state rx-error))
      (setq is-receiving (/= rx-state rx-idle))
      (setq is-transmitting (/= tx-state tx-idle))

      ;; make data visible
      (setq rx-byte rx-data)

      (@ (posedge clk)
	 ;; reset the machine when requested
	 (when rst
	   (setq rx-state rx-idle)
	   (setq tx-state tx-idle))

	 ;; run the countdown timers
	 (decf rx-clk-divider)
	 (when (0= rx-clk-divider)
	   (setq rx-clk-divider clk-divide)
	   (decf rx-countdown))
	 (decf tx-clk-divider)
	 (when (0= tx-clk-divider)
	   (setq tx-clk-divider clk-divide)
	   (decf tx-countdown))

	 ;; receiving state machine
	 (case rx-state
	   (rx-idle
	    (when (0= rx)
	      (setq rx-clk-divider clk-divide)
	      (setq rx-countdown 2)
	      (setq rx-state rx-check-start)))

	   (rx-check-start
	    (when (0= rx-countdown)
	      (if (0= rx)
		  (progn
		    (setq rx-countdown 4)
		    (setq rx-bits-remaining 8)
		    (setq rx-state rx-read-bits))

		  (setq rx-state rx-error))))

	   (rx-read-bits
	    (when (0= rx-countdown)
	      (setq rx-data (+ (<< rx-data 1) rx))
	      (setq rx-countdown 4)
	      (decf rx-bits-remaining)
	      (setq rx-state (if rx-bits-remaining
				 rx-read-bits
				 rx-check-stop))))

	   (rx-check-stop
	    (when (0= rx-countdown)
	      (setq rx-state (if rx
				 rx-received
				 rx-error))))

	   (rx-delay-restart
	    (setq rx-state (if rx-countdown
			       rx-delay-restart
			       rx-idle)))

	   (rx-error
	    (setq rx-countdown 8)
	    (setq rx-state rx-delay-restart))

	   (rx-received
	    (setq rx-state rx-idle)))

	 ;; transmitting state machine
	 (case tx-state
	   (tx-idle
	    (when transmit
	      (setq tx-data tx-byte)
	      (setq tx-clk-divider clk-divide)
	      (setq tx-countdown 4)
	      (setq tx 0)
	      (setq tx-bits-remaining 8)
	      (setq tx-state tx-sending)))

	   (tx-sending
	    (when (0= tx-countdown)
	      (if tx-bits-remaining
		  (progn
		    (decf tx-bits-remaining)
		    (setq tx (bit tx-data 0))
		    (setq tx-data (>> tx-data 1))
		    (setq tx-countdown 4)
		    (setq tx-state tx-sending))

		  (progn
		    (setq tx 1)
		    (setq tx-countdown 0)
		    (setq tx-state tx-idle)))))

	   (tx-delay-restart
	    (setq tx-state (if tx-countdown
			       tx-delay-restart
			       tx-idle))))))))
