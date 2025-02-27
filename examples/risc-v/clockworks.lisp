;; Clock gearbox
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

(in-package :cl-vhdsl/examples/risc-v)
(declaim (optimize debug))

(defmodule clockworks ((clk-in :width 1 :direction :in)
		       (reset-in :width 1 :direction :in)
		       (clk :width 1 :direction :out)
		       (reset :width 1 :direction :out)
		       &key (slow 0))

  ;; clock divider
  (let ((slow-clk 0 :width (1+ slow)))
    (@ (posedge clk-in)
       (incf slow-clk))
    (setf clk (bit slow-clk slow)))

  ;; reset (always active-high)
  (setq reset reset-in))
