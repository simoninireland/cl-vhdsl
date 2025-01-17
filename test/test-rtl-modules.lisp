;; Tests of modules
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

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl/rtl)
(declaim (optimize debug))


(test test-typecheck-module
  "Test we can typecheck a module definition."
  (is (equal (type-of (rtl:typecheck '(rtl:module test ((clk :width 1 :direction :in)
							:key (p 1))
				       (let ((a 1))
					 (setq a 0)))
				     emptyenv))
	     'rtl::module-interface)))


(test test-module-no-wires
  "Test modules always need a wire."
  (signals (rtl:not-synthesisable)
    (rtl:typecheck '(rtl:module test (:key (p 1))
		     (let ((a 0))
		       (setq a 1)))
		   emptyenv)))


(test test-synthesise-module
  "Test we can syntheise a module with a variety of features."
  (is (rtl:synthesise '(rtl::module test ((clk :width 1 :direction :in)
					  (a :width 8 :direction :in)
					  (b :width 4 :direction :in)
					  :key e (f 45))
			(let ((x 0 :width 8)
			      (y 10 :width 8)
			      (z 44 :as :constant))
			  (rtl::@ ((rtl::posedge clk))
				  (setf x (+ x b) :sync t))))
		      :toplevel)))
