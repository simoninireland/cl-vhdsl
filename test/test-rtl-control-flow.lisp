;; Tests of control flow
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


;; ---------- PROGN ----------

(test test-synthesise-progn
  "Test we can synthesise PROGN forms."
  (is (rtl:synthesise '(progn
			(setf a 5)
			(setf b 34))
		      :inblock)))


;; ---------- @ ----------

(test test-typecheck-at
  "Test we can type-check triggered blocks."
  ;; single wire sensitivity
  (is (subtypep (rtl:typecheck '(let ((clk 0 :as :wire)
				      (a 0))
				 (rtl::@ ((rtl::posedge clk))
				  (setf a 1)))
			       emptyenv)
		'(unsigned-byte 1)))

  ;; multiple wires sensitivity
  (is (subtypep (rtl:typecheck '(let ((clk 0 :as :wire)
				      (rst 0 :as :wire)
				      (a 0))
				 (rtl::@ ((rtl::posedge clk) (rtl::negedge rst))
				  (setf a 1)))
			       emptyenv)
		'(unsigned-byte 1))))


(test test-typecheck-edges
  "Test we can type-check edge trigger expressions."
  (is (subtypep (rtl:typecheck '(let ((clk 0 :as :wire)
				      (a 0))
				 (rtl::@ ((rtl::posedge clk))
				  (setq a clk)))
			       emptyenv)
		'(unsigned-byte 1))))


(test test-typecheck-wire-singleton
  "Test we can type-check single-wire triggers."
  (is (subtypep (rtl:typecheck '(let ((clk 0 :as :wire)
				      (a 0))
				 (rtl::@ clk
				  (setq a clk)))
			       emptyenv)
		'(unsigned-byte 1))))


(test test-typecheck-wire-trigger
  "Test we can type-check an edge trigger as a singleton, not in a list."
  (is (subtypep (rtl:typecheck '(let ((clk 0 :as :wire)
				      (a 0))
				 (rtl::@ (rtl::posedge clk)
				  (setq a clk)))
			       emptyenv)
		'(unsigned-byte 1))))


(test test-synthesise-edges
  "Test synthesis of sensitivies using edges."
  (is (rtl:synthesise '(let ((clk 0 :as :wire)
			     (a 0))
			(rtl::@ ((rtl::posedge clk) a)
			 (setq a clk)))
		      :inblock)))


(test test-synthesise-wire-singleton
  "Test synthesis of sensitivies using edges."
  (is (rtl:synthesise '(let ((clk 0 :as :wire)
			     (a 0))
			(rtl::@ clk
			 (setq a clk)))
		      :inblock)))


(test test-synthesise-wire-trigger
  "Test synthesis of sensitivies using a single trigger."
  (is (rtl:synthesise '(let ((clk 0 :as :wire)
			     (a 0))
			(rtl::@ (rtl:posedge clk)
			 (setq a clk)))
		      :inblock)))
