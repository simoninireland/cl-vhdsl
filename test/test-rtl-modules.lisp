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


;; ---------- Module definition ----------

(test test-typecheck-module
  "Test we can typecheck a module definition."
  (is (equal (type-of (rtl:typecheck '(rtl:module test ((clk :type (unsigned-byte 1) :direction :in)
							&key (p 1))
				       (let ((a 1))
					 (setq a 0)))
				     emptyenv))
	     'rtl::module-interface)))


(test test-module-no-wires
  "Test modules always need a wire."
  (signals (rtl:not-synthesisable)
    (rtl:typecheck '(rtl:module test (&key (p 1))
		     (let ((a 0))
		       (setq a 1)))
		   emptyenv)))


(test test-synthesise-moduletest
  "Test we can syntheise a module with a variety of features."
  (is (rtl:synthesise '(rtl::module test ((clk :type (unsigned-byte 1) :direction :in)
					  (a   :type (unsigned-byte 8) :direction :in)
					  (b   :type (unsigned-byte 4) :direction :in)
					  &key e (f 45))
			(let ((x 0  :type (unsigned-byte 8))
			      (y 10 :type (unsigned-byte 8))
			      (z 44 :as :constant))
			  (rtl::@ (rtl::posedge clk)
				  (setf x (+ x b) :sync t))))
		      :toplevel)))


(test test-synthesise-module-late-init
  "Test we can synthesise modules with late initialisation."
  (rtl::clear-module-late-initialisation)

  (is (rtl:synthesise '(rtl::module test ((clk :type (unsigned-byte 1) :direction :in)
					  (a   :type (unsigned-byte 8) :direction :in)
					  (b   :type (unsigned-byte 4) :direction :in)
					  &key e (f 45))
			(let ((x 0 :type (unsigned-byte 8))
			      (a (make-array '(8) :initial-contents (:file "test.hex"))))
			  (rtl::@ (rtl::posedge clk)
				  (setf x (aref a 4)))))
		      :toplevel))

  ;; make sure synthesis cleared the late intiialisation queue
  (is (not (rtl::module-late-initialisation-p))))


;; ---------- Module instanciation ----------

(test test-module-instanciate
  "Test we can instanciate a module."

  (rtl:clear-module-registry)

  (rtl:defmodule clock ((clk-in  :direction :in  :as :wire :type (unsigned-byte 1))
			(clk-out :direction :out :as :wire :type (unsigned-byte 1)))
    (setq clk-out clk-in))

  ;; ceck that the import types correctly
  (is (subtypep (rtl:typecheck '(let ((clk 0    :type (unsigned-byte 1) :as :wire)
				      (clk-in 0 :type (unsigned-byte 1) :as :wire))
				 (let ((clock (make-instance 'clock :clk-in clk-in
								    :clk-out clk)))
				   clock))
			       emptyenv)
		'rtl::module-interface))

  ;; check we need to wire all arguments
  (signals (rtl:not-importable)
    (rtl:typecheck '(let ((clk 0 :type (unsigned-byte 1) :as :wire))
		     (let ((clock (make-instance 'clock :clk-out clk)))
		       clock))
		   emptyenv))

  ;; check wires can be deliberately left unconnected
  ;; TBD

  ;; to check module instanciation we have to perform several
  ;; passes to get the variables into the body of the module
  (is (rtl:synthesise (rtl:simplify-progn
		       (car (rtl:float-let-blocks
			     '(rtl:module module-instanciate
			       ((clk-in :type (unsigned-byte 1) :direction :in :as :wire))
			       (let ((clk 0 :type (unsigned-byte 1) :as :wire))
				 (let ((clock (make-instance 'clock :clk-in clk-in
								    :clk-out clk)))
				   (setq clk 1)))))))
		      :toplevel)))


(test test-module-instanciate-with-bitfields
  "Test we can instanciate a module that uses bitfields in its wiring."

  (rtl:clear-module-registry)

  (rtl:defmodule clock ((clk_in  :direction :in  :as :wire :type (unsigned-byte 1))
			(clk_out :direction :out :as :wire :type (unsigned-byte 1)))
    (setq clk_out clk_in))

  (is (subtypep (type-of (rtl:typecheck (rtl::expand-macros
					 '(rtl:module moduleinstanciatebitfields
					   ((clk_in :type (unsigned-byte 1) :direction :in :as :wire))
					   (let ((ctrl 0 :type (unsigned-byte 4) :as :wire))
					     (rtl:with-bitfields (clk b2 b1 b0)
						 ctrl
					       (let ((clock (make-instance 'clock :clk_in clk_in
										  :clk_out clk)))
						 clock)))))
					emptyenv))
		'rtl::module-interface))

  (is (rtl:synthesise (rtl:simplify-progn (car (rtl:float-let-blocks
						(rtl:expand-macros
						 '(rtl:module moduleinstanciatebitfields
						   ((clk_in :type (unsigned-byte 1) :direction :in :as :wire))
						   (let ((ctrl 0 :type (unsigned-byte 4) :as :wire))
						     (rtl:with-bitfields (clk b2 b1 b0)
							 ctrl
						       (let ((clock (make-instance 'clock :clk_in clk_in
											  :clk_out clk)))
							 (setf ctrl 1)))))))))
		      :toplevel)))


(test test-synthesise-module-instanciation
  "Test we can synthesise a module instanciation."
  (rtl:clear-module-registry)

  (rtl:defmodule clock ((clk_in  :direction :in  :as :wire :type (unsigned-byte 1))
			(clk_out :direction :out :as :wire :type (unsigned-byte 1))
			&key (p 1) (q 2))
    (setq clk_out clk_in))

  (is (rtl:synthesise '(let ((c 0 :as :wire :type (unsigned-byte 1))
			     (d 0 :as :wire :type (unsigned-byte 1)))
			(let ((a (make-instance 'clock :clk_in c :clk_out d)))))
		      :inmodule))
  (is (rtl:synthesise '(let ((c 0 :as :wire :type (unsigned-byte 1))
			     (d 0 :as :wire :type (unsigned-byte 1)))
			(let ((a (make-instance 'clock :clk_in c :clk_out d :p 23)))))
		      :inmodule)))
