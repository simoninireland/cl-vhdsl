;; Tests of component definitions
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
(in-suite cl-vhdsl/def)
(declaim (optimize debug))


(test test-mop-create-simple
  "Test we can create a simple component."
  (defclass mop-create-simple (def:component)
    ((clk
      :width 1
      :role :clock
      :exported t
      :reader clk)
     (addr-width
      :as :parameter
      :reader addr-width)
     (addr
      :width 8
      :exported t
      :accessor addr)
     (sub
      :type def:component
      :accessor sub))
    (:metaclass def:synthesisable-component))

  (let ((c (make-instance 'mop-create-simple)))

    ;; check the pin interface (exported pins)
    (is (set-equal (def::pin-interface (class-of c))
		   '(addr clk)))

    ;; check clock is defined and only a wire
    (is (= (def::slot-width (class-of c) 'clk) 1))
    (is (eql (def::slot-role (class-of c) 'clk) :clock))

    ;; check parameters
    (is (set-equal (def::parameters (class-of c)) '(addr-width)))

    ;; check addr has the right attributes
    (is (def::slot-exported (class-of c) 'addr))
    (is (= (def::slot-width (class-of c) 'addr) 8))
    (is (null (def::slot-role (class-of c) 'addr)))

    ;; check sub-components
    (is (set-equal (def::subcomponents (class-of c)) '(sub)))

    ;; check only addr are clk are exported
    (is (set-equal (def::synthesise-module-args (class-of c))
		   '((addr :width 8)
		     (clk :width 1))
		   :test #'equal))))
