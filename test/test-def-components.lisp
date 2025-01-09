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
      :role :control
      :reader clk)
     (addr
      :width 8
      :exported t
      :accessor addr))
    (:metaclass def:synthesisable-component))

  (let ((c (make-instance 'mop-create-simple)))
    (is (set-equal (def::pin-interface (class-of c))
		   '(addr)))
    (is (set-equal (def::variables (class-of c))
		   '(def::name clk)))

    (is (= (def::slot-width (class-of c) 'clk) 1))
    (is (eql (def::slot-role (class-of c) 'clk) :control))

    (is (def::slot-exported (class-of c) 'addr))
    (is (= (def::slot-width (class-of c) 'addr) 8))
    (is (null (def::slot-role (class-of c) 'addr)))

    (is (equal (def::synthesise-module-args (class-of c))
	       '((addr :width 8))))))
