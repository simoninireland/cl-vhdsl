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


;; ---------- Components and mixins ----------

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
     (store
      :width 16
      :accessor store)
     (sub
      :as :subcomponent
      :accessor sub))
    (:metaclass def:synthesisable-component))

  (let ((c (make-instance 'mop-create-simple)))

    ;; check the pin interface (exported pins)
    (is (set-equal (def::pin-interface (class-of c))
		   '(addr clk)))

    ;; check clock is defined and only a wire
    (is (= (def::slot-width c 'clk) 1))
    (is (def::slot-exported c 'clk))
    (is (eql (def::slot-role c 'clk) :clock))

    ;; check parameters
    (is (set-equal (def::parameters c) '(addr-width)))

    ;; check addr has the right attributes
    (is (def::slot-exported c 'addr))
    (is (= (def::slot-width c 'addr) 8))
    (is (null (def::slot-role c 'addr)))

    ;; check store has the right attributes
    (is (not (def::slot-exported c 'store)))
    (is (= (def::slot-width c 'store) 16))
    (is (null (def::slot-role c 'addr)))

    ;; check sub-components
    (is (set-equal (def::subcomponents c) '(sub)))

    ;; check parameter is synthesised correctly
    (is (set-equal (def::generate-module-params c)
		   '((addr-width :initial-value 0))
		   :test #'equal))

    ;; check only addr are clk are exported
    (is (set-equal (def::generate-module-args c)
		   '((addr :width 8)
		     (clk :width 1))
		   :test #'equal))))


(test test-mixins
  "Test mixins give rise to the expected pin interfaces."
  (defclass mop-mixins-clocked (def:component def:clocked)
    ((addr
      :width 8
      :exported t
      :accessor addr))
    (:metaclass def:synthesisable-component))

  (defclass mop-mixins-enabled (def:component def:enabled)
    ((addr
      :width 8
      :exported t
      :accessor addr))
    (:metaclass def:synthesisable-component))

  (defclass mop-mixins-resetable (def:component def:resetable)
    ((addr
      :width 8
      :exported t
      :accessor addr))
    (:metaclass def:synthesisable-component))

  ;; combine several mixins
  (defclass mop-mixins-clocked-enabled-resetable (def:component
						  def:clocked def:enabled def:resetable)
    ((addr
      :width 8
      :exported t
      :accessor addr))
    (:metaclass def:synthesisable-component))

  (let ((mmc (make-instance 'mop-mixins-clocked))
	(mme (make-instance 'mop-mixins-enabled))
	(mmr (make-instance 'mop-mixins-resetable))
	(mmcer (make-instance 'mop-mixins-clocked-enabled-resetable)))

    (is (set-equal (def:pin-interface mmc) '(def::clk addr)))
    (is (set-equal (def:pin-interface mme) '(def::en addr)))
    (is (set-equal (def:pin-interface mmr) '(def::rst addr)))

    ;; check the three mixins together
    (is (set-equal (def:pin-interface mmcer) '(def::clk def::en addr def::rst)))))


;; ---------- Wiring ----------

(test test-component-wiring-diagram
  "Test we can specify wiring disgrams."

  (defclass mop-component-wiring-diagram (def:component)
    ((addr
      :width 16
      :exported t
      :as :wire
      :direction :in
      :role :io)
     (internal
      :width 16)
     (external
      :width 16
      :as :wire
      :direction :out
      :exported t)
     (another
      :width 16))
    (:wiring (addr internal)
     (external internal another))
    (:metaclass def:synthesisable-component))

  (let ((c (make-instance 'mop-component-wiring-diagram)))

    ;; check diagram is saved
    (is (equal (def:wiring-diagram c)
	       '((addr internal)
		 (external internal another))))

    ;; check wires
    (is (set-equal (def::generate-wiring c)
		   '((setq addr internal)
		     (setq external internal)
		     (setq internal another))
		   :test #'equal))))


(test test-component-wiring-subcomponent
  "Test we can wire to a sub-component's exported pins."

  (defclass mop-component-sub (def:component)
    ((external
      :width 8
      :as :wire
      :exported t))
    (:metaclass def:synthesisable-component))

  (defclass mop-component-super (def:component)
    ((outside
      :width 8
      :as :wire
      :exported t)
     (sub
      :as :subcomponent
      :initarg :sub))
    (:wiring ((sub external) outside))
    (:metaclass def:synthesisable-component))

  (let* ((csub (make-instance 'mop-component-sub))
	 (c (make-instance 'mop-component-super :sub csub)))
    (def::generate-wiring c)

    )


  )
