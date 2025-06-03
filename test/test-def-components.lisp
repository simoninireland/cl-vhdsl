;; Tests of component definitions
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :verilisp/test)
(in-suite verilisp/def)


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
		   '((addr-width 0))
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


(test test-component-subcomponents
  "Test we can identify sub-component slots correctly."

  (defclass mop-component-subcomponents (def:component)
    ((outside
      :width 8
      :as :wire
      :exported t)
     (not-a-subcomponent
      :type integer)
     (sub
      :as :subcomponent	;; by explicit marker
      :initarg :sub)
     (othersub
      :type def:component     ;; by type
      :initarg :othersub))
    (:metaclass def:synthesisable-component))

  (let ((c (make-instance 'mop-component-subcomponents)))
    (is (set-equal (def:subcomponents (find-class 'mop-component-subcomponents))
		   '(sub othersub)))
    (is (set-equal (def:subcomponents c)
		   '(sub othersub)))))


;; ---------- Wiring ----------

(test test-component-top-wire
  "Test we can extract top-level wires'"

  ;; if there are no connectors, return two nils
  (destructuring-bind (wire connectors)
      (def::find-or-create-module-connector '())
    (is (and (null wire)
	     (null connectors))))

  ;; create a new wire when there's none appropriate
  (let ((w '((sub a) (sub b) (super a))))
    (is (equal (cadr (def::find-or-create-module-connector w))
	       w)))

  ;; use a wire when there is one
  (let ((w '((sub a) b (super a))))
    (destructuring-bind (wire connectors)
	(def::find-or-create-module-connector w)
      (is (and (equal wire 'b)
	       (equal connectors '((sub a) (super a)))))))

  ;; if there are two possible wires, use the first
  (let ((w '((sub a) b c (super a))))
    (destructuring-bind (wire connectors)
	(def::find-or-create-module-connector w)
      (is (and (equal wire 'b)
	       (equal connectors '((sub a) c (super a))))))))


(test test-component-instanciate-subcomponents
  "Test we can instanciate sub-components when needed."

  (defclass mop-component-instanciate-sub (def:component)
    ((external
      :width 8
      :as :wire
      :exported t))
    (:metaclass def:synthesisable-component))

  (defclass mop-component-instanciate (def:component)
    ((outside
      :width 8
      :as :wire
      :exported t)
     (sub
      :type mop-component-instanciate-sub)
     (othersub
      :as :subcomponent)
     (wrongsub
      :type integer))
    (:metaclass def:synthesisable-component))

  (let ((c (make-instance 'mop-component-instanciate)))
    (def::instanciate-subcomponent c 'sub)

    ;; make sure the value was cached
    (is (subtypep (type-of (slot-value c 'sub))
		  'mop-component-instanciate-sub))

    ;; make sure it doesn't get re-created
    (let ((v (slot-value c 'sub)))
      (is (equal (def::instanciate-subcomponent c 'sub)
		 v)))

    ;; can't instanciate without a type
    (signals (def::subcomponent-mismatch)
      (def::instanciate-subcomponent c 'othersub))

    ;; can't instanciate a sub-component with a non-component type
    (signals (def::subcomponent-mismatch)
      (def::instanciate-subcomponent c 'wrongsub))))


(test test-component-generate-wires
  "Test we can generate wires to sub-components according to a wiring diagram."

  (defclass mop-component-wires-sub (def:component)
    ((external
      :width 8
      :as :wire
      :exported t))
    (:metaclass def:synthesisable-component))

  (defclass mop-component-wires-one (def:component)
    ((outside
      :width 8
      :as :wire
      :exported t)
     (sub
      :type mop-component-instanciate-sub))
    (:wiring ((sub external) outside))
    (:metaclass def:synthesisable-component))

  (defclass mop-component-wires-two (def:component)
    ((outside
      :width 8
      :as :wire
      :exported t)
     (sub
      :type mop-component-instanciate-sub)
     (othersub
      :type mop-component-instanciate-sub))
    (:wiring ((sub external) (othersub external)))
    (:metaclass def:synthesisable-component))

  (let ((c (make-instance 'mop-component-wires-one)))
    (def::instanciate-subcomponents c)

    (let ((w (car (def::wiring-diagram c))))
      ;; no new wire created by wiring
      (is (null (def::connect-subcomponents-on-wire c w)))

      ;; ensure we wired the slot
      (is (eql (slot-value (slot-value c 'sub) 'external)
	       'outside))))

  (let ((c (make-instance 'mop-component-wires-two)))
    (def::instanciate-subcomponents c)

    (let ((w (car (def::wiring-diagram c))))
      (let ((nw (def::connect-subcomponents-on-wire c w)))
	;; new wire needed
	(is (not (null nw)))

	;; ensure we wired both slots to the same new wire
	 (is (eql (slot-value (slot-value c 'sub) 'external)
		  nw))
	 (is (eql (slot-value (slot-value c 'othersub) 'external)
		  nw))))))


(test test-component-create-wire-subcomponent
  "Test that we can create the declarations for sub-com,ponents with the correct wiring."

  (defclass mop-component-create-wires-sub (def:component)
    ((external
      :width 8
      :as :wire
      :initarg :external
      :exported t))
    (:metaclass def:synthesisable-component))

  (defclass mop-component-create-wires-one (def:component)
    ((outside
      :width 8
      :as :wire
      :initarg :outside
      :exported t)
     (sub
      :type mop-component-create-wires-sub))
    (:wiring ((sub external) outside))
    (:metaclass def:synthesisable-component))

  (let ((c (make-instance 'mop-component-create-wires-one)))
    (def::instanciate-subcomponents c)

    (let ((w (car (def::wiring-diagram c))))
      (def::connect-subcomponents-on-wire c w)

      (let ((decl (def::generate-subcomponent-decl c 'sub)))
	(is (= (length decl) 2))
	(is (eql (car decl) 'sub))))))
