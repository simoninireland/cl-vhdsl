;; Mixins for synthesisable components
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

(in-package :cl-vhdsl/def)


(defclass mixin ()
  ()
  (:metaclass synthesisable-component)
  (:documentation "Base class for component mixins.

Mixins should be used as additional base classes for components. They
provide additional behaviour that is automatically synthesised."))


;; ---------- Synchronous (clocked) components ----------

(defclass clocked (mixin)
  ((clk
    :documentation "The clock wire of the component."
    :width 1
    :as :wire
    :initarg :clk
    :role :clock
    :reader clk))
  (:metaclass synthesisable-component)
  (:documentation "Mixin for creating clocked components.

Clocked components all have a CLK wire that is used to trigger
activity in the component."))


(defgeneric on-clock-rising (c)
  (:method-combination append)
  (:documentation "Behaviour of C on rising clock edge.

This behaviour is run whenever the CLK line of C
transitions to 1.

The behaviour should be given in RTLisp."))


(defgeneric on-clock-falling (c)
  (:method-combination append)
  (:documentation "Behaviour of C on falling clock edge.

This behaviour is run whenever the CLK line of C
transitions to 0

The behaviour should be given in RTLisp."))


;; ---------- Resetable components ----------

(defclass resetable (mixin)
  ((rst
    :documentation "The reset wire of the component."
    :width 1
    :as :wire
    :role :trigger
    :initarg :rst
    :reader rst))
  (:metaclass synthesisable-component)
  (:documentation "Mixin for creating resetable components.

Resetable components have reset behaviour that runs
when the RST line is asserted. "))


(defgeneric on-reset (c)
  (:method-combination append)
  (:documentation "Behaviour of C on reset.

This behaviour is run whenever the RST line of C
is asserted.

If C is also clocked, the reset is synchronous and happens
on a clock edge. If C is not clocked, the reset is asynchronous
and happens as soon as the RST line is asserted.

The behaviour should be given in RTLisp."))
