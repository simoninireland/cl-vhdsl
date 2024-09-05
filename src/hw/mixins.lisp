;; Mixins for common component features
;;
;; Copyright (C) 2024 Simon Dobson
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

(in-package :cl-vhdsl/hw)

;; ---------- Enable- and disable-able components ----------

(defclass enabled ()
  ((enable
    :documentation "The component-enable pin."
    :initarg :enable
    :pins 1
    :role :control)
   (enabled
    :documentation "Flag recording the component's enablement."
    :initform nil
    :accessor enabled))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that can be enabled and disabled.

Enable-able components only respond to changes in their pin interface
when they are enabled. The states of their pins are left unchanged."))

(defmethod enabled-p ((c enabled))
  (let ((en (elt (pins (slot-value c 'enable)) 0)))
    (and (pin-wired-p en)
	 (not (floating-p en))
	 (equal (state en) 1))))


;; Guards to prevent callbacks on disabled components
(defmethod on-pin-changed :if ((c enabled))
  (let ((en (enabled-p c))
	(pre (enabled c)))

    ;; re-direct the callback if the enabled status has changed
    (cond ((and en (not pre))
	   ;; component has become enabled
	   (on-enable c))

	  ((and (not en) pre)
	   ;; component has become disabled
	   (on-disable c)))
    (setf (enabled c) en)

    ;; return whether we're now enabled
    en))


(defmethod on-pin-triggered :if ((c enabled) p v)
  (enabled-p c))


;; Callbacks for enablement and disablement

;; TODO: Should we tristate :io and :reading pins, or indeed all pins except
;; enable, when the component is disabled? Or leave that to sub-classes?

(defgeneric on-enable (c)
  (:documentation "Callback called when a component is enabled.")

  ;; default callback is empty
  (:method ((c enabled))))


(defgeneric on-disable (c)
  (:documentation "Callback called when a component is disabled.")

  ;; defalt callback is empty
  (:method ((c enabled))))


;; ---------- Clocked components ----------

(defclass clocked ()
  ((clock
    :documentation "The component's clock pin."
    :initarg :clock
    :pins 1
    :role :trigger
    :reader clock))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that has a clock line.

Clocked components do most of their active work when the clock
transitions, although they can change state at other times too."))


;; ---------- Read/write components ----------

(defclass readwrite ()
  ((write-enable
    :documentation "The component's write-enable pin."
    :initarg :write-enable
    :pins 1
    :role :control
    :reader write-enable))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that has a write-enable line..

This provides a common control line for determining whether a component
reads data from a bus (when write-enable is high) or makes data available
on the bus. 'Write' should be seen from the perspective of ourside the
component.

This only works for components with a single decision on read or write.
More complicated components might need several such control lines."))


(defmethod write-enabled-p ((c readwrite))
  (equal (state (elt (pins (slot-value c 'write-enable)) 0)) 1))


(defmethod read-enabled-p ((c readwrite))
  (not (write-enabled-p c)))
