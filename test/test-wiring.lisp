;; Tests of wiring components
;;
;; Copyright (C) 2023 Simon Dobson
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
(in-suite cl-vhdsl)

;; ---------- Accessing qualified slots ----------

(defclass test-slotted (hw:component)
  ((one
    :pins 16)
   (two
    :pins 8
    :role :control))
  (:metaclass hw:metacomponent))


(defclass test-subcomponents (hw:component)
  ((three
    :pins 8)
   (c
    :initarg :c
    :type test-slotted))
  (:metaclass hw:metacomponent))


(c2mop:ensure-finalized (find-class 'test-slotted))
(c2mop:ensure-finalized (find-class 'test-subcomponents))


;; ---------- Wiring ----------
