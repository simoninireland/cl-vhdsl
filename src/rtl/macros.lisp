;; (Re-)implementations of some macros
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

;; We re-implement some macros to avoid possible issues with the standard
;; versions generating non-synthesisable Lisp. The names of these macros
;; are suffixed /rtl, and we alias them using ADD-MACRO in the loader.

(in-package :cl-vhdsl/rtl)
(declaim (optimize debug))


;; ---------- Single-armed conditionals ----------

(defmacro when/rtl (cond &body body)
  "Execute BODY when COND is true."
  `(if ,cond
       (progn
	 ,@body)))


(defmacro unless/rtl (cond &body body)
  "Execute BODY unless COND is true."
  `(if (not,cond)
       (progn
	 ,@body)))


;; ---------- Increment and decrement ----------

;; These work because places in RTLisp can't be side-effecting.

(defmacro incf/rtl (place &optional (value 1))
  "Increment PLACE by VALUE, which defaults to 1."
  `(setf ,place (+ ,place ,value)))


(defmacro decf/rtl (place &optional (value 1))
  "Decrement PLACE by VALUE, which defaults to 1."
  `(setf ,place (+ ,place ,value)))


;; ---------- Quick common tests ----------

(defmacro 0= (arg)
  "Test whether ARG is equal to zero."
  `(= ,arg 0))


(defmacro 0/= (arg)
  "Test whether ARG is not equal to zero."
  `(/= ,arg 0))


;; ---------- Quick common maths operations ----------

(defmacro 1+/rtl (arg)
  "Return ARG plus one."
  `(+ ,arg 1))


(defmacro 1-/rtl (arg)
  "Return ARG minus one."
  `(- ,arg 1))


(defmacro 2* (arg)
  "Return ARG time two."
  `(<< ,arg 1))
