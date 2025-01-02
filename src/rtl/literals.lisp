;; Literal constants
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

(in-package :cl-vhdsl/rtl)


;; ---------- Integers ----------

(defmethod typecheck ((form integer) env)
  (let ((w (bitwidth form env)))
    (if (< form 0)
	`(fixed-width-signed ,w)
	`(fixed-width-unsigned ,w))))


(defmethod float-let-blocks ((form integer))
  `(,form ()))


(defmethod simplify-progn ((form integer))
  form)


(defmethod synthesise ((form integer) as)
  (format *synthesis-stream* "~s" form))


(defmethod lispify ((form integer) env)
  form)
