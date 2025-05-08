;; References to variables
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

(in-package :cl-vhdsl/rtl)


(defmethod bitwidth ((val symbol) env)
  (let ((tys (safe-car-cdr (get-type val env))))
    (bitwidth-type (car tys) (cadr tys) env)))


(defmethod typecheck ((form symbol) env)
  (get-type form env))


(defmethod float-let-blocks ((form symbol))
  `(,form ()))


(defmethod simplify-progn ((form symbol))
  form)


(defmethod synthesise ((form symbol) env (context (eql :inexpression)))
  (as-literal (format nil "~(~a~)" (ensure-legal-identifier (symbol-name form)))))

(defmethod synthesise ((form symbol) env (context (eql :inassignment)))
  (synthesise form env :inexpression))

(defmethod synthesise ((form symbol) env (context (eql :indeclaration)))
  (synthesise form env :inexpression))


(defmethod lispify ((form symbol) env)
  form)
