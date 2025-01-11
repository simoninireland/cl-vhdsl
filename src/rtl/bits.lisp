;; Bitwise access to variables
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
(declaim (optimize debug))


;; ---------- Single-bit access ----------

(defmethod typecheck-sexp ((fun (eql 'bit)) args env)
  (destructuring-bind (var bit)
      args
    '(fixed-width-unsigned 1)))


(defmethod synthesise-sexp ((fun (eql 'bit)) args context)
  (destructuring-bind (var bit)
      args
    (synthesise var :indeclaration)
    (as-literal "[ ")
    (synthesise bit :inexpression)
    (as-literal "]")))


(defmethod lispify-sexp ((fun (eql 'bit)) args env)
  (destructuring-bind (var bit)
      args
    `(logand (ash ,(lispify var) (- ,bit))
	     1)))


;; ---------- Multi-bit access ----------

(defmethod typecheck-sexp ((fun (eql 'bits)) args env)
  (destructuring-bind (var start :key end width)
      args
    (let ((tyvar (typecheck var env))
	  (l (eval-in-static-environment `(+ 1 (- ,start ,end)) env)))
      (if (<= l (bitwidth tyvar env))
	  `(fixed-width-unsigned ,l)

	  (error 'not-synthesisable :fragment `(bits ,start ,end))))))


(defmethod synthesise-sexp ((fun (eql 'bits)) args (context (eql :inexpression)))
  (destructuring-bind (var start end)
      args
    (synthesise var :inassignment)
    (as-literal "[ ")
    (synthesise start :inexpression)
    (as-literal " : ")
    (synthesise end :inexpression)
    (as-literal " ]")))


(defmethod lispify-sexp ((fun (eql 'bits)) args env)
  (destructuring-bind (var start &key end width)
      args
    (let ((l (eval-in-static-environment `(+ 1 (- ,start ,end)) env)))
      `(logand (ash ,(lispify var) (- ,end)) (1- (ash 1 ,l))))))
