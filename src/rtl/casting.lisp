;; Type casting
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


(defmethod typecheck-sexp ((fun (eql 'the)) args env)
  (destructuring-bind (ty val)
      args
    (let ((tyval (typecheck val env)))
      (ensure-subtype tyval ty)
      ty)))


(defmethod synthesise-sexp ((fun (eql 'the)) args context)
  (destructuring-bind (ty val)
      args
    (synthesise val context)))
