;; Data types
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

(in-package :cl-vhdsl/def)

;; ---------- Type constructors ----------

(defun unsigned-bitfield (width)
  "The type specifier of unsigned bitfields of WIDTH bits."
  (let ((maxval (1- (floor (expt 2 width)))))
    `(integer 0 ,maxval)))


(defun signed-bitfield (width)
  "The type specifier of signed bitfields of WIDTH bits."
  (let* ((half (floor (expt 2 (1- width))))
	 (minval (- half))
	 (maxval (1- half)))
    `(integer ,minval ,maxval)))


;; ---------- Common types ----------

(deftype unsigned-8 () (unsigned-bitfield 8))
(deftype signed-8 () (signed-bitfield 8))
(deftype unsigned-16 () (unsigned-bitfield 16))
(deftype signed-16 () (signed-bitfield 16))
