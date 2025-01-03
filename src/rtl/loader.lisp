;; Loader macros and helpers
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


;; ---------- Module registry ----------

(defparameter *module-list* nil
  "The module registry.

Modules added here are queued for synthesis.")


(defun get-modules-for-synthesis ()
  "Return an alist consisting of module names and their declarations."
  *module-list*)


;; ---------- Module declaration ----------

;; should we typecheck here too?

(defmacro defmodule (modname decls &body body)
  "Declare a module MODNAME with given DECLS and BODY.

The module is added to the *MODULE-LIST* list for synthesis."
  (with-gensyms (module)
    (let* ((code `(module ,modname ,decls
			  ,@body)))
      `(let ((,module ',code))
	 (appendf *module-list* (list (list ',modname ,module)))))))
