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


(defparameter *module-interfaces* '()
  "Mapping of known modules to their interface types.

This variable contains all the modules that can be imported.")


(defun clear-module-registry ()
  "Clear the module registry of all imported and declared modules."
  (setq *module-list* '()
	*module-interfaces* '()))


(defun known-module-interface-p (modname)
  "Test whether MODNAME is known as a module interface."
  (not (null (assoc modname *module-interfaces*))))


(defun add-module-interface (modname intf)
  "Add module MODNAME with given INTF."
  (when (known-module-interface-p modname)
      (error 'duplicate-module :module modname))

  (appendf *module-interfaces* (list (list modname intf))))


(defun get-module-interface (modname)
  "Return the module interface type for MODNAME."
  (if-let ((m (assoc modname *module-interfaces*)))
    (cadr m)

    (error 'unknown-module :module modname)))


(defun synthesising-module-p (modname)
  "Test whether MODNAME is queued for synthesis."
  (not (null (assoc modname *module-list*))))


(defun add-module-for-synthesis (modname module)
  "Queue the module MODNAME with code MODULE for synthesis."
  (when (synthesising-module-p modname)
      (error 'duplicate-module :module modname))

  (appendf *module-list* (list (list modname module))))


(defun get-modules-for-synthesis ()
  "Return an alist consisting of module names and their declarations."
  *module-list*)


;; ---------- Module declaration ----------

(defmacro defmodule (modname decls &body body)
  "Declare a module MODNAME with given DECLS and BODY.

The module is loaded, macro-expanded, and type-checked, meaning that
it will be at least minimally syntactically correct after definition.

The module is added to the *MODULE-LIST* list for synthesis. Its
type is added to *MODULE-INTERFACES* for importing. Duplicate
module names will cause a DUPLICATE-MODULE error."
  (with-gensyms (module expanded)
    (let* ((code `(module ,modname ,decls
			  ,@body)))
      `(let* ((,module ',code)
	      (,expanded (expand-macros ,module)))
	 ;; typecheck the expanded module
	 (let ((intf (typecheck ,expanded (empty-environment))))

	   ;; add type to interfaces available for import
	   (add-module-interface ',modname intf)

	   ;; add code to modules for synthesis
	   (add-module-for-synthesis ',modname ,module )

	   t)))))


;; ---------- Module synthesis ----------

(defun synthesise-module (m str)
  "Synthesise module M to STR."
  (with-synthesis-to-stream str
      (funcall (compose (rcurry #'synthesise :toplevel)
			(compose #'car #'float-let-blocks))
	       m)))
