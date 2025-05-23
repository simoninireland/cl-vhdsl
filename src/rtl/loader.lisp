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


;; ---------- Macros we allow in RTLisp ----------

(defparameter *macros* nil
  "List of macros expanded within RTLisp forms.")


(defun add-macro (name &optional real-name)
  "Add NAME as a macro in RTLisp.

The macros will be expanded during the expansion pass (if enabled).
If REAL-NAME is given, then NAME acts as a pseudonym for it. Otherwise
NAME is assuemd to be the \"real\" name of the macro."
  (appendf *macros* (list (list name (or real-name nil)))))


;; conditionals
(add-macro 'cond)
(add-macro 'when 'when/rtl)
(add-macro 'unless 'unless/rtl)

;; representation-specific lets
(add-macro 'let-wires)
(add-macro 'let-registers)
(add-macro 'let-constants)

;; places
(add-macro 'incf 'incf/rtl)
(add-macro 'decf 'decf/rtl)

;; tests and maths
(add-macro '0=)
(add-macro '0/=)
(add-macro '1+ '1+/rtl)
(add-macro '1- '1-/rtl)
(add-macro '2* '2*)

;; variable introduction and aliasing
(add-macro 'with-bitfields)

;; state machines
(add-macro 'state-machine)
(add-macro 'next 'next/rtl)
(add-macro 'exit 'exit/rtl)


;; ---------- Module registry ----------

(defparameter *module-list* nil
  "The module registry.

Modules added here are queued for synthesis.")


(defparameter *module-interfaces* '()
  "Mapping of known modules to their interface types.

This variable contains all the modules that can be imported. It will
contain the types of all the modules in *MODULE-LIST* that have been
defined in this session, plus any externlly-defined modules made
available for import.")


(defun clear-module-registry ()
  "Clear the module registry of all imported and declared modules."
  (setq *module-list* '()
	*module-interfaces* '()))


(defun known-module-interface-p (modname)
  "Test whether MODNAME is known as a module interface."
  (not (null (assoc modname *module-interfaces*))))


(defun add-module-interface (modname intf)
  "Add module MODNAME with given INTF."
  (ensure-legal-identifier modname)

  (when (known-module-interface-p modname)
    (error 'duplicate-module :module modname))

  (appendf *module-interfaces* (list (list modname intf))))


(defun get-module-interface (modname)
  "Return the module interface type for MODNAME."
  (if-let ((m (assoc modname *module-interfaces*)))
    (cadr m)

    (error 'unknown-module :module modname
			   :hint "Make sure the module has been defined or imported")))


(defun synthesising-module-p (modname)
  "Test whether MODNAME is queued for synthesis."
  (not (null (assoc modname *module-list*))))


(defun add-module-for-synthesis (modname module)
  "Queue the module MODNAME with code MODULE for synthesis."
  (ensure-legal-identifier modname)

  (when (synthesising-module-p modname)
    (error 'duplicate-module :module modname))

  (appendf *module-list* (list (list modname module))))


(defun get-module (modname)
  "Return the module code for MODNAME.

This will typically have been set by DEFMODULE and so will have been
type-checked, macro-expanded, and possibly had other passes applied."
  (if-let ((m (assoc modname *module-list*)))
    (cadr m)

    (error 'unknown-module :module modname
			   :hint "Make sure the module has been declared")))


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
module names will cause a DUPLICATE-MODULE error.

Return the name of the newly-defined module."
  (with-gensyms (module expanded)
    (let* ((code `(module ,modname ,decls
			  ,@body)))
      `(let* ((,module ',code)
	      (,expanded (expand-macros ,module)))
	 ;; typecheck the expanded module
	 (let ((intf (typecheck ,expanded)))

	   ;; add type to interfaces available for import
	   (add-module-interface ',modname intf)

	   ;; add expanded code to modules for synthesis
	   (add-module-for-synthesis ',modname ,expanded)

	   ',modname)))))


;; ---------- Module synthesis ----------

(defun synthesise-module (m str)
  "Synthesise module M to STR."
  (with-synthesis-to-stream str
      (funcall (compose #'synthesise
			#'simplify-progn
			(compose #'car #'float-let-blocks))
	       m)))
