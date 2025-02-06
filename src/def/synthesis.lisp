;; Synthesising synthesisable components to RTLisp
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

(in-package :cl-vhdsl/def)
(declaim (optimize debug))


;; ---------- Module arguments and parameters ----------

(defun generate-module-args (c)
  "Return the RTLisp code to declare the module arguments.

The module arguments are constructed from the pin interface of C.

The code is a list of declarations suitable for a LET form."
  (flet ((pin-interface-to-decl (slot)
	   (let* ((slot-def (find-slot-def c slot))
		  (name (slot-definition-name slot-def))
		  (v (if (slot-boundp c slot)
			 (slot-value c slot)
			 0))
		  (width (slot-width c slot))
		  (rep (slot-representation c slot))
		  (direction (slot-direction c slot)))
	     `(,name ,@(if width
			   `(:width ,width))
		     ,@(if direction
			   `(:direction ,direction))
		     ,@(if (/= v 0)
			   `(:initial-value ,v))
		     ,@(if rep
			   `(:as ,rep))))))

    (mapcar #'pin-interface-to-decl (pin-interface c))))


(defun generate-module-params (c)
  "Return the RTLisp code to declare the module parameters.

The code is a list of declarations suitable for a LET form."
  (flet ((param-to-decl (slot)
	   (let* ((slot-def (find-slot-def c slot))
		  (name (slot-definition-name slot-def))
		  (v (if (slot-boundp c slot)
			 (slot-value c slot)
			 0)))
	     `(,name ,v))))

    (mapcar #'param-to-decl (parameters c))))


;; ---------- Sub-components ----------

(defun instanciate-subcomponent (c slot)
  "Instanciate the sub-component of C in SLOT.

If a new sub-component instance is created, it is saved in SLOT."
  (or (slot-subcomponent-instance c slot)

      ;; no instance, try to create one
      (let ((ty (slot-subcomponent-type c slot)))
	(if (eql ty t)
	    ;; no type either, fail
	    (error 'subcomponent-mismatch :component c :slot slot
					  :hint "Provide an instance or type for the slot.")

	    (if (subtypep ty 'component)
		;; instanciate and save
		(let ((subcomponent (make-instance ty)))
		  (setf (slot-value c slot) subcomponent))

		;; not a valid component type
		(error 'subcomponent-mismatch :component c :slot slot
					      :hint "Sub-component must be a sub-type of component."))))))


(defun instanciate-subcomponents (c)
  "Instanciate all the sub-components of C.

For each sub-component slot, if there is no instance existing, use
the slot type to create one. If successful, this will result in
sub-component object instances in all sub-component slots, with the
corret types."
  (dolist (slot (subcomponents c))
    (instanciate-subcomponent c slot)))


(defun generate-subcomponent-decl (c slot)
  "Return the declaration of the sub-component SLOT on C."
  (declare (optimize debug))
  (let* ((subcomponent (slot-value c slot))
	 (ty (type-of subcomponent))
	 (keys (foldr (lambda (ks k)
			(let ((kw (make-keyword k)))
			  (append ks `(,kw ,(slot-value subcomponent k)))))
		      (append (subcomponents subcomponent)
			      (pin-interface subcomponent))
		      '())))

    `(,slot (make-instance ',ty ,@keys))))


;; ---------- Wiring ----------

;; Wiring is a fundamentally different concept to that in Verilog. We need
;; to construct a mapping from components to their initial arguments and values,
;; which we then use when synthesising the RTLisp module instanciation.

(defun new-wire-name ()
  "Return a name for a new wire."
  (gensym "wire-"))


(defun wire-contains-module-connector-p (w)
  "Test whether W contains a module-level connector.

A module-level connectoris one represented by just a symbol, not a list (i.e.,
not to a sub-component).

Return the index of the module-level connector in W, or NIL."
  (position-if #'atom w))


(defun extract-module-connector-to-wire (w i)
  "Extract the  module conector at position I in W.

Return a two-element list consisting of the module connector
and the rest of the connectors."
  (let ((wire (elt w i))
	(connectors (append (butlast w (- (length w) i))
			    (last w (- (length w) (1+ i))))))
    (list wire connectors)))


(defun create-module-connector-to-wire (w)
  "Create a new module connector to W.

Return a two-element list consisting of a new module connector
and the other connectors."
  (let ((wire (new-wire-name)))
    (list wire w)))


(defun find-or-create-module-connector (w)
  "Search W for a module-level conenctor.

If such a connector is found, return a list consisting of two elements,
that wire and the rest of the connectors. Otherwise, create a new wire
name and return that and all the connectors."
  (if (null w)
      ;; no connectors, no wire
      (list nil nil)

      ;; extract or create wire
      (if-let ((i (wire-contains-module-connector-p w)))
	(extract-module-connector-to-wire w i)
	(create-module-connector-to-wire w))))


(defun connect-subcomponents-on-wire (c w)
  "Wire-up connectors of W to sub-components and slots of C.

Set the appropriate slots to contain the correct symbols.
Return a new wire name if one needs to be created."
  (let* ((i (wire-contains-module-connector-p w))
	 (wcs (if i
		  (extract-module-connector-to-wire w i)
		  (create-module-connector-to-wire w))))

    (destructuring-bind (wire connectors)
	wcs
      (dolist (connector connectors)
	(destructuring-bind (subcomponent-slot slot)
	    connector
	  (let ((subcomponent (slot-value c subcomponent-slot)))
	    (setf (slot-value subcomponent slot) wire))))

      ;; return the new wire, if there was one
      (if (null i)
	  wire))))


;;---------- Components ----------

(defmethod to-rtl ((c component))
  (let* ((modname (class-name (class-of c)))
	 (modparams (generate-module-params c))
	 (modargs (generate-module-args c))
	 (modheader (if modparams
			`(,modparams ,modargs)
			modargs))
	 (wiring (generate-wiring c)))

    `(module ,modname ,modheader
	     ,@wiring
	     )))


;; ---------- Mixins ----------
