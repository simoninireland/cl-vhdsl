;; Base class for fully-software-emulated hardware components
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

(in-package :cl-vhdsl/hw)

;; ---------- Components ----------

(defclass component ()
  ((name
    :documentation "The readable name of the component."
    :initarg :name
    :reader name))
  (:metaclass metacomponent)
  (:documentation "A component in an architecture.

Components encapsulate functions and offer a pin-based interface."))


;; Initialisation of components is split into two parts. We first
;; override the primary method for `initialize-instance' to create the
;; pins for all slots in the pin interface for which we know their
;; width. We then add an :around method that calls
;; `component-pins-changed' to let the component set up its internal
;; state to be consistent with its initial pin values.
;;
;; The reason to separate these, and for using the :around method, is
;; to support sub-class initialisation. The new primary method runs
;; first and calls the underlying `initialize-instance' method to
;; create and populate other slots. This will run any :before methods,
;; then run the overridden primary method, and then run any :after
;; methods on `initilize-instance' that sub-classes might define.
;; These :after methods will see an object whose pins have been
;; initialised, as expected. The :around method then calls
;; `component-pins-changed' in an environment where the pins *and* any
;; other state has been initialised, which means it sees the component
;; in a sensible state.

;; This method should be decomposed to let the individual elements be
;; called/overridden programmatically

(defun component-width-of-slot (c slot-def)
  "Return the width of SLOT-DEF on C."
  (let ((w (slot-value slot-def 'pins)))
    (if (and (symbolp w)
	     (not (eql w t))) ;; t = undefined
	;; width derived from the value of another slot
	(slot-value c w)

	;; width as given
	w)))


(defmethod initialize-instance ((cc component) &rest initargs)
  (declare (ignore initargs))

  (let* ((c (call-next-method))
	 (slot-defs (class-slots (class-of c))))
    (dolist (slot-def slot-defs)
      (when (slot-def-in-pin-interface-p slot-def)
	;; slot is in the pin interface
	(let* ((slot (slot-definition-name slot-def))

	       ;; number of wires in the slot
	       (width (component-width-of-slot c slot-def))

	       ;; role the slot fulfills
	       ;; (set by `compute-effective-slot-definition' if omitted)
	       (role (slot-value slot-def 'role))

	       ;; wires the slot's pins should be connected to
	       (bus (if (slot-exists-and-bound-p c slot)
			(slot-value c slot))))

	  ;; sanity check any wiring
	  (when bus
	    ;; we accept single wires and single pins
	    (unless (typep bus 'bus)
	      (let ((wire-or-pin bus))
		(etypecase wire-or-pin
		  (wire
		   (setq bus (make-instance 'bus :width 1))
		   (setf (elt (wires bus) 0) wire-or-pin))
		  (pin
		   (setq bus (make-instance 'bus :width 1))
		   (setf (elt (wires bus) 0) (wire wire-or-pin))))))

	    ;; check widths match
	    (let ((bus-width (width bus)))
	      (cond ((eql width t)
		     ;; no width, set it to the number of
		     ;; wires on the bus we've been given
		     (setq width bus-width))

		    ;; wrong number of wires, signal an error
		    ((not (equal bus-width width))
		     (error 'mismatched-wires
			    :component c
			    :slot slot
			    :expected width
			    :received bus-width)))))

	  ;; create the connector
	  (setf (slot-value c slot)
		(if (not (equal width t))
		    (let ((conn (make-instance 'connector :width width
							  :component c
							  :role role)))

		      ;; if we've been given a bus, connect it
		      (when bus
			(connect-pins conn bus))

		      conn))))))

    ;; return the instance
    c))


(defmethod initialize-instance :around ((c component) &rest initargs)
  (declare (ignore initargs))

  ;; do the normal initialisation routines
  (call-next-method)

  ;; wire-up the component internally
  (connect-component c)

  ;; make sure we're in a state consistent with our initial pins
  (on-pin-changed c))


;; ---------- Pin interface ----------

(defun find-pin-slot-def (cl slot &key fatal)
  "Return the slot definition for SLOT in CL.

If FATAL is set to T an error is raised if SLOT does
not exist or isn't part of the pin interface."
  (flet ((pin-slot-named (slot-def)
	   (and (slot-def-in-pin-interface-p slot-def)
		(equal (slot-definition-name slot-def) slot))))
    (if-let ((slot-defs (remove-if-not #'pin-slot-named
				       (class-slots cl))))
      (car slot-defs)

      (if fatal
	  (error "No slot named ~s in pin interface of ~s"
		 slot cl)))))


(defun slot-def-in-pin-interface-p (slot-def)
  "Test whether SLOT-DEF defines a slot composed of pins.

This tests for the existence of the :pins slot option in the
slot definition."
  (slot-exists-and-bound-p slot-def 'pins))


(defun pin-interface (cl)
  "Return a list of all the slots of class CL comprising its pin interface."
  (let ((slot-defs (remove-if-not #'slot-def-in-pin-interface-p
				  (class-slots cl))))
    (mapcar #'slot-definition-name slot-defs)))


(defun pin-interface-p (cl slot)
  "Test whether SLOT is in the pin interface of class CL.

Returns nil if SLOT either isn't a slot in CL's pin
interface, or isn't a slot of CL at all."
  (not (null (find-pin-slot-def cl slot))))


(defun pin-role-for-slot (cl slot)
  "Return the role assigned to the pins of SLOT in class CL.

SLOT must be in C's pin interface."
  (let ((slot-def (find-pin-slot-def cl slot :fatal t)))
    (slot-value slot-def 'role)))


(defun pin-slots-for-roles (cl roles)
  "Extract all the slots in the pin interface of CL having roles in ROLES."
  (remove-if-not #'(lambda (slot)
		     (member (pin-role-for-slot cl slot) roles))
		 (pin-interface cl)))


;; ---------- Behavioural interface ----------

(defgeneric on-pin-changed (c)
  (:method-combination guarded)
  (:documentation "Callback called when the calues asserted on pins of component C change.

This happens for changes on `:reading' and `:control' pins only.
Changes to tristated pins are ignored; changes to `:trigger' pins
cause a `pin-triggered' callback.

The methods on this function are guarded, meaning that methods may
implement the `:if' qualifier to prevent execution of the method if
the guard evaluates to false.")

  ;; default callback is empty
  (:method ((c component))))


(defgeneric on-pin-triggered (c p v)
  (:method-combination guarded)
  (:documentation "Callback called when trigger pin P on component C transitions to V.

This method can be specialised using `eql' to only be fired on (for example)
rising transitions.

The methods on this function are guarded, meaning that methods may
implement the `:if' qualifier to prevent execution of the method if
the guard evaluates to false.")

  ;; default callback is empty
  (:method ((c component) p v)))


(defmethod pins (c)
  (let* ((cl (class-of c))
	 (pin-slots (pin-interface cl)))
    (flatten (map 'list #'(lambda (slot)
			    (coerce (pins (slot-value c slot)) 'list))
		  pin-slots))))


(defmethod fully-wired-p ((c component))
  (let ((pin-slots (pin-interface (class-of c))))
    (every #'(lambda (slot)
	       (fully-wired-p (slot-value c slot)))
	   pin-slots)))


;; ---------- Sub-components ----------

(defun subcomponent-p (cl slot)
  "Test whether SLOT holds a sub-component on class CL.

The component type of SLOT is returned if it is a component,
otherwise `NIL'."
  (if-let ((slot-def (find slot (class-slots cl) :key #'slot-definition-name)))
    (let ((slot-type (slot-definition-type slot-def) ))
      (when (subtypep slot-type 'component)
	slot-type))))


(defun subcomponent-interface (cl)
  "Return the slots holding sub-components on class CL."
  (let ((slot-names (mapcar #'slot-definition-name (class-slots cl))))
    (remove-if-not #'(lambda (slot)
			  (subcomponent-p cl slot))
		      slot-names)))


;; sd: Should the definition of `components' when called on a component
;; include the component itself? -- or just its sub-components, as at present?

(defmethod components ((c component))
  (mapcar #'(lambda (slot)
	      (slot-value c slot))
	  (subcomponent-interface (class-of c))))


;; ---------- Mixins for common components ----------

;; Enable-able components
(defclass enabled ()
  ((enable
    :documentation "The component-enable pin."
    :initarg :enable
    :pins 1
    :role :control)
   (enabled
    :documentation "Flag recording the component's enablement."
    :initform nil
    :accessor enabled))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that can be enabled.

Enable-able components only respond to changes in their pin interface
when they are enabled. The states of their pins are left unchanged."))

(defmethod enabled-p ((c enabled))
  (let ((en (elt (pins (slot-value c 'enable)) 0)))
    (and (fully-wired-p en)
	 (not (floating-p en))
	 (equal (state en) 1))))


;; Guards to prevent callbacks on disabled components
(defmethod on-pin-changed :if ((c enabled))
  (let ((en (enabled-p c))
	(pre (enabled c)))

    ;; re-direct the callback if the enabled status has changed
    (cond ((and en (not pre))
	   ;; component has become enabled
	   (on-enable c))

	  ((and (not en) pre)
	   ;; component has become disabled
	   (on-disable c)))
    (setf (enabled c) en)

    ;; return whether we're now enabled
    en))


(defmethod on-pin-triggered :if ((c enabled) p v)
  (enabled-p c))


;; Callbacks for enablement and disablement

;; TODO: Should we tristate :io and :reading pins, or indeed all pins except
;; enable, when the component is disabled?

(defgeneric on-enable (c)
  (:documentation "Callback called when a component is enabled.")

  ;; default callback is empty
  (:method ((c enabled))))


(defgeneric on-disable (c)
  (:documentation "Callback called when a component is disabled.")

  ;; defalt callback is empty
  (:method ((c enabled))))


;; Clocked components
(defclass clocked ()
  ((clock
    :documentation "The component's clock pin."
    :initarg :clock
    :pins 1
    :role :trigger
    :reader clock))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that has a clock line.

Clocked components do most of their active work when the clock
transitions, although they can change state at other times too."))


;; Read/write components
(defclass readwrite ()
  ((write-enable
    :documentation "The component's write-enable pin."
    :initarg :write-enable
    :pins 1
    :role :control
    :reader write-enable))
  (:metaclass metacomponent)
  (:documentation "A mixin for a component that has a write-enable line..

This provides a common control line for determining whether a component
reads data from a bus (when write-enable is high) or makes data available
on the bus. 'Write' should be seen from the perspective of ourside the
component.

This only works for components with a single decision on read or write.
More complicated components might need several such control lines."))


(defmethod write-enabled-p ((c readwrite))
  (equal (state (elt (pins (slot-value c 'write-enable)) 0)) 1))


(defmethod read-enabled-p ((c readwrite))
  (not (write-enabled-p c)))


;; ---------- Integral wiring diagrams ----------

(defgeneric connect-component (c)
  (:documentation "Internally wire C according to its wiring diagram.

Methods on this function enact any wiring diagrams. They are
synthesised for `component' classes and called automatically when the
class is instanciated."))


;; No universally shared wiring -- although naybe there should be, to
;; a clock?
(defmethod connect-component ((c component)))


;; These parse functions should probably signal errors to
;; highlight where the problem is

(defun parse-wire-description-endpoint (cl endpoint)
  "Parse ENDPOINT against class CL.

Return `T' if the endpoint is valid, meaning that it is a symbol
naming a pin slot on CL or a pair of symbols naming a sub-component
slot on CL and a pin slot on that sub-component."
  (cond ((symbolp endpoint)
	 (pin-interface-p cl endpoint))

	((consp endpoint)
	 (let* ((cslot (car endpoint))
		(slot (if (consp (cdr endpoint))
			  (cadr endpoint)  ;; for lists
			  (cdr endpoint))) ;; for pairs
		(slot-type (subcomponent-p cl cslot)))
	   (and slot-type
		(parse-wire-description-endpoint slot-type slot))))

	(t
	 nil)))


(defun parse-wire-description (cl wiredesc)
  "Parse WIREDESC against class CL and check validity.

Return `T' if the wire description is valid, mmeaning all its
endpoints are valid."
  (every #'(lambda (endpoint)
	     (parse-wire-description-endpoint cl endpoint))
	 wiredesc))


(defun parse-wiring-diagram (cl diagram)
  "Parse the wiring DIAGRAM for class CL.

Parsing is possibly the wrong word, since we just return the wire
lists. But these are checked to ensure that they're all consistent
with the definition of CL."
  (if (wiring-diagram-p diagram)
      (let ((wires (cdr diagram)))
	(if (every #'(lambda (wiredesc)
			(parse-wire-description cl wiredesc))
		    diagram)
	    wires))

      (error "Not a wiring diagram")))


;; This should perhaps be the top-level method, including the class?

(defun wiring-diagram-p (arg)
  "Test ARG is a wiring diagram.

Wiring diagrams are class arguments that start with
`:wiring'."
  (equal (car arg) :wiring))


(defun generate-wiring-method (name wiring)
  "Generate a wiring method from the given WIRING diagram on class NAME."
  (with-gensyms (c)
    (flet ((generate-connect (wires)
	     (destructuring-bind (from to)
		 wires
	       `(connect-slots ,c ',(ensure-list from)
			       ,c ',(ensure-list to)))))

      (let ((wirings (mapcar #'generate-connect (cdr wiring))))
	`(defmethod connect-component ((,c ,name))
	   ,@wirings)))))


;; Should add sanity checks on superclasses, make sure there's no
;; incompatible metaclass specified.

(defmacro defcomponent (name superclasses slots &rest args)
  "Define a new component NAME.

This follows the same pattern as `defclass', with a possibly
empty list of SUPERCLASSES followed by a list of SLOTS and
further ARGS. The differences are:

- NAME is always a sub-class of `component', which is added
  if no SUPERCLASSES are specified
- NAME is given the `metacomponent' metaclass
- NAME can have a `:wiring' option providing a wiring diagram
  describing how any sub-components should be wired-up when
  an object is instanciated
- The wiring diagram is used to synthesise a method on the
 `connect-component' generic function. "

  ;; components default to sub-classes of `component'
  (when (null superclasses)
    (setq superclasses (list 'component)))

  ;; if we have a wiring diagram, generate the wiring method
  (let ((wiring (find-if #'wiring-diagram-p args))
	wiring-method)
    (when wiring
      (setq args (remove-if #'wiring-diagram-p
			    args))
      (setq wiring-method (generate-wiring-method name wiring)))

    `(progn
       ;; class definition
       (defclass ,name (,@superclasses)
	 ,slots
	 (:metaclass metacomponent)
	 ,@args)

       ;; wiring method definition (if any)
       ,wiring-method
       )))
