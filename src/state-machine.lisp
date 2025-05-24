;; State machine construction
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :vl)
(declaim (optimize debug))


;; ---------- Special states ------------------

(defvar *special-states* '(:before :after)
  "The \"special\" states of a state machine, used
for its internal working.")


;; ---------- Parser helpers ----------

(defun extract-state-labels (body)
  "Return a list of state labels from BODY.

Each clause in BODY is a list consisting of a label and one or more
forms making up the code executed in that state."
  (mapcar #'car body))


(defun special-state-p (state)
  "Test whether STATE is \"special\".

Special states are used internally in building the behaviour of the
state machine."
  (member state *special-states*))


(defun extract-user-state-labels (body)
  "Return a list of state labels not including the special states."
  (remove-if #'special-state-p
	     (extract-state-labels body)))


(defun next-state-p (state labels)
  "Test whether STATE is a valid next state in LABELS.

:BEFORE and :AFTER are never valid next states."
  (and (member state labels)
       (not (special-state-p state))))


(defun ensure-next-state (state labels)
  "Ensure that STATE is a valid next state in LABELS.

Signal a STATE-MACHINE-MISMATCH error if STATE is invalid as a next
state, either because it is unknown or because it is special.."
  (unless (next-state-p state labels)
    (error 'state-machine-mismatch :state state
				   :hint "Make sure the target state is defined in the state machine")))


(defun action-for-state (state body)
  "Return the forms that constitute the actions of STATE in BODY.

This can be used to access the actions of both user and special
states."
  (if-let ((m (assoc state body)))
    (cdr m)))


;; ---------- Top-level macros ----------

;; We make use of the DSL and intercept the macro-expander directly
;; so we can contextually expand the macros within.
;;
;; It's still a macro, though, because it expands the Verilisp source
;; into more Verilisp source, and doesn't complicate the core. It does
;; however need more macro-expansion machinery than normal.

(defvar *current-machine-state* nil
  "Variable holding the current state of the current state machine.")


(defvar *current-machine-initial-state* nil
  "Variable holding the initial state of the current state machine.")


(defvar *current-machine-states* nil
  "Variable holding all the states of the current state machine.")


(defvar *surrounding-machine-state* nil
  "Variable holding the current state of the enclosing state machine.")


(defvar *surrounding-machine-states* nil
  "Variable holding all the state of the enclosing state machine.")


(defmethod expand-macros-sexp ((fun (eql 'state-machine)) args)
  "Construct a state machine.

BODY defines a set of state/action pairs roughly in the style of CASE.
Each entry is a list with a symbol (the state label) as its head and
the rest being the action code executed when the machine executes that
state.

The action in a state may include occurrences of the NEXT macro
which change the machine's next state. Otherwise the next time
the machine turns it will remain in the same state.

There are two labels reserved for special purposes. The actions in
the :BEFORE state happen before each turn of the state machine, and
so can affect the state that will execute. The :AFTER state happens
every time the machine turns."
  (declare (optimize debug))
  (destructuring-bind (&rest body)
      args
    (let* ((user-state-labels (extract-user-state-labels body))
	   (initial-state (car user-state-labels))
	   (user-state-decls (mapcar (lambda (state i)
				       (list state i :as :constant))
				     user-state-labels
				     (iota (length user-state-labels))))
	   (before-forms (action-for-state :before body))
	   (after-forms (action-for-state :after body))
	   (state-actions (remove-if (lambda (clause)
				       (special-state-p (car clause)))
				     body)))

      (with-gensyms (state)
	(let* ((*surrounding-machine-state* *current-machine-state*)
	       (*surrounding-machine-states* *current-machine-states*)
	       (*current-machine-state* state)
	       (*current-machine-initial-state* initial-state)
	       (*current-machine-states* user-state-labels))

	  ;; recursively expand the body forms
	  (expand-macros `(let ,user-state-decls
			    (let ((,state ,initial-state))

			      ;; actions always performed before each turn (if any)
			      ,@before-forms

			      ;; the state machine proper
			      (case ,state
				,@state-actions)

			      ;; actions always performed after each turn (if any)
			      ,@after-forms))))))))


(defmacro next/vl (next-state)
  "Move the current machine to NEXT-STATE."
  (ensure-next-state next-state *current-machine-states*)
  `(setq ,*current-machine-state* ,next-state))


(defmacro exit/vl (&optional exit-state)
  "Exit the current machine to EXIT-STATE of the surrounding machine.

The current machine is returned to its initial state.

If EXIT-STATE is omitted, the state of the surrounding machine is left
unchanged. The net result will be that, the next time the surrounding
machine turns, it will re-enter the current machine in that machine's
initial state."
  `(progn
     (setq ,*current-machine-state* ,*current-machine-initial-state*)
     ,(when exit-state
	  (ensure-next-state exit-state *surrounding-machine-states*)
	  `(setq ,*surrounding-machine-state* ,exit-state))))
