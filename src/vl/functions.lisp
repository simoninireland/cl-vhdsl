;; Functions
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


;; ---------- Typechecking declarations ----------

(defun function-type-p (type)
  "Test whether TYPE is a function type"
  (eql (if (atom type)
	   type
	   (car type))
       'function))


(defun ensure-function-type (type)
  "Ensure that TYPE is a function type.

Signal TYPE-MISMATCH is not."
  (unless (function-type-p type)
    (error 'type-mismatch :expected "function type" :got type
			  :hint "Is the variale declared as a function?")))


(defun function-return-type (type)
  "Return the return type of function type TYPE.

If there is no return type defined, return UNSIGNED-BYTE."
  (if (and (listp type)
	   (= (length type) 3))
      (caddr type)

      ;; no return type given
      'unsigned-byte))


(defun typecheck-function (decl)
  "Typecheck DECL as a function.

At present we only support functions of no arguments and with
single-expression bodies."
  (declare (optimize debug))

  (with-current-form decl
    (destructuring-bind (n arglist &rest body)
	decl

      ;; check arguments and body
      (unless (null arglist)
	(warn 'type-mismatch :expected "no arguments" :got arglist
			     :hint "Currently Verlisp only allows functions of no arguments"))
      (unless (= (length body) 1)
	(warn 'type-mismatch :expected "expression" :got body
			     :hint "Currently Verilisp only allows single-expression bodies in functions"))

      (let* ((expr (car body))
	     (ty (typecheck expr)))
	;; declare the function
	(declare-variable n `((:as :function)
			      (:type (function () ,ty))
			      (:initial-value ,expr)))))))


(defun typecheck-function-env (decls)
  "Typecheck function declarations DECLS."
  (mapc #'typecheck-function (decls-without-cached-frame decls)))


(defmethod typecheck-sexp ((fun (eql 'flet)) args)
  (destructuring-bind (decls &rest body)
      args

    (with-new-frame
      (typecheck-function-env decls)

      ;; typecheck body
       (prog1
	   (let ((ty (typecheck `(progn ,@body))))
	     (ensure-fixed-width ty)

	     ty)

	 ;; cache the shallowest frame for use in later passes
	 (setf (cdr (last decls))
	       (list (list 'frame (detach-frame *global-environment*))))))))


;; ---------- Typechecking calls ----------

(defun typecheck-function-call-sexp (fun args)
  "Typecheck function FUN applied to ARGS."
  (let ((type (get-type fun)))
    (ensure-function-type type)

    (let ((rtype (function-return-type type)))

      ;; only no-argument functions for now
      (unless (null args)
	(warn 'type-mismatch :expected "no arguments" :got args
			      :hint "Currently Verlisp only allows functions of no arguments"))

      ;; type of call is the return type
      rtype)))


;; ---------- Macro expansion ----------

(defun expand-macros-function-decl (decl)
  "Expand macros in the body of DECL."
  (destructuring-bind (n arglist &rest body)
      decl
    `(,n ,arglist ,(expand-macros `(progn ,@body)))))


(defmethod expand-macros-sexp ((fun (eql 'flet)) args)
  (destructuring-bind (decls &rest body)
      args
    (let ((newdecls (mapcar #'expand-macros-function-decl decls))
	  (newbody (expand-macros (cons 'progn body))))
      `(flet ,newdecls
	 ,newbody))))


;; ---------- PROGN simplificartion ----------

(defun simplify-progn-function-decl (decl)
  "Simplify the body of DECL."
  (destructuring-bind (n arglist &rest body)
      decl
    (let ((newbody (mapcar #'simplify-progn body)))
      `(,n ,arglist ,(simplify-implied-progn newbody)))))


(defmethod simplify-progn-sexp ((fun (eql 'flet)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args
    (let ((newdecls (mapcar #'simplify-progn-function-decl decls))
	  (newbody (mapcar #'simplify-progn body)))
      `(flet ,newdecls
	 ,(simplify-implied-progn newbody)))))


;; ---------- Floating ----------

;; This is the same as for LET, and should be refatored.

(defmethod float-let-blocks-sexp ((fun (eql 'flet)) args)
  (declare (optimize debug))

  (destructuring-bind (decls &rest body)
      args

    (destructuring-bind (newbody newenv)
	(float-let-blocks `(progn ,@body))

      ;; add our declarations to the environment
      (when (null newenv)
	(setq newenv (make-frame)))
      (let ((f (get-cached-frame decls)))
	;; add the new declarations to the front of NEWENV
	(add-frame-to-environment f newenv t)

	;; return the re-written body and the new environment
	(list newbody newenv)))))


;; ---------- Synthesis of declarations ----------

(defun synthesise-function (decl)
  "Synthesise DECL as a function."
  (destructuring-bind (n arglist &rest body)
      decl

    (let* ((type (caddr (get-type n)))
	   (width (bitwidth type))
	   (v (get-initial-value n)))

      (as-literal "wire ")
      (when (or (not (numberp width))
		(> width 1))
	;; we have a width (or a width expression)
	(as-literal"[ ")
	(synthesise width)
	(as-literal " - 1 : 0 ] "))
      (synthesise n)

      ;; synthesise the body
      (as-literal " = ")
      (synthesise v)
      (as-literal";"))))


(defmethod synthesise-sexp ((fun (eql 'flet)) args)
  (destructuring-bind (decls &rest body)
      args

    (with-frame (get-cached-frame decls)
      (let ((real-decls (decls-without-cached-frame decls)))
	;; synthesise the functions
	(as-block-forms real-decls :process #'synthesise-function)

	(if (> (length real-decls) 0)
	    (as-blank-line))

	;; synthesise the body
	(as-block-forms body)))))


;; ---------- Synthesis of calls ----------

(defun synthesise-function-call-sexp (fun args)
  "Synthesise a call to FUN with ARGS."
  (let ((type (get-type fun)))
    ;; function becomes access to a wire
    (as-literal fun)))
