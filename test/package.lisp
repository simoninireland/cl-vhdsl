;; Top-level test package
;;
;; Copyright (C) 2023 Simon Dobson
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

(defpackage cl-vhdsl/test
  (:use :cl :alexandria :fiveam
	:cl-vhdsl)
  (:local-nicknames ;;(:dsl :cl-vhdsl/dsl)
		    (:rtl :cl-vhdsl/rtl)
		    (:def :cl-vhdsl/def)
		    )
  (:import-from :fiveam #:is #:test))

(in-package :cl-vhdsl/test)

(def-suite cl-vhdsl)       ;; utilities
;;(def-suite cl-vhdsl/dsl)   ;; DSL definition
(def-suite cl-vhdsl/rtl)   ;; synthesisable fragment
(def-suite cl-vhdsl/def)   ;; architectural component definitions


;; ---------- File access relative to the project root ----------

;; This lets tests access data files stored elsewhere in the project
;; https://stackoverflow.com/questions/70239407/how-can-i-get-the-current-file-name-in-common-lisp

(defparameter *this-file* #.(or *compile-file-truename* *load-truename*)
	      "The pathname of this file.")

(defparameter *project-root* (butlast (pathname-directory *this-file*))
  "The pathname to the project root directory.")


(defun pathname-relative-to-project-root (fn)
  "Return the pathname to FN relative to the project root."
  (let* ((p (parse-namestring fn))
	 (pdir (cdr (pathname-directory p))) ; remove :relative
	 (pname (pathname-name p))
	 (ptype (pathname-type p)))
    (make-pathname :directory (append *project-root* pdir)
		   :name pname
		   :type ptype)))
