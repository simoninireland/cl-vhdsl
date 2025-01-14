;; Package definition for component definitions
;;
;; Copyright (C) 2024-2025 Simon Dobson
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

(in-package :common-lisp-user)

(defpackage cl-vhdsl/def
  (:use :cl :alexandria :cl-vhdsl)
  (:import-from :slot-extra-options
		#:def-extra-options-metaclass
		#:slot-exists-and-bound-p)
  (:import-from :closer-mop
		#:finalize-inheritance
		#:validate-superclass
		#:class-slots
		#:slot-boundp
		#:slot-definition-name
		#:slot-definition-type)
  (:import-from :cl-ppcre
		#:scan-to-strings)
  (:local-nicknames (:rtl :cl-vhdsl/rtl))
  (:export
   ;; Constants
   #:KB
   #:MB

   ;; components
   #:synthesisable-component
   #:component
   #:on-startup
   #:pin-interface
   #:subcomponent-interface
   #:wiring-diagram

   ;; mixins and their behaviours
   #:clocked
   #:on-clock
   #:resetable
   #:on-reset

   ;; conditions

   ))
