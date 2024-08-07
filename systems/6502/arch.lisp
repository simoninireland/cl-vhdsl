;; 6502 architecture definition
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

(in-package :cl-vhdsl/6502)

(defparameter *6502-system* (make-instance 'architecture)
  "6502 system.")


(defparameter *MOS6502* (make-instance 'core)
  "6502 core.")


(dolist (r (list
	    (make-instance 'data-register :name 'A :width 8
					      :documentation "The accumulator.")
	    (make-instance 'index-register :name 'X :width 8
					       :documentation "Index register X.")
	    (make-instance 'index-register :name 'Y :width 8
					       :documentation "Index register Y.")
	    (make-instance 'program-counter :name 'PC :width 16
						:documentation "Program counter.")
	    (make-instance 'index-register :name 'SP :width 8
					       :documentation "Stack pointer (offset).")
	    (make-instance 'special-register :name 'P :width 8
						 :documentation "Flags register.")))
  (setf (gethash (register-name r) (core-registers *MOS6502*)) r))


(dolist (f (list
	    (make-instance 'flag :name 'C :register 'P :bit 0
				     :documentation "Carry flag.")
	    (make-instance 'flag :name 'Z :register 'P :bit 1
				     :documentation "Zero flag.")))
  (setf (gethash (flag-name f) (core-flags *MOS6502*)) f))


(setf (architecture-components *6502-system*)
      (list
       :core *MOS6502*
       :memory (make-instance 'memory :size (* 8 KB))
       :address-bus (make-instance 'bus :connections '(:core :memory))
       :data-bus (make-instance 'bus :connections '(:core :memory))))
