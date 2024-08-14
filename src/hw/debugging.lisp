;; Some debgging support code
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

;; ---------- Component visibility ----------

(defgeneric components-seen-by (c)
  (:documentation "Return a list of components 'seen' by C.

C may be a component, a pin, or some other element. Including
a component in the list means that component's pin interface
may affect C in some way."))


(defmethod components-seen-by ((w wire))
  (uniquify (map 'list #'component (pins w))))


(defmethod components-seen-by ((p pin))
  (components-seen-by (wire p)))


(defmethod components-seen-by ((b bus))
  (uniquify (flatten (map 'list #'components-seen-by (wires b)))))


(defmethod components-seen-by ((conn connector))
  (uniquify (flatten (map 'list #'components-seen-by (pins conn)))))


(defmethod components-seen-by ((s sequence))
  (uniquify (flatten (map 'list #'components-seen-by s))))


(defmethod components-seen-by ((c component))
  (let* ((cl (class-of c))
	 (pin-slots (pin-interface cl))
	 (pins (flatten (map 'list #'(lambda (slot)
				       (components-seen-by (slot-value c slot)))
			     pin-slots))))

    ;; remove the component itself
    (remove-if #'(lambda (comp)
		   (equal comp c))
	       (uniquify pins))))
