;; Tests of STATE-MACHINE macros
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

(in-package :cl-vhdsl/test)
(in-suite cl-vhdsl/rtl)
(declaim (optimize debug))


;; It's tricky to test the output automatically, so we
;; only test for errors.

(test test-state-machine-jumps
  "Test we can jump within a state machine."
  (is (rtl:expand-macros '(rtl:state-machine
			   (test-even
			    (incf a)
			    (rtl::next test-odd))
			   (test-odd
			    (incf a)
			    (if (> a 10)
				(rtl::next test-idle)
				(rtl::next test-even)))
			   (test-idle)))))


(test test-state-machine-nested
  "Test we can expand a nested pair of a state machines."
  (is (rtl:expand-macros '(rtl:state-machine
			(test-even
			 (incf a)
			 (rtl::next test-odd))
			(test-odd
			 (incf a)
			 (if (> a 10)
			     (rtl::next test-idle)

			     (rtl:state-machine
			       (nested-one
				(rtl::next nested-two))
			       (nested-two
				(setq a 5)
				(rtl::exit test-even)))))))))


(test test-state-machine-reenter
  "Test we can re-enter a nested machine."
  (is (rtl:expand-macros '(rtl:state-machine
			    (test-even
			     (incf a)
			     (rtl::next test-odd))
			    (test-odd
			     (incf a)
			     (if (> a 10)
				 (rtl::next test-idle)

				 (rtl:state-machine
				   (nested-one
				    (rtl::next nested-two))
				   (nested-two
				    (setq a 5)
				    (rtl::exit)))))))))


(test test-state-machine-before-after
  "Test we include state machine before and after code."
  (is (rtl:expand-macros '(rtl:state-machine
			   (:before
			    (setf a 0))
			   (:after
			    (setf a 255))
			   (test-even
			    (incf a)
			    (rtl::next test-odd))
			   (test-odd
			    (incf a)
			    (if (> a 10)
				(rtl::next test-odd)
				(rtl::next test-even)))))))


(test test-state-machine-not-state
  "Test we detect jumping to an undefined state."
  (signals (rtl:state-machine-mismatch)
    (rtl:expand-macros '(rtl:state-machine
			 (test-even
			  (incf a)
			  (rtl::next test-ttt))
			 (test-odd
			  (incf a))))))


(test test-state-machine-not-before
  "Test we detect jumping to a special state."
  (signals (rtl:state-machine-mismatch)
    (rtl:expand-macros '(rtl:state-machine
			 (test-even
			  (incf a)
			  (rtl::next test-ttt))
			 (:before
			  (setf a 0))
			 (test-odd
			  (incf a)
			  (rtl::next :before))))))


(test test-state-machine-not-surrounding-state
  "Test we detect exiting to an undefined state in the surrounding machine."
  (signals (rtl:state-machine-mismatch)
    (rtl:expand-macros '(rtl:state-machine
			 (test-even
			  (incf a)
			  (rtl::next test-odd))
			 (test-odd
			  (incf a)
			  (rtl:state-machine
			      (nested-one
			       (rtl::next nested-two))
			      (nested-two
			       (setq a 5)
			       (rtl::exit test-ttt))))))))


(test test-state-machine-not-next-state
  "Test we detect trying to jump (rather than exit) to a state in the surrounding machine."
  (signals (rtl:state-machine-mismatch)
    (rtl:expand-macros '(rtl:state-machine
			 (test-even
			  (incf a)
			  (rtl::next test-odd))
			 (test-odd
			  (incf a)
			  (rtl:state-machine
			    (nested-one
			     (rtl::next nested-two))
			    (nested-two
			     (setq a 5)
			     (rtl::next test-even))))))))
