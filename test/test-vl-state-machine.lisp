;; Tests of STATE-MACHINE macros
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

(in-package :verilisp/test)
(in-suite verilisp/vl)


;; It's tricky to test the output automatically, so we
;; only test for errors.

(test test-state-machine-jumps
  "Test we can jump within a state machine."
  (is (vl:expand-macros '(vl:state-machine
			   (test-even
			    (incf a)
			    (vl::next test-odd))
			   (test-odd
			    (incf a)
			    (if (> a 10)
				(vl::next test-idle)
				(vl::next test-even)))
			   (test-idle)))))


(test test-state-machine-nested
  "Test we can expand a nested pair of a state machines."
  (is (vl:expand-macros '(vl:state-machine
			(test-even
			 (incf a)
			 (vl::next test-odd))
			(test-odd
			 (incf a)
			 (if (> a 10)
			     (vl::next test-idle)

			     (vl:state-machine
			       (nested-one
				(vl::next nested-two))
			       (nested-two
				(setq a 5)
				(vl::exit test-even)))))))))


(test test-state-machine-reenter
  "Test we can re-enter a nested machine."
  (is (vl:expand-macros '(vl:state-machine
			    (test-even
			     (incf a)
			     (vl::next test-odd))
			    (test-odd
			     (incf a)
			     (if (> a 10)
				 (vl::next test-idle)

				 (vl:state-machine
				   (nested-one
				    (vl::next nested-two))
				   (nested-two
				    (setq a 5)
				    (vl::exit)))))))))


(test test-state-machine-before-after
  "Test we include state machine before and after code."
  (is (vl:expand-macros '(vl:state-machine
			   (:before
			    (setf a 0))
			   (:after
			    (setf a 255))
			   (test-even
			    (incf a)
			    (vl::next test-odd))
			   (test-odd
			    (incf a)
			    (if (> a 10)
				(vl::next test-odd)
				(vl::next test-even)))))))


(test test-state-machine-not-state
  "Test we detect jumping to an undefined state."
  (signals (vl:state-machine-mismatch)
    (vl:expand-macros '(vl:state-machine
			 (test-even
			  (incf a)
			  (vl::next test-ttt))
			 (test-odd
			  (incf a))))))


(test test-state-machine-not-before
  "Test we detect jumping to a special state."
  (signals (vl:state-machine-mismatch)
    (vl:expand-macros '(vl:state-machine
			 (test-even
			  (incf a)
			  (vl::next test-ttt))
			 (:before
			  (setf a 0))
			 (test-odd
			  (incf a)
			  (vl::next :before))))))


(test test-state-machine-not-surrounding-state
  "Test we detect exiting to an undefined state in the surrounding machine."
  (signals (vl:state-machine-mismatch)
    (vl:expand-macros '(vl:state-machine
			 (test-even
			  (incf a)
			  (vl::next test-odd))
			 (test-odd
			  (incf a)
			  (vl:state-machine
			      (nested-one
			       (vl::next nested-two))
			      (nested-two
			       (setq a 5)
			       (vl::exit test-ttt))))))))


(test test-state-machine-not-next-state
  "Test we detect trying to jump (rather than exit) to a state in the surrounding machine."
  (signals (vl:state-machine-mismatch)
    (vl:expand-macros '(vl:state-machine
			 (test-even
			  (incf a)
			  (vl::next test-odd))
			 (test-odd
			  (incf a)
			  (vl:state-machine
			    (nested-one
			     (vl::next nested-two))
			    (nested-two
			     (setq a 5)
			     (vl::next test-even))))))))
