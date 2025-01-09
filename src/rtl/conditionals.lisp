;; Synthesisable conditionals
;;
;; cl-vhdsl is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-vhdsl. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :cl-vhdsl/rtl)
(declaim (optimize debug))


;; ---------- if ----------

(defmethod typecheck-sexp ((fun (eql 'if)) args env)
  (destructuring-bind (condition then &rest else)
      args
    (let ((tycond (typecheck condition env))
	  (tythen (typecheck then env))
	  (tyelse (if else
		      (typecheck (cons 'progn else) env))))
      (ensure-boolean tycond env)

      ;; the type of the expression is the widest of the
      ;; types of the two arms
      (if else
	  (lub tythen tyelse env)
	  tythen))))


(defmethod synthesise-sexp ((fun (eql 'if)) args (context (eql :inblock)))
  (destructuring-bind (condition then &rest else)
      args
    (format *synthesis-stream* "~aif("
	    (indentation))
    (synthesise condition :inexpression)
    (format *synthesis-stream* ") then~&")
    (as-body (list then) :inblock :before "begin" :after "end")
    (when else
      (format *synthesis-stream* "~aelse~&" (indentation))
      (as-body (if (> (length else) 1)
		   `((progn ,@else))
		   else)
	       :inblock :before "begin" :after "end"))))


;; ---------- case ----------

(defun typecheck-clause (clause ty env)
  "Typecheck case CLAUSE in ENV.

The value of the cluase should have a type compatible with TY.
Return the type of the clause body."
  (destructuring-bind (val &rest body)
      clause
    (if (not (eql val 't))
	(let ((tyval (typecheck val env)))
	  (ensure-subtype tyval ty)))
    (typecheck (cons 'progn body) env)))


(defun typecheck-clauses (clauses ty env)
  "Typecheck case CLAUSES in ENV.

The clauses' test values should be compatible with TY.
The type is the lub of the clause types."
  (foldr (lambda (tyl clause)
	   (lub tyl (typecheck-clause clause ty env) env))
	 clauses nil))


(defmethod typecheck-sexp ((fun (eql 'case)) args env)
  (destructuring-bind (condition &rest clauses)
      args
    (let ((ty (typecheck condition env)))
      (typecheck-clauses clauses ty env))))


(defun synthesise-clause (clause context)
  "Synthesise case CLAUSE in CONTEXT."
  (destructuring-bind (val &rest body)
      clause
    (if (eql val 't)
	(format *synthesis-stream* "default")
	(synthesise val :inexpression))
    (format *synthesis-stream* ":~%")
    (as-body body context
	     :before "begin"
	     :after "end"
	     :always nil)))


(defmethod synthesise-sexp ((fun (eql 'case)) args (context (eql :inblock)))
  (destructuring-bind (condition &rest clauses)
      args
    (format *synthesis-stream* "case (")
    (synthesise condition :inexpression)
    (format *synthesis-stream* ")~%")
    (with-indentation
      (as-body clauses :inblock :process #'synthesise-clause))
    (format *synthesis-stream* "endcase~%")))
