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
