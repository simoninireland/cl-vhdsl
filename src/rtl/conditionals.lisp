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


(defmethod synthesise-sexp ((fun (eql 'if)) args (as (eql :statement)))
  (destructuring-bind (condition then &rest else)
      args
    (format *synthesis-stream* "~aif("
	    (indentation))
    (synthesise condition :rvalue)
    (format *synthesis-stream* ") then~&")
    (in-logical-block (:before "begin" :after "end" :always nil)
      (synthesise then :statement))
    (if else
	(progn
	  (format *synthesis-stream* "~aelse~&" (indentation))
	  (in-logical-block (:before "begin" :after "end" :always nil)
	    (synthesise (cons 'progn else) :statement))))))
