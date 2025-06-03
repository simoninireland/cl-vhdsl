;; Instruction set architecture definition
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

(in-package :verilisp/def)


;; ---------- Synthesisable types (should be moved to rtl) ----------

(defun simple-synthesisable-type-p (ty)
  "Test whether TY is a simple synthesisable type.

The simple synthesisable types are those that can be represented
directly in RTL. These include fixed-width numbers (signed and
unsigned) and some variants of these."
  ;; UNSIGNED-BYTE is a sub-type of SIGNED-BYTE
  (or (and (subtypep ty 'signed-byte)
	   (listp ty)
	   (not (eql (cadr ty) '*)))

      ;; BIT is a single bit, so qualifies
      (subtypep ty 'bit)))


(defun synthesisable-type-p (ty)
  "Test whether TY is a synthesisable type.

The synthesisable types are the simple synthesisable types and
arrays of these."
  (or (simple-synthesisable-type-p ty)
      (and (subtypep ty 'array)
	   (listp ty)
	   (simple-synthesisable-type-p (cadr ty)))))


;; ---------- Base class ----------

(defclass instruction ()
  ()
  (:documentation "An instruction."))


(defgeneric as-binary (ins)
  (:documentation "Return the binary form of the instruction INS.

Return a list of bytes to be assembled into memory. The default
returns an empty list.")
  (:method (ins)
    nil))


(defun slot-definition-width (slot-def)
  "Return the width of SLOT-DEF in bits.

SLOT-DEF must be a slot with a specified type, and that type must
be either UNSIGNED-BYTE or SIGNED-BYTE with a width. The width is
simply the number of bits in the representation."
  (let ((ty (slot-definition-type slot-def)))
    (unless (synthesisable-type-p ty)
      (error 'slot-type-mismatch :slot (slot-definition-name slot-def)
				 :type ty
				 :hint "Use a synthesisable type with known width."))

    ;; the width is taken from the type
    ;; TODO: need to deal with arrays?
    (cadr ty)))


(defun instruction-width (ins)
  "Return the number of bits in INS.

This comes from summing-up the widths of all the slots
in INS' class definition."
  ;; this should be stored on the class as part of its definition,
  ;; since it never changes
  (let* ((slot-defs (class-slots (class-of ins)))
	 (widths (mapcar #'slot-definition-width slot-defs)))
    (apply #'+ widths)))


(defun form-bitfield-from-slots (ins slots)
  "Form a bitfield consisting of the named SLOTS of INS.

The class of INS is interrogated to find the widths of each slot,
whose value is then added to the bitfield starting from the
left (most-significant bits)."
  (labels ((build (slots slot-defs widths bs)
	     (if (null slot-defs)
		 bs
		 (let* ((slot-def (car slot-defs))
			(ty (slot-definition-type slot-def))
			(w (car widths))
			(mask (1- (ash 1 w)))
			(v (or (slot-value ins (car slots))
			       0)))
		   (if (> v mask)
		       (warn 'value-too-wide :value v :width w))
		   (build (cdr slots)
			  (cdr slot-defs)
			  (cdr widths)
			  (+ (ash bs w) (logand v mask)))))))

    (let* ((slot-defs (mapcar (curry #'find-slot-def ins)
			      slots))
	   (widths (mapcar #'slot-definition-width slot-defs)))
      (build slots slot-defs widths 0))))


;; ---------- Example RISC-V ----------

(defclass rv32 (instruction)
  ((opcode
    :documentation "The opcode."
    :type (unsigned-byte 7)
    :reader opcode))
  (:documentation "Base class for all RISC-V 32-bit instructions."))


(defclass rv32-r (rv32)
  ((funct3
    :documentation "The function-3 field."
    :type (unsigned-byte 3)
    :reader funct3)
   (funct7
    :documentation "The function-7 field."
    :type (unsigned-byte 7)
    :reader funct7)
   (rs1Id
    :documentation "The first source register identification field."
    :type (unsigned-byte 5)
    :initarg :rs1
    :reader source-register-1)
   (rs2Id
    :documentation "The second source register identification field."
    :type (unsigned-byte 5)
    :initarg :rs2
    :reader source-register-2)
   (rdId
    :documentation "The destination register identification field."
    :type (unsigned-byte 5)
    :initarg :rd
    :reader destination-register))
  (:documentation "RISC-V R-type instructions.

R-type (also known as register-ALU) instructions include two source
and one destination register, and two function fields."))


(defmethod as-binary ((ins rv32-r))
  (form-bitfield-from-slots ins '(funct7
				  rs2Id
				  rs1Id
				  funct3
				  rdId
				  opcode)))


(defmethod argument-initargs ((ins rv32-r))
  (list :rd :rs1 :rs2))


(defclass rv32-i (rv32)
  ((funct3
    :documentation "The function-3 field."
    :type (unsigned-byte 3))
   (rs1Id
    :documentation "The first source register identification field."
    :type (unsigned-byte 5)
    :initarg :rs1
    :reader source-register)
   (immediate
    :documentation "The immediate value field."
    :type (unsigned-byte 12)
    :initarg :v
    :reader immediate-value)
   (rdId
    :documentation "The destination register identification field."
    :type (unsigned-byte 5)
    :initarg :rd
    :reader destination-register))
  (:documentation "RISC-V I-type instructions.

I-type (also known as register-immediate) instructions include one
source register, one destination register, one immediate value, and
one function-3 field."))


(defmethod as-binary ((ins rv32-i))
  (form-bitfield-from-slots ins '(immediate
				  rs1Id
				  funct3
				  rdId
				  opcode)))


(defmethod argument-initargs ((ins rv32-i))
  (list :rd :rs1 :v))


(defclass rv32-j (rv32)
  ((rdId
    :documentation "The destination link register identification field."
    :type (unsigned-byte 5)
    :initarg :rs1
    :reader destination-register
    :reader link-register)
   (address
    :documentation "The destination address value field."
    :type (unsigned-byte 20)
    :initarg :addr
    :reader immediate-value))
  (:documentation "RISC-V J-type instructions.

J-type instructions encode jumps, linking the return address into
a register."))


(defmethod as-binary ((ins rv32-j))
  (form-bitfield-from-slots ins '(address
				  opcode)))


(defmethod argument-initargs ((ins rv32-j))
  (list :rd :addr))


(defclass add (rv32-r)
  ((opcode
    :allocation :class
    :initform #2r0110011)
   (funct3
    :allocation :class
    :initform #2r000)
   (funct7
    :allocation :class
    :initform #2r0000000))
  (:documentation "Add two registers."))


(defclass addi (rv32-i)
  ((opcode
    :allocation :class
    :initform #2r0010011)
   (funct3
    :allocation :class
    :initform #2r000))
  (:documentation "Add a register to an immediate value."))


;; ---------- Machine code segments ----------

(defclass section ()
  ((memory
    :documentation "The memory bytes."
    :type (array (unsigned-byte 8) 1)
    :initform (make-array 256
			  :element-type (unsigned-byte 8)
			  :initial-element 0
			  :fill-pointer 0
			  :adjustable t)
    :reader memory))
  (:documentation "A section of binary data.

A section is initially 256 bytes long, and grows in crements
of 256 bytes as it is filled."))


(defun compile-byte (b s)
  "Compile byte B into section S."
  (vector-push-extend (logand #16rff b) (memory s) 256))


(defun compile-half-word (hw s)
  "Compile half-word HW into section S.

The half-word is compiled little-endian."
  (compile-byte hw s)
  (compile-byte (ash hw -8) s))


(defun compile-word (hw s)
  "Compile word W into section S.

The word is compiled little-endian."
  (compile-helf-word w s)
  (compile-half-word (ash w -16) s))


;; ---------- Evaluating in restricted environments ----------

(defun assembler-constant-p (n)
  "Test whether N is an assembler constant.

Assembler constants are named constants or labels
for memory addresses within assembler programs."
  (and (variable-declared-p n)
       (member (get-representation n) '(:constant))))


(defun close-form-in-assembler-environment (form)
  "Close th Lisp FORM in the assembler part of the current enviroment.

ENV is filtered to include only constants."
  (let ((assenv (filter-environment (lambda (n env)
				      (assembler-constant-p n))
				    *global-environment*)))
    (close-form-in-environment form assenv)))


(defun eval-in-assembler-environment (form)
  "Evaluate Lisp FORM in the assembler environment."
  (let ((cf (close-form-in-assembler-environment form)))
    (eval cf)))


(defvar *registers* '(
		      ;; general-purpose
		      (x0   0) (x1   1) (x2   2) (x3   3)
		      (x4   4) (x5   5) (x6   6) (x7   7)
		      (x8   8) (x9   9) (x10 10) (x11 11)
		      (x12 12) (x13 13) (x14 14) (x15 15)
		      (x16 16) (x17 17) (x18 18) (x19 19)
		      (x20 20) (x21 21) (x22 22) (x23 23)
		      (x24 24) (x25 25) (x26 26) (x27 27)
		      (x28 28) (x29 29) (x30 30) (x31 31)

		      ;; ABI
		      ;; operational
		      (zero 0)  ;; zero
		      (ra   1)  ;; return address
		      (sp   2)  ;; stack pointer
		      (gp   3)  ;; general pointer
		      (tp   4)  ;; thread pointer
		      (fp   8)  ;; frame pointer (also s0)

		      ;; function arguments
		      (a0  10) (a1  11) (a2  12) (a3  13)
		      (a4  14) (a5  15) (a6  16) (a7  17)

		      ;; saved registers
		      (s0   8) (s1   9) (s2  18) (s3  19)
		      (s4  20) (s5  21) (s6  22) (s7  23)
		      (s8  24) (s9  25) (s10 26) (s11 27)

		      ;; temporary registers
		      (t0   5) (t1   6) (t2   7) (t3  28)
		      (t4  29) (t5  30) (t6  31))
  "Alist mapping register names to bit patterns.

The list includes both the general-purpose and ABI register names.")


(defun register (form env)
  "Convert FORM to a register bit pattern.

An UNRECOGNISED-REGISTER is signalled if FORM doesn't name
a recognised register."
  (if-let ((sym (symbolp form))
	   (m (member form *registers*)))
    (cadr m)

    (error 'unrecognised-register :register form)))


(defun literal (form env)
  "Convert FORM to a literal.

FORM may be an integer literal, a symbol defined
in ENV, or a Lisp expression using only the symbols
in ENV."
  (if-let ((v (eval-in-assembler-environment form env)))
    v

    )
  )


(defun mapcar-plist (f pl)
  "Map F across the values of plist P.

The keys are left unchanged; the values are processed. Return the
plist consisting of the keys linked to the updated values."
  (labels ((process (pl)
	     (if (null pl)
		 nil
		 (destructuring-bind (k v &rest pl2)
		     pl
		   `(,k ,(funcall f v) ,@(process pl2))))))

    (process pl)))


(defun assemble-instruction (ins)
  "Assemble INS."
  (destructuring-bind (cn &rest args)
      ins
    (let* ((vs (mapcar-plist #'eval-in-assembler-environment
			     args))
	   (mc (apply #'make-instance (cons cn vs))))

      (let ((bs (as-binary mc)))
	nil
)

      )))


(defvar *test* '((.equ :n step :v 1)
		 (add :rd x1 :rs1 x0 :rs2 x0)
		 (.label :label L0)
		 (addi :rd x1 :rs1 x1 :v step)
		 (jal :rd x0 :addr L0)
		 (ebreak)))


(with-new-frame
  (dolist (reg *registers*)
    (declare-variable (car reg) `((:as :constant)
				  (:initial-value ,(cadr reg)))))
  (assemble-instruction (cadr *test*))

  )
