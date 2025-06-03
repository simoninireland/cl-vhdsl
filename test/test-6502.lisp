(assemble (:origin #16r1000)
  (.DEFINE SOURCE #16r200)
  (.DEFINE DEST #16r300)
  (.DEFINE SIZE 25)
  (LDX :mode (immediate :value (.FROM SIZE)))
  (.LABEL COPY)
  (LDA :mode (absolute-indexed :address (.FROM) SOURCE
			       :index X))
  (STA :mode (absolute-indexed :address (.FROM DEST)
			       :index X))
  (DEX)
  (BNZ :mode (relative :offset (.FROM COPY))))


(in-package :verilisp/systems/6502)

(assemble (:origin #16r1000)
  (lda :addressing-mode (immediate :value 12))
  )

(list
 (make-instance 'LDA :addressing-mode (immediate :vlue 100))
 )
