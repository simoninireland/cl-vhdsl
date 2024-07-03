(defprogram
  (.equ :label "SOURCE" :value #16r200)
  (.equ :label "DEST" :value #16r300)
  (.org :address #16r100)
  (.label :label "START")
  (LDX :mode (immediate :value 25))
  (.label :label "COPY")
  (LDA :mode (absolute-indexed :address-label "SOURCE" :index X))
  (SDA :mode (absolute-indexed :address-label "DEST" :index X))
  (DEX)
  (BNZ :label "COPY")
  (.end))


(list
 (LDX :mode (immediate :value 25))
 (LDA :mode (absolute-indexed
	     :address #16r200
	     :index X))
 (SDA :mode (absolute-indexed
	     :address #16r200
	     :index X))
 (DEX)
 (BNZ :mode (relative :offset -9))

 )
