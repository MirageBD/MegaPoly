; .segment "POLYGON"

.define vxptr			$c0
.define vyptr			$c2
.define vzptr			$c4
.define vxcptr			$c6
.define vycptr			$c8

; ----------------------------------------------------------------------------------------------------

transformvertices:

			; ---------------------------- INIT TRANSFORM VERTICES

			lda #<vertsx
			sta vxptr+0
			lda #>vertsx
			sta vxptr+1

			lda #<vertsy
			sta vyptr+0
			lda #>vertsy
			sta vyptr+1

			lda #<vertsz
			sta vzptr+0
			lda #>vertsz
			sta vzptr+1

			; ---------------------------- TRANSFORM VERTICES

			lda #$00
			sta vertindex
rploop
			ldz #0
			ldq (vxptr),z
			stq fx
			ldz #0
			ldq (vyptr),z
			stq fy
			ldz #0
			ldq (vzptr),z
			stq fz

			MATH_MUL_VEC3_MAT3x3 fx, fy, fz, m11, sx, sy, sz
			
			;sec
			ldq qdistance                                   ; take distance, sub z
			sbcq sz
			stq MULTINB

			MATH_MOV q80, MULTINA							; multiply by factor
			MATH_MOV DIVOUTWHOLE+2, MULTINA					; add 2 to get new 16.16 fixed point result

			MATH_MUL_APOS_DIRECT sx							; perspective divide
			adcq q100										; and move to center of screen
			ldx vertindex
			sty vertsxconv,x
			MATH_MUL_APOS_DIRECT sy
			adcq q100
			ldx vertindex
			sty vertsyconv,x

			clc
			lda vxptr+0
			adc #4
			sta vxptr+0
			sta vyptr+0
			sta vzptr+0
			bne :+
			inc vxptr+1
			inc vyptr+1
			inc vzptr+1

:			inc vertindex
			lda vertindex
			cmp #numverts
			beq :+
			jmp rploop
:
			rts

; ----------------------------------------------------------------------------------------------------

drawpolygons:

			; ---------------------------- INIT DRAW POLYGONS

			lda #<vertsxconv								; set pointers to transformed vertices
			sta vxcptr+0
			lda #>vertsxconv
			sta vxcptr+1

			lda #<vertsyconv
			sta vycptr+0
			lda #>vertsyconv
			sta vycptr+1

			; ---------------------------- DRAW POLYGONS

			ldx #$00
dploop		stx polyindex

			ldz indicesp1,x
			lda (vxcptr),z
			sta leftX+2
			lda (vycptr),z
			sta leftY+2

			ldz indicesp2,x
			lda (vxcptr),z
			sta midX+2
			lda (vycptr),z
			sta midY+2

			ldz indicesp3,x
			lda (vxcptr),z
			sta rightX+2
			lda (vycptr),z
			sta rightY+2

			ldq midX										; calculate winding order
			sbcq leftX
			stq MULTINA
			ldq rightY
			sbcq midY
			stq MULTINB

			MATH_MOV MULTOUT+2, t1

			ldq midY
			sbcq leftY
			stq MULTINA
			ldq rightX
			sbcq midX
			stq MULTINB

			ldq MULTOUT+2
			sbcq t1
			stq t1

			bit t1+3										; backface cull
			bmi not_backface_culled
			jmp skippolydraw

not_backface_culled:

			; ROTATE/LIGHT NORMALS/POLYS

			ldx polyindex									; get poly index
			lda times4lo,x									; and multiply by 4
			sta vxptr+0
			sta vyptr+0
			sta vzptr+0
			lda times4hi,x
			sta pihi

			clc												; add normals addresses. needs to be page aligned
			lda #>normalsx
			adc pihi
			sta vxptr+1

			;clc
			lda #>normalsy
			adc pihi
			sta vyptr+1

			;clc
			lda #>normalsz
			adc pihi
			sta vzptr+1

			ldz #$00
			ldq (vxptr),z
			stq fx
			ldz #$00
			ldq (vyptr),z
			stq fy
			ldz #$00
			ldq (vzptr),z
			stq fz

			MATH_MUL_VEC3_MAT3x3 fx, fy, fz, m11, sx, sy, sz

			ldq sz
			stq MULTINA
			MATH_MUL_APOS lightvec+8, t3
			ldq sy
			stq MULTINA
			MATH_MUL_APOS lightvec+4, t2
			ldq sx
			stq MULTINA
			MATH_MUL_APOS lightvec+0, t1
			clc
			adcq t2
			adcq t3
			stq MULTINA

			MATH_MOV qlightmult, MULTINB
			ldq MULTOUT+2
			adcq qlightadd

			tya

			ldx polyindex
			ldy orgcol,x
			clc
			adc colorremap,y
			sta linecolour

			jsr rasterizepoly
			;lda #0
			;sta $d020

skippolydraw

			ldx polyindex
			inx
			cpx #numpolies
			beq :+
			jmp dploop

:           rts

; ----------------------------------------------------------------------------------------------------