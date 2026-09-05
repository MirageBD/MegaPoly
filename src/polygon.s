; .segment "POLYGON"

.define vxptr			$c0
.define vyptr			$c2
.define vzptr			$c4
.define vxcptr			$c6
.define vycptr			$c8

; ----------------------------------------------------------------------------------------------------

drawpolygons:

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
rploop		sta vertindex

			ldz vertindex
			ldq (vxptr),z
			stq sx
			ldz vertindex
			ldq (vyptr),z
			stq sy
			ldz vertindex
			ldq (vzptr),z
			stq sz

			;MATH_DOT3 sx, m11+0*4, sy, m11+1*4, sz, m11+2*4, fx
			;MATH_DOT3 sx, m11+3*4, sy, m11+4*4, sz, m11+5*4, fy
			;MATH_DOT3 sx, m11+6*4, sy, m11+7*4, sz, m11+8*4, fz
			MATH_MUL_VEC3_MAT3x3 sx, sy, sz, m11, fx, fy, fz
			
			ldq qdistance                                   ; take distance, sub z
			sec
			sbcq fz

			stq MULTINB
			MATH_MOV q80, MULTINA							; multiply by factor
			MATH_MOV DIVOUTWHOLE+2, fz						; add 2 to get new 16.16 fixed point result

			MATH_MUL fx, fz, fx
			MATH_MUL fy, fz, fy

			MATH_ADD fx, q100, fx
			MATH_ADD fy, q100, fy

			ldy vertindex
			lda fx+2
			sta vertsxconv+2,y
			lda fy+2
			sta vertsyconv+2,y

			clc
			lda vertindex
			adc #$04
			cmp #numverts*4
			beq :+
			jmp rploop
:

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

			lda #$00
dploop		sta polyindex

			ldx polyindex									; get 3 transformed vertices, left/mid/right X/Y
			ldz indicesp1,x
			ldq (vxcptr),z
			stq leftX
			ldx polyindex
			ldz indicesp1,x
			ldq (vycptr),z
			stq leftY

			ldx polyindex
			ldz indicesp2,x
			ldq (vxcptr),z
			stq midX
			ldx polyindex
			ldz indicesp2,x
			ldq (vycptr),z
			stq midY

			ldx polyindex
			ldz indicesp3,x
			ldq (vxcptr),z
			stq rightX
			ldx polyindex
			ldz indicesp3,x
			ldq (vycptr),z
			stq rightY

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
			stq sx
			ldz #$00
			ldq (vyptr),z
			stq sy
			ldz #$00
			ldq (vzptr),z
			stq sz

			;MATH_DOT3 sx, m11+0*4, sy, m11+1*4, sz, m11+2*4, fx
			;MATH_DOT3 sx, m11+3*4, sy, m11+4*4, sz, m11+5*4, fy
			;MATH_DOT3 sx, m11+6*4, sy, m11+7*4, sz, m11+8*4, fz
			MATH_MUL_VEC3_MAT3x3 sx, sy, sz, m11, fx, fy, fz

			MATH_DOT3 fx, lightvec+0, fy, lightvec+4, fz, lightvec+8, fx

			MATH_MUL_APOS_BPOS fx, qlightmult, fx
			MATH_ADD fx, qlightadd, fx

			ldx polyindex
			ldy orgcol,x
			lda colorremap,y
			clc
			adc fx+2
			sta linecolour

			jsr rasterizepoly
			;lda #0
			;sta $d020

skippolydraw

			lda polyindex
			inc
			cmp #numpolies
			beq :+
			jmp dploop

:           rts

; ----------------------------------------------------------------------------------------------------