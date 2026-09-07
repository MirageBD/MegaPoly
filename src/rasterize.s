.segment "RASTERIZE"

.define leftSlopeX			$9c								; overwrites rotation matrix in ZP/BP, but we're done with that anyway
.define rightSlopeX			$a0
.define totalSlopeX			$a4

leftX				.byte $00, $00, $00, $00
leftY				.byte $00, $00, $00, $00

midX				.byte $00, $00, $00, $00
midY				.byte $00, $00, $00, $00
midY2				.byte $00, $00, $00, $00

rightX				.byte $00, $00, $00, $00
rightY				.byte $00, $00, $00, $00

leftSpanX			.byte $00, $00, $00, $00
rightSpanX			.byte $00, $00, $00, $00
totalSpanX			.byte $00, $00, $00, $00

leftSpanY			.byte $00, $00, $00, $00
rightSpanY			.byte $00, $00, $00, $00
totalSpanY			.byte $00, $00, $00, $00

middleLengthY		.byte $00, $00, $00, $00

.macro SWAP this, that
		ldy this
		ldx that
		stx this
		sty that
.endmacro		

.macro GENERATE_SLOPE_TABLE_NONCLIPPED startx, starty, spanx, slope
.scope
					lda spanx+2
					beq span_skip									; span 0 -> skip rendering

					sta dma_slpcount+0
					lda starty+2									; Y start
					sta dma_slpsadr+0
					lda startx+2									; X start
					sta dma_slpdadr+0

					bit slope+3										; if Y span negative, then set DMA to render in reverse direction (and negate delta to start in reverse order)
					bmi slope_negative

slope_positive:		lda #%00000000									; positive DMA copy
					sta dma_slpdir
					lda slope+1										; Y/X delta low
					sta dma_slpsskiplo+1
					lda slope+2										; Y/X delta high
					sta dma_slpsskiphi+1
					bra span_finalise

slope_negative:		lda #%00010000									; negative DMA copy
					sta dma_slpdir
					lda slope+1										; negative Y/X delta low
					eor #$ff
					sta dma_slpsskiplo+1
					lda slope+2										; negative Y/X delta low
					eor #$ff
					sta dma_slpsskiphi+1
					;jmp span_finalise

span_finalise:		jsr dma_plot_slope
span_skip:
.endscope
.endmacro

; ----------------------------------------------------------------------------------------------------

dma_plot_slope:

					sta $d707								; inline DMA
dma_slpsskiplo:		.byte $82, 0							; Source skip rate (256ths of bytes)
dma_slpsskiphi:		.byte $83, 0							; Source skip rate (whole bytes)
					.byte $85, 1							; Destination skip rate (whole bytes)
					.byte $00								; end of job options

dma_slpdir:			.byte %00000000							; positive DMA copy. copy (bit 5 = invert source, bit 6 = invert destination)
dma_slpcount:		.word $0000								; count - needs initialising
dma_slpsadr:		.word lineartable						; src
					.byte $00								; src bank and flags
dma_slpdadr:		.word slopetop							; dst
					.byte $00								; dst bank and flags
					.byte $00								; cmd hi
					.word $0000								; modulo, ignored
					rts

; ----------------------------------------------------------------------------------------------------

dma_plot_heights:

					sta $d707								; inline DMA
dma_hgtskiplo:		.byte $82, 0							; Source skip rate (256ths of bytes)
dma_hgtskiphi:		.byte $83, 0							; Source skip rate (whole bytes)
					.byte $85, 1							; Destination skip rate (whole bytes)
					.byte $00								; end of job options

dma_hgtdir:			.byte %00000000							; positive DMA copy. copy (bit 5 = invert source, bit 6 = invert destination)
dma_hgtcount:		.word $0000								; count - needs initialising
					.word lineartable+1						; src
					.byte $00								; src bank and flags
dma_hgtdadr:		.word slopeheights						; dst
					.byte $00								; dst bank and flags
					.byte $00								; cmd hi
					.word $0000								; modulo, ignored
					rts

; ----------------------------------------------------------------------------------------------------

rasterizepoly:

			lda linecolour
			lsr
			clc
			adc #$c0
			sta $d020

			; ----------------------------------------------- swap points if needed, sorting points from left to right

			lda leftX+2
			cmp midX+2
			bmi :+
				ldx midX+2
				stx leftX+2
				sta midX+2
				SWAP leftY+2, midY+2
:			lda leftX+2
			cmp rightX+2
			bmi :+
				ldx rightX+2
				stx leftX+2
				sta rightX+2
				SWAP leftY+2, rightY+2
:			lda midX+2
			cmp rightX+2
			bmi :+
				ldx rightX+2
				stx midX+2
				sta rightX+2			
				SWAP midY+2, rightY+2
:
			; ----------------------------------------------- calculate X spans. these are always positive, so can do simpler Accumulator subtract

			sec
			lda midX+2
			sbc leftX+2
			sta leftSpanX+2
			lda rightX+2
			sbc midX+2
			sta rightSpanX+2
			lda rightX+2
			sbc leftX+2
			sta totalSpanX+2 ; return here if total == 0 ?

			; ----------------------------------------------- calculate slopes

			sec	
			ldq midY
			sbcq leftY
			stq leftSpanY
			MATH_DIV_BPOS leftSpanY,  leftSpanX,  leftSlopeX

			sec
			ldq rightY
			sbcq midY
			stq rightSpanY
			MATH_DIV_BPOS rightSpanY, rightSpanX, rightSlopeX

			sec
			ldq rightY
			sbcq leftY
			stq totalSpanY
			MATH_DIV_BPOS totalSpanY, totalSpanX, totalSlopeX

			; check if we're inverted (I.E. longest slope is running at the top)
			; (leftY + leftspanX * totalSlopeX) is this point (*):
			;
			;   (1) ---___
			;        -    (*)-____
			;         -    |      --- (3)
			;          -   |       -
			;           -  |     -
			;            - |   -
			;             -| -
			;             (2)
			;
			; if this point is smaller than point 2 (midY), then the longest slope is at the top (inverse case)

			MATH_MOV leftSpanX, MULTINA				; calculate leftspanX*totalSlopeX
			MATH_MUL_APOS_DIRECT totalSlopeX
			adcq leftY								; add leftY. Q now contains the Y position of the point marked (*)
			stq midY2

			cpy midY+2
			bpl plg_noninverse
			jmp plg_inverse
plg_noninverse: ; longest slope running at bottom
			GENERATE_SLOPE_TABLE_NONCLIPPED leftX, leftY,  leftSpanX,  leftSlopeX 			; partial span left
			GENERATE_SLOPE_TABLE_NONCLIPPED  midX,  midY, rightSpanX, rightSlopeX			; partial span right

			sec
			lda midY2+2
			sbc midY+2
			inc a
			sta middleLengthY+2

			lda leftSpanX+2
			beq skip_ni_lsp
			sta dma_hgtcount+0
			MATH_DIV_BPOS_DIRECT middleLengthY,  leftSpanX
			stx dma_hgtskiplo+1
			sty dma_hgtskiphi+1
			lda leftX+2
			sta dma_hgtdadr+0
			lda #%00000000
			sta dma_hgtdir
			jsr dma_plot_heights
skip_ni_lsp

			lda rightSpanX+2
			beq skip_ni_rsp
			inc a
			sta dma_hgtcount+0
			MATH_DIV_BPOS_DIRECT middleLengthY, rightSpanX
			stx dma_hgtskiplo+1
			sty dma_hgtskiphi+1
			lda rightX+2
			sta dma_hgtdadr+0
			lda #%00100000
			sta dma_hgtdir
			jsr dma_plot_heights
skip_ni_rsp


			jmp plg_checkend
plg_inverse: ; longest slope running at top

			GENERATE_SLOPE_TABLE_NONCLIPPED leftX, leftY, totalSpanX, totalSlopeX			; total span

			sec
			lda midY+2
			sbc midY2+2
			sta middleLengthY+2

			lda leftSpanX+2
			beq skip_i_lsp
			sta dma_hgtcount+0
			MATH_DIV_BPOS_DIRECT middleLengthY,  leftSpanX
			stx dma_hgtskiplo+1
			sty dma_hgtskiphi+1
			lda leftX+2
			sta dma_hgtdadr+0
			lda #%00000000
			sta dma_hgtdir
			jsr dma_plot_heights
skip_i_lsp

			lda rightSpanX+2
			beq skip_i_rsp
			inc a
			sta dma_hgtcount+0
			MATH_DIV_BPOS_DIRECT middleLengthY, rightSpanX
			stx dma_hgtskiplo+1
			sty dma_hgtskiphi+1
			lda rightX+2
			sta dma_hgtdadr+0
			lda #%00100000
			sta dma_hgtdir
			jsr dma_plot_heights
skip_i_rsp

plg_checkend

		; ----------------------------------------------- set up polygon

polygon_setup:

			lda #0											; get ready to multiply stuff by 8 in inner loop
			tax
			tay
			taz
			stq MULTINA
			stq MULTINB

			lda #8
			sta MULTINA+0

			ldx leftX+2										; set all variabe low bytes

			; ----------------------------------------------- do the actual polygon drawing loop.

polygon_draw_loop:

			cpx rightX+2
			beq polygon_end_draw

			lda slopeheights,x
			sta linesize+0

			lda slopetop,x									; get top again
			sta MULTINB+0									; and multiply by 8 to get to correct row

			lda MULTOUT+0 ; times8lo,x						; then add address for this column
			adc dstcolumnlo,x
			sta linestart+0

			lda MULTOUT+1 ; times8hi,x
			adc dstcolumnhi,x
			sta linestart+1

drawspan:		sta $d707									; inline DMA
				.byte $85, 8								; Destination skip rate (whole bytes)
				.byte $00									; No more options
				.byte %00000011								; fill and last request
linesize:		.word $0004									; count - needs initialising
linecolour:		.word $00b0									; src - this is normally the source addres, but contains the fill value now
				.byte $00									; src bank and flags (ignored)
linestart		.word (screenchars1 & $ffff)				; dst
linebuf			.byte ((screenchars1 >> 16) & $0f)			; dst bank and flags
				.byte $00									; cmd hi
				.word $0000									; modulo, ignored

			inx
			bra polygon_draw_loop

polygon_end_draw:

			lda #$00
			sta $d020

			rts

; ----------------------------------------------------------------------------------------------------

