.segment "RASTERIZE"

.define leftSlopeX		$9c								; overwrites rotation matrix in ZP/BP, but we're done with that anyway
.define rightSlopeX		$a0
.define totalSlopeX		$a4

leftX			.byte $00, $00, $00, $00
leftY			.byte $00, $00, $00, $00

midX			.byte $00, $00, $00, $00
midY			.byte $00, $00, $00, $00

rightX			.byte $00, $00, $00, $00
rightY			.byte $00, $00, $00, $00

leftSpanX		.byte $00, $00, $00, $00
rightSpanX		.byte $00, $00, $00, $00
totalSpanX		.byte $00, $00, $00, $00

leftSpanY		.byte $00, $00, $00, $00
rightSpanY		.byte $00, $00, $00, $00
totalSpanY		.byte $00, $00, $00, $00

.macro SWAP this, that
		ldq this
		stq FP_A
		ldq that
		stq this
		ldq FP_A
		stq that
.endmacro		

.macro GENERATE_SLOPE_TABLE_NONCLIPPED starty, spanx, spany, delta, destinationlo
.scope
					lda spanx+2
					beq span_skip									; span 0 -> skip rendering

					sta dma_slpcount+0
					lda starty+2									; Y start
					sta dma_slpsadr+0
					clc
					lda destinationlo								; put Y numbers at xxyy
					adc leftX+2
					sta dma_slpdadr+0

					bit spany+3										; if Y span negative, then set DMA to render in reverse direction (and negate delta to start in reverse order)
					bmi span_negative

span_positive:		lda #%00000000									; positive DMA copy
					sta dma_slpdir
					lda delta+1										; Y/X delta low
					sta dma_slpsskiplo+1
					lda delta+2										; Y/X delta high
					sta dma_slpsskiphi+1
					bra span_finalise

span_negative:		lda #%00010000									; negative DMA copy
					sta dma_slpdir
					lda delta+1										; negative Y/X delta low
					eor #$ff
					sta dma_slpsskiplo+1
					lda delta+2										; negative Y/X delta low
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
					.byte $06								; Disable use of transparent value
						;.byte $80, $00							; sourceMB
						;.byte $81, $00							; destMB - ignored when drawing lines
dma_slpsskiplo:		.byte $82, 0							; Source skip rate (256ths of bytes)
dma_slpsskiphi:		.byte $83, 0							; Source skip rate (whole bytes)
						;.byte $84, 0							; Destination skip rate (256ths of bytes)
					.byte $85, 1							; Destination skip rate (whole bytes)
						;.byte $8f, %00000000					; bit 7 = enable DESTINATION line drawing, Bit 6 = select X or Y direction, Bit 5 = slope is negative.
;dma_slpsadrfrac:	.byte $91, 0							; linear source initial fractional part.
						;.byte $92, 0							; linear destination initial fractional part.
						;.byte $9f, %00000000					; bit 7 = enable SOURCE line drawing, Bit 6 = select X or Y direction, Bit 5 = slope is negative.
					.byte $00								; end of job options

dma_slpdir:			.byte $00 | %00000000					; copy (bit 5 = invert source, bit 6 = invert destination)
dma_slpcount:		.word $0000								; count - needs initialising
dma_slpsadr:		.word lineartable						; src
					.byte $00								; src bank and flags
dma_slpdadr:		.word slopetop							; dst
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
			SWAP leftX, midX
			SWAP leftY, midY
:			lda leftX+2
			cmp rightX+2
			bmi :+
			SWAP leftX, rightX
			SWAP leftY, rightY
:			lda midX+2
			cmp rightX+2
			bmi :+
			SWAP midX, rightX
			SWAP midY, rightY
:
			; ----------------------------------------------- calculate X spans. these are always positive, so can do simpler Accumulator subtract

			sec
			lda midX+2
			sbc leftX+2
			sta leftSpanX+2
			sec
			lda rightX+2
			sbc midX+2
			sta rightSpanX+2
			sec
			lda rightX+2
			sbc leftX+2
			sta totalSpanX+2 ; return here if total == 0 ?

			; ----------------------------------------------- calculate Y spans and slopes

			lda #>slopetop
			sta dma_slpdadr+1
			sec	
			ldq midY
			sbcq leftY
			stq leftSpanY
			MATH_DIV_BPOS leftSpanY,  leftSpanX,  leftSlopeX
			GENERATE_SLOPE_TABLE_NONCLIPPED leftY,  leftSpanX,  leftSpanY,  leftSlopeX, #0				; partial span left

			sec
			ldq rightY
			sbcq midY
			stq rightSpanY
			MATH_DIV_BPOS rightSpanY, rightSpanX, rightSlopeX
			GENERATE_SLOPE_TABLE_NONCLIPPED  midY, rightSpanX, rightSpanY, rightSlopeX, leftSpanX+2		; partial span right

			lda #>slopebottom
			sta dma_slpdadr+1
			sec
			ldq rightY
			sbcq leftY
			stq totalSpanY
			MATH_DIV_BPOS totalSpanY, totalSpanX, totalSlopeX
			GENERATE_SLOPE_TABLE_NONCLIPPED leftY, totalSpanX, totalSpanY, totalSlopeX, #0				; total span

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

			MATH_MUL leftSpanX, totalSlopeX, FP_A	; optimise this later. no need to store in temp Q reg
			;ldq FP_A ; Q should already contain correct value
			clc
			adcq leftY
			cmpq midY
			bmi plg_inverse
plg_noninverse:
			lda #>slopebottom
			sta pdlbot+2
			lda #>slopetop
			sta pdltop1+2
			sta pdltop2+2
			bra plg_checkend
plg_inverse:		
			lda #>slopetop
			sta pdlbot+2
			lda #>slopebottom
			sta pdltop1+2
			sta pdltop2+2		
plg_checkend

		; ----------------------------------------------- set up polygon

polygon_setup:

			ldq q0											; get ready to multiply stuff by 8 in inner loop
			stq MULTINA
			stq MULTINB
			lda #8
			sta MULTINA+0

			; ----------------------------------------------- do the actual polygon drawing loop.

polygon_draw_loop:

			ldy leftX+2										; set all variabe low bytes

polygon_draw_loop2:

			cpy rightX+2
			bne polygon_continue_draw

			lda #$00
			sta $d020

			rts

polygon_continue_draw:

			sec
pdlbot:		lda $ff00,y										; get bottom y
pdltop1:	sbc $ff00,y										; subtract top y to get span size
			bcs pdlpos										; continue if positive
			bra pdl3										; otherwise skip span
pdlpos:		beq pdl3										; continue if not 0
pdl2:		sta linesize+0

			clc
pdltop2:	lda $ff00,y										; get top again
			sta MULTINB+0									; and multiply by 8 to get to correct column
			lda MULTOUT+0 ; times8lo,y
			adc dstcolumnlo,y
			sta linestart+0
			lda MULTOUT+1 ; times8hi,y
			adc dstcolumnhi,y
			sta linestart+1

drawspan:		sta $d707									; inline DMA
				.byte $85, 8								; Destination skip rate (whole bytes)
				.byte $00									; No more options
				.byte %00000011								; fill and last request
linesize:		.word $0000									; count - needs initialising
linecolour:		.word $00b0									; src - this is normally the source addres, but contains the fill value now
				.byte $00									; src bank and flags (ignored)
linestart		.word (screenchars1 & $ffff)				; dst
linebuf			.byte ((screenchars1 >> 16) & $0f)			; dst bank and flags
				.byte $00									; cmd hi
				.word $0000									; modulo, ignored

pdl3:		iny												; increase everything to get to next pixel/column
			bra polygon_draw_loop2	; if we've crossed the 256 (when the screen is 320 wide, which it's not) boundary then increase columnhi and stuff

; ----------------------------------------------------------------------------------------------------

