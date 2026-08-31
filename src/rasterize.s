.macro SWAP this, that
		ldq this
		stq FP_A
		ldq that
		stq this
		ldq FP_A
		stq that
.endmacro		

.macro GENERATE_SLOPE_TABLE_NONCLIPPED startx, starty, spanx, spany, delta, destinationhi, destinationlo
.scope
					lda spanx+3										; test if span == 0
					beq span_couldbe0
					bra span_biggerthan256							; not zero, continue
span_couldbe0:		lda spanx+2
					beq span_skip									; high AND low span both 0 -> skip rendering
span_biggerthan256:	lda spanx+2
					sta dma_slpcount+0
					lda spanx+3
					sta dma_slpcount+1

					lda starty+2									; Y start
					sta dma_slpsadr+0
					;lda starty+1									; Y start fraction
					;sta dma_slpsadrfrac+1
					clc
					lda destinationlo								; put Y numbers at xxyy
					adc startx+2
					sta dma_slpdadr+0
					lda destinationhi								; put Y numbers at xxyy
					;adc startx+3
					sta dma_slpdadr+1

					bit spany+3										; if Y span negative, then set DMA to render in reverse direction (and negate delta to start in reverse order)
					bmi span_negative
;span_positive:
					lda delta+1										; Y/X delta low
					sta dma_slpsskiplo+1
					lda delta+2										; Y/X delta high
					sta dma_slpsskiphi+1
					lda #%00000000									; positive DMA copy
					sta dma_slpdir
					bra span_finalise
span_negative:
					lda delta+1										; negative Y/X delta low
					eor #$ff
					sta dma_slpsskiplo+1
					lda delta+2										; negative Y/X delta low
					eor #$ff
					sta dma_slpsskiphi+1
					lda #%00010000									; negative DMA copy
					sta dma_slpdir
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

drawpoly

			lda linecolour
			lsr
			clc
			adc #$c0
			sta $d020

			; ----------------------------------------------- swap points if needed

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
			; ----------------------------------------------- calculate X spans

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

			; ----------------------------------------------- calculate Y spans

			sec	
			ldq midY
			sbcq leftY
			stq leftSpanY
			sec
			ldq rightY
			sbcq midY
			stq rightSpanY
			sec
			ldq rightY
			sbcq leftY
			stq totalSpanY

			; ----------------------------------------------- calculate deltas

			MATH_DIV leftSpanY,  leftSpanX,  leftSlopeX
			MATH_DIV rightSpanY, rightSpanX, rightSlopeX
			MATH_DIV totalSpanY, totalSpanX, totalSlopeX

			; ----------------------------------------------- DMA plot slopes

			MATH_DIV totalSpanX, totalSpanY, totalSlopeY
			GENERATE_SLOPE_TABLE_NONCLIPPED leftX, leftY,  leftSpanX,  leftSpanY,  leftSlopeX, #>slopetop,    #0					; partial span left
			GENERATE_SLOPE_TABLE_NONCLIPPED leftX,  midY, rightSpanX, rightSpanY, rightSlopeX, #>slopetop,    leftSpanX+2		; partial span right
			GENERATE_SLOPE_TABLE_NONCLIPPED leftX, leftY, totalSpanX, totalSpanY, totalSlopeX, #>slopebottom, #0					; total span

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

			lda leftX+2										; set all variabe low bytes
			sta pdlbot+1
			sta pdltop1+1
			sta pdltop2+1
			sta pdlcollo+1
			sta pdl9+1

			;lda leftX+3
			;sta columnhi

			clc
			lda #>dstcolumnlo							; set variable high bytes
			adc leftX+3
			sta pdlcollo+2
			clc
			lda #>dstcolumnhi
			adc leftX+3
			sta pdl9+2

			clc
			lda pdlbot+2
			adc leftX+3
			sta pdlbot+2

			clc
			lda pdltop1+2
			adc leftX+3
			sta pdltop1+2
			sta pdltop2+2

			; ----------------------------------------------- do the actual polygon drawing loop.

			ldq q0												; get ready to multiply stuff by 8 in inner loop
			stq MULTINA
			lda #8
			sta MULTINA+0
			ldq q0
			stq MULTINB

polygon_draw_loop:

			lda pdlbot+1

polygon_draw_loop2:

			cmp rightX+2
			bne polygon_continue_draw
			;lda columnhi
			;cmp rightX+3
			;bne polygon_continue_draw

			lda #$00
			sta $d020

			rts

polygon_continue_draw:

			sec
pdlbot:		lda $ff00											; get bottom y
pdltop1		sbc $ff00											; subtract top y to get span size
			bcs pdlpos											; continue if positive
			bra pdl3											; otherwise skip span
pdlpos:		bne pdl2											; continue if not 0
			bra pdl3											; otherwise skip span
pdl2:		sta linesize+0

pdltop2:	ldy $ff00
			sty MULTINB+0
			clc
			lda MULTOUT+0 ; times8lo,y
pdlcollo:	adc dstcolumnlo
			sta linestart+0
			lda MULTOUT+1; times8hi,y
pdl9:		adc dstcolumnhi
			sta linestart+1

drawspan:
				sta $d707									; inline DMA
				;.byte $80, 0x00							; sourceMB
				;.byte $81, (screenchars1 >> 20)			; destMB
				;.byte $84, 0								; Destination skip rate (256ths of bytes)
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

pdl3:
			inc pdlbot+1
			lda pdlbot+1
			sta pdltop1+1
			sta pdltop2+1
			sta pdlcollo+1
			sta pdl9+1
			bne polygon_draw_loop2	; if we've crossed the 256 (when the screen is 320 wide, which it's not) boundary then increase columnhi and stuff
;			inc pdlbot+2
;			inc pdltop1+2
;			inc pdltop2+2
;			inc pdlcollo+2
;			inc pdl9+2
;			inc columnhi
;pdl7:		bra polygon_draw_loop

; ----------------------------------------------------------------------------------------------------
