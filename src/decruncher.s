; -----------------------------------------------------------------------------------------------

dc_base		= $02				; -
dc_bits		= dc_base			; 1
dc_get_zp	= dc_base+2			; 4

; original data

; FF FF FF FF FF FF FF FF FF FF 08 FE FE 08 FF FF
; FF 08 FE 08 08 FE 08 FF FF FE 08 08 FE FF FE FF
; FF FE 08 FF 08 FE FE FF FF 08 FE 08 FF FF FF FF
; FF FF 08 FE FE FE 08 FF FF FF FF FF FF FF FF FF
; FF FF FF FF FF FF FF FF FF FF FF 08 FE 08 FF FF
; FF FF FF FE 08 FE FF FF FF FF 08 FE FF FE 08 FF
; FF FF FE 08 FF 08 FE FF FF 08 FE FE FE FE FE 08
; FF FE 08 FF FF FF 08 FE FF FF FF FF FF FF FF FF
; FF FF FF FF FF FF FF FF FF FE FE FE FE FE 08 FF

; compressed data

; 0001372A

; 00 00 01 00 2A FF 1E A6 08 FE FE 08 26 16 1B 03
; 89 87 C4 73 9B 47 46 A3 63 E1 7D 8F F9 BD 04 FE
; B0 3C C4 F1 7F B3 27 98 FE 26 FE 65 FF FF


; 2A    0 0 1 0 1 0 1 0
; FF    1 1 1 1 1 1 1 1
; 1E    0 0 0 1 1 1 1 0
; A6    1 0 1 0 0 1 1 0
; 08    0 0 0 0 1 0 0 0
; FE    1 1 1 1 1 1 1 0
; FE    1 1 1 1 1 1 1 0
; 08    0 0 0 0 1 0 0 0
; 26    0 0 1 0 0 1 1 0
; 16    0 0 0 1 0 1 1 0
; 1B    0 0 0 1 1 0 1 1
; 03    0 0 0 0 0 0 1 1
; 89    1 0 0 0 1 0 0 1
; 87    1 0 0 0 0 1 1 1
; C4    1 1 0 0 0 1 0 0
; 73    0 1 1 1 0 0 1 1
; 9B    1 0 0 1 1 0 1 1
; 47    0 1 0 0 0 1 1 1

; ----------------------------------------------------

; 0 = literal run
; 1 = match

; 2A FF 1E A6 08 FE FE 08 26 16 1B 03 89 87 C4 73
; 9B 47 46 A3 63 E1 7D 8F F9 BD 04 FE B0 3C C4 F1

; $0000: Lit(1, F)
; $0001: Mat(1, 9, F)
; $000a: Lit(4, F)
; $000e: Mat(7, 4, F)
; $0012: Mat(6, 2, T)

; ----------------------------------------------------

; 80       10000000
; asl  1 < 00000000

; bits empty, get new bits

; 2a       00101010
; rol  0 < 01010101 < 1

; GETLEN (because c = 0)

;      A = 1
; asl  0 < 10101010
; carry clear, end

; GETLEN (because next sequence should be a match)

;              A = 1
; asl bits     10101000
;              01010100 c = 1          carry is one, continue adding to A
; asl bits     01010100
;              10101000 c = 0
; rol          A = 2                   7th bit 1, so continue
; asl bits     10101000
;              01010000 c = 1          carry is one, continue adding to A
; asl bits     01010000
;              10100000 c = 0
; rol          A = 4                   7th bit 1, so continue
; asl bits     10100000
;              01000000 c = 1          carry is one, continue adding to A
; asl bits     01000000
;              10000000 c = 0
; rol          A = 8                   7th bit 1, so continue
; asl bits     10000000
;              00000000 c = 0
;                                    dc_bits = 0 -> fetch next bits
; 
;              A = 8
;              00011110
; rol bits 0 < 00111100

; -----------------------------------------------------------------------------------------------

addput	clc
		tya
		adc dc_ldst+0
		sta dc_ldst+0
		bcc :+
		lda dc_ldst+1
		adc #$00
		sta dc_ldst+1
		bcc :+
		lda dc_ldst+2
		adc #$00
		sta dc_ldst+2

:		clc
		tya
		adc dc_mdst+0
		sta dc_mdst+0
		bcc :+
		lda dc_mdst+1
		adc #$00
		sta dc_mdst+1
		bcc :+
		lda dc_mdst+2
		adc #$00
		sta dc_mdst+2
:		rts

addget 	clc
		tya
		adc dc_get_zp+0
		sta dc_get_zp+0
		bcc :+
		lda dc_get_zp+1
		adc #$00
		sta dc_get_zp+1
		bcc :+
		lda dc_get_zp+2
		adc #$00
		sta dc_get_zp+2
:		rts

getlen	lda #1
glloop	jsr getnextbit
		bcc glend
		jsr rolnextbit									; if next bit is 1 then ROL the next-next bit into A
		bpl glloop										; if the highest bit is now still 0, continue. this means highest len is 255
glend	rts

rolnextbit
		jsr getnextbit
		rol												; rol sets N flag
		rts

getnextbit
		asl dc_bits
		bne dgend
		pha
		jsr getnextbyte
		rol
		sta dc_bits
		pla
dgend	rts

getnextbyte
		lda [dc_get_zp],z
		inc dc_get_zp+0
		bne :+
		inc dc_get_zp+1
		bne :+
		inc dc_get_zp+2
:		rts

; -----------------------------------------------------------------------------------------------

offsets	.byte %11011111 ; 3								; short offsets
		.byte %11111011 ; 6
		.byte %00000000 ; 8
		.byte %10000000 ; 10
		.byte %11101111 ; 4								; long offsets
		.byte %11111101 ; 7
		.byte %10000000 ; 10
		.byte %11110000 ; 13

; -----------------------------------------------------------------------------------------------

decrunch_readstartaddress
		ldz #$00
		jsr getnextbyte									; set unpack address
		sta dc_ldst+0
		sta dc_mdst+0
		jsr getnextbyte
		sta dc_ldst+1
		sta dc_mdst+1
		jsr getnextbyte
		sta dc_ldst+2
		sta dc_mdst+2
		jsr getnextbyte									; set attic byte (megabyte). normally a >>20 shift, so shift left 4 bytes to get to 3*8
		asl
		asl
		asl
		asl
		sta dc_lsrcm+1
		sta dc_msrcm+1
		sta dc_ldstm+1
		sta dc_mdstm+1
		rts

decrunch_dowork
		clc

		lda #$80
		sta dc_bits

dloop	jsr getnextbit									; after this, carry is 0, bits = 01010101
		bcs match

		jsr getlen										; Literal run.. get length. after this, carry = 0, bits = 10101010, A = 1
		sta dc_llen
		tay												; put length into y for addput

		lda dc_get_zp+0
		sta dc_lsrc+0
		lda dc_get_zp+1
		sta dc_lsrc+1
		lda dc_get_zp+2
		sta dc_lsrc+2

		sta $d707										; inline DMA copy
dc_lsrcm	.byte $80, ($00000000 >> 20)					; sourcebank
dc_ldstm	.byte $81, ($08000000 >> 20)					; destbank
		.byte $00										; end of job options
		.byte $00										; copy
dc_llen	.word $0000										; count
dc_lsrc	.word $0000										; src
		.byte $00										; src bank
dc_ldst	.word $0000										; dst
		.byte $00										; dst bank
		.byte $00										; cmd hi
		.word $0000										; modulo, ignored

		jsr addget
		jsr addput

		iny	
		beq dloop
														; has to continue with a match so fall through
match
		jsr getlen										; match.. get length.

		tax												; length 255 -> EOF
		inx
		beq dc_end

		stx dc_mlen

		lda #0											; Get num bits
		cpx #3
		rol
		jsr rolnextbit
		jsr rolnextbit
		tax
		lda offsets,x
		beq m8

:		jsr rolnextbit									; Get bits < 8
		bcs :-
		bmi mshort

m8		eor #$ff										; Get byte
		tay
		jsr getnextbyte
		jmp mdone

		;.byte $ae ; = jmp mdone (LDX $FFA0)

mshort	ldy #$ff

mdone	;clc

								; HRMPF! HAVE TO DO THIS NASTY SHIT TO WORK AROUND DMA BUG :(((((
		ldx #$00
		cmp #$ff				; compare A with ff
		bne :+ 
		cpy #$ff				; compare Y with ff
		bne :+
		ldx #%00000010			; FFFF = -1 offset -> set source addressing to HOLD
:		stx dc_cmdh

		clc
		adc dc_mdst+0
		sta dc_msrc+0
		tya
		adc dc_mdst+1
		sta dc_msrc+1

		lda dc_mdst+2									; added for m65 for when we cross banks
		sta dc_msrc+2
		bcs :+
		dec dc_msrc+2
:
		sta $d707										; inline DMA copy
dc_msrcm		.byte $80, ($00000000 >> 20)					; sourcebank
dc_mdstm		.byte $81, ($08000000 >> 20)					; destbank
		.byte $00										; end of job options
		.byte $00										; copy
dc_mlen	.word $0000										; count
dc_msrc	.word $0000										; src
		.byte $00										; src bank and flags
dc_mdst	.word $0000										; dst
		.byte $00										; dst bank and flags
dc_cmdh	.byte $00										; cmd hi
		.word $0000										; modulo, ignored

		ldy dc_mlen
		jsr addput

		;beq dc_end
		jmp dloop

dc_end	rts

; -----------------------------------------------------------------------------------------------

dc_breaknow
		.byte $00