.define emptychar		$ff80							; size = 64

.define palette			$ca00
.define altpalette		$cd00

.define screen1			$e000	; 40*25*2 = $0800      ; 80*25*2 = $1000
.define screen2			$f000

.define bmpchars		$10000	; 320x200 = $fa00
.define screenchars1	$20000
.define screenchars2	$30000

.define moddata			$40000

.define matrix4x4_TEMP	$0a00

.define colptr			$90
.define scrptr1			$94
.define scrptr2			$98

.define numverts		58
.define numpolies		112

; ----------------------------------------------------------------------------------------------------

.segment "MAIN"

entry_main

			sei

			lda #$35
			sta $01

			lda #%10000000									; Clear bit 7 - HOTREG
			trb $d05d

			lda #$00										; unmap
			tax
			tay
			taz
			map
			eom

			lda #$47										; enable C65GS/VIC-IV IO registers
			sta $d02f
			lda #$53
			sta $d02f
			eom

			lda #%10000000									; force PAL mode, because I can't be bothered with fixing it for NTSC
			trb $d06f										; clear bit 7 for PAL ; trb $d06f 
			;tsb $d06f										; set bit 7 for NTSC  ; tsb $d06f

			lda #$41										; enable 40MHz
			sta $00

			;lda #$70										; Disable C65 rom protection using hypervisor trap (see mega65 manual)
			;sta $d640
			;eom

			lda #%11111000									; unmap c65 roms $d030 by clearing bits 3-7
			trb $d030

			lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
			sta $d054

			lda #%10100000									; CLEAR bit7=40 column, bit5=Enable extended attributes and 8 bit colour entries
			trb $d031

			lda #40*2										; logical chars per row
			sta $d058
			lda #$00
			sta $d059

			ldx #$00
			lda #$00
:			sta emptychar,x
			inx
			cpx #64
			bne :-

			ldx #$00
:			lda #<(emptychar/64)
			sta screen1+0*$0100+0,x
			sta screen1+1*$0100+0,x
			sta screen1+2*$0100+0,x
			sta screen1+3*$0100+0,x
			sta screen1+4*$0100+0,x
			sta screen1+5*$0100+0,x
			sta screen1+6*$0100+0,x
			sta screen1+7*$0100+0,x
			lda #>(emptychar/64)
			sta screen1+0*$0100+1,x
			sta screen1+1*$0100+1,x
			sta screen1+2*$0100+1,x
			sta screen1+3*$0100+1,x
			sta screen1+4*$0100+1,x
			sta screen1+5*$0100+1,x
			sta screen1+6*$0100+1,x
			sta screen1+7*$0100+1,x
			inx
			inx
			bne :-

			DMA_RUN_JOB clearcolorramjob

			lda #<.loword(screen1)							; set pointer to screen ram
			sta $d060
			lda #>.loword(screen1)
			sta $d061
			lda #<.hiword(screen1)
			sta $d062
			lda #>.hiword(screen1)
			sta $d063

			lda #<$0800										; set (offset!) pointer to colour ram
			sta $d064
			lda #>$0800
			sta $d065

			lda #$7f										; disable CIA interrupts
			sta $dc0d
			sta $dd0d
			lda $dc0d
			lda $dd0d

			lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
			sta $d01a

			lda #$00
			sta $d012
			lda #<fastload_irq_handler
			sta $fffe
			lda #>fastload_irq_handler
			sta $ffff

			lda #$01										; ACK
			sta $d01a

			cli

			jsr fl_init
			jsr fl_waiting
			FLOPPY_IFFL_FAST_LOAD_INIT "MEGAPLY.IFFLCRCH"
			FLOPPY_IFFL_FAST_LOAD_ADDRESS $00010000
			FLOPPY_IFFL_FAST_LOAD_ADDRESS $0000ca00
			FLOPPY_IFFL_FAST_LOAD_ADDRESS $0000cd00
			FLOPPY_IFFL_FAST_LOAD_ADDRESS $00040000

			jsr fl_exit

			sei

			lda #$35
			sta $01

			lda #<.loword(moddata)
			sta adrPepMODL+0
			lda #>.loword(moddata)
			sta adrPepMODL+1
			lda #<.hiword(moddata)
			sta adrPepMODH+0
			lda #>.hiword(moddata)
			sta adrPepMODH+1

			jsr peppitoInit

			lda #$00
			sta $d020
			sta $d021

			lda #$05										; enable Super-Extended Attribute Mode by asserting the FCLRHI and CHR16 signals - set bits 2 and 0 of $D054.
			sta $d054

			lda #%10100000									; Clear bit7=40 column, bit5=disable ...?
			trb $d031

			lda #%00100000									; set bit 5 to enable multicolour mode, needed for alt palette
			tsb $d031

			lda #<80										; CHRCOUNT - Number of visual characters to display per row
			sta $d05e
			lda #>80
			asl
			asl
			asl
			asl
			sta $d063										; ..xx.... high bits of CHRCOUNT

			lda #80*2										; LINESTEPLSB
			sta $d058
			lda #$00
			sta $d059

			lda #$50										; set TEXTXPOS to same as SDBDRWDLSB
			sta $d04c

			DMA_RUN_JOB clearcolorramjob
			DMA_RUN_JOB clearpartialbitmapjob1
			DMA_RUN_JOB clearpartialbitmapjob2

			; pal y border start
			lda #<104
			sta verticalcenter+0
			lda #>104
			sta verticalcenter+1

			bit $d06f
			bpl pal

ntsc		lda #<55
			sta verticalcenter+0
			lda #>55
			sta verticalcenter+1

pal			lda verticalcenter+0
			sta $d048
			lda #%00001111
			trb $d049
			lda verticalcenter+1
			tsb $d049

			lda #<.loword(screen1)							; set pointer to screen ram
			sta $d060
			lda #>.loword(screen1)
			sta $d061
			lda #<.hiword(screen1)
			sta $d062
			lda #>.hiword(screen1)
			sta $d063

			; ----------------------------------------------- SET UP SCREEN 1

			lda #$00
			sta screenrow
			sta screencolumn

			ldx #<(bmpchars / 64)
			ldy #>(bmpchars / 64)

put10		stx screen1+0
put11		sty screen1+1

			clc
			txa
			adc #$01
			tax
			tya
			adc #$00
			tay

			clc
			lda put10+1
			adc #160
			sta put10+1
			lda put10+2
			adc #0
			sta put10+2

			clc
			lda put11+1
			adc #160
			sta put11+1
			lda put11+2
			adc #0
			sta put11+2

			inc screenrow
			lda screenrow
			cmp #25
			bne put10

			lda #0
			sta screenrow
			inc screencolumn
			inc screencolumn
			lda screencolumn
			cmp #80
			beq endscreenplot1

			lda #>(screen1)
			sta put10+2
			sta put11+2
			clc
			lda #<(screen1)
			adc screencolumn
			sta put10+1
			adc #$01
			sta put11+1

			jmp put10

endscreenplot1

			; ----------------------------------------------- SET UP SCREEN 2

			lda #$00
			sta screenrow
			sta screencolumn

			ldx #<(bmpchars / 64)
			ldy #>(bmpchars / 64)

put20		stx screen2+0
put21		sty screen2+1

			clc
			txa
			adc #$01
			tax
			tya
			adc #$00
			tay

			clc
			lda put20+1
			adc #160
			sta put20+1
			lda put20+2
			adc #0
			sta put20+2

			clc
			lda put21+1
			adc #160
			sta put21+1
			lda put21+2
			adc #0
			sta put21+2

			inc screenrow
			lda screenrow
			cmp #25
			bne put20

			lda #0
			sta screenrow
			inc screencolumn
			inc screencolumn
			lda screencolumn
			cmp #80
			beq endscreenplot2

			lda #>(screen2)
			sta put20+2
			sta put21+2
			clc
			lda #<(screen2)
			adc screencolumn
			sta put20+1
			adc #$01
			sta put21+1

			jmp put20

endscreenplot2

			; ----------------------------------------------- SET UP SCREEN 3

			lda #$00
			sta screenrow
			sta screencolumn

			ldx #<(screenchars1 / 64)
			ldy #>(screenchars1 / 64)

put30		stx screen1+40*2+2
put31		sty screen1+40*2+3

			clc
			txa
			adc #$01
			tax
			tya
			adc #$00
			tay

			clc
			lda put30+1
			adc #160
			sta put30+1
			lda put30+2
			adc #0
			sta put30+2

			clc
			lda put31+1
			adc #160
			sta put31+1
			lda put31+2
			adc #0
			sta put31+2

			inc screenrow
			lda screenrow
			cmp #25
			bne put30

			lda #0
			sta screenrow
			inc screencolumn
			inc screencolumn
			lda screencolumn
			cmp #80-2
			beq endscreenplot3

			lda #>(screen1+40*2+2)
			sta put30+2
			sta put31+2
			clc
			lda #<(screen1+40*2+2)
			adc screencolumn
			sta put30+1
			adc #$01
			sta put31+1

			jmp put30

endscreenplot3
			; ----------------------------------------------- SET UP SCREEN 4

			lda #$00
			sta screenrow
			sta screencolumn

			ldx #<(screenchars2 / 64)
			ldy #>(screenchars2 / 64)

put40		stx screen2+40*2+2
put41		sty screen2+40*2+3

			clc
			txa
			adc #$01
			tax
			tya
			adc #$00
			tay

			clc
			lda put40+1
			adc #160
			sta put40+1
			lda put40+2
			adc #0
			sta put40+2

			clc
			lda put41+1
			adc #160
			sta put41+1
			lda put41+2
			adc #0
			sta put41+2

			inc screenrow
			lda screenrow
			cmp #25
			bne put40

			lda #0
			sta screenrow
			inc screencolumn
			inc screencolumn
			lda screencolumn
			cmp #80-2
			beq endscreenplot4

			lda #>(screen2+40*2+2)
			sta put40+2
			sta put41+2
			clc
			lda #<(screen2+40*2+2)
			adc screencolumn
			sta put40+1
			adc #$01
			sta put41+1

			jmp put40

endscreenplot4

			; ----------------------------------------------- END OF SCREEN SETUP

			; set up scr and col ptrs
			lda #<.loword(SAFE_COLOR_RAM+40*2)
			sta colptr+0
			lda #>.loword(SAFE_COLOR_RAM+40*2)
			sta colptr+1
			lda #<.hiword(SAFE_COLOR_RAM+40*2)
			sta colptr+2
			lda #>.hiword(SAFE_COLOR_RAM+40*2)
			sta colptr+3

			lda #<.loword(screen1+40*2)
			sta scrptr1+0
			lda #>.loword(screen1+40*2)
			sta scrptr1+1
			lda #<.hiword(screen1+40*2)
			sta scrptr1+2
			lda #>.hiword(screen1+40*2)
			sta scrptr1+3

			lda #<.loword(screen2+40*2)
			sta scrptr2+0
			lda #>.loword(screen2+40*2)
			sta scrptr2+1
			lda #<.hiword(screen2+40*2)
			sta scrptr2+2
			lda #>.hiword(screen2+40*2)
			sta scrptr2+3

			; ----------------------------------------- set up gotox attribs

			ldx #0
setatrbs
			ldz #0
			lda #%10010000 ; gotox and transparency bits set
			sta [colptr],z
			lda #<40
			sta [scrptr1],z
			sta [scrptr2],z
			inz
			lda #0
			sta [colptr],z
			lda #>40
			sta [scrptr1],z
			sta [scrptr2],z

			ldz #52 ; end of sphere right
			lda #%00010000 ; gotox and transparency bits set
			sta [colptr],z
			lda #<320
			sta [scrptr1],z
			sta [scrptr2],z
			inz
			lda #0
			sta [colptr],z
			lda #>320
			sta [scrptr1],z
			sta [scrptr2],z

			clc
			lda colptr+0
			adc #<160
			sta colptr+0
			lda colptr+1
			adc #>160
			sta colptr+1

			clc
			lda scrptr1+0
			adc #<160
			sta scrptr1+0
			lda scrptr1+1
			adc #>160
			sta scrptr1+1

			clc
			lda scrptr2+0
			adc #<160
			sta scrptr2+0
			lda scrptr2+1
			adc #>160
			sta scrptr2+1

			inx
			cpx #25
			beq endsetatrbt
			jmp setatrbs

endsetatrbt

			; ----------------------------------------- set up alt palette

			; set up scr and col ptrs
			lda #<.loword(SAFE_COLOR_RAM+41*2)
			sta colptr+0
			lda #>.loword(SAFE_COLOR_RAM+41*2)
			sta colptr+1
			lda #<.hiword(SAFE_COLOR_RAM+41*2)
			sta colptr+2
			lda #>.hiword(SAFE_COLOR_RAM+41*2)
			sta colptr+3

			ldx #0
setaltpalette1
			ldz #0
setaltpalette2
			lda #%00000000 ; use this for mirroring chars and stuff
			sta [colptr],z
			inz
			lda #%01100000 ; bold+reverse = alt palette
			sta [colptr],z
			inz
			cpz #48
			bne setaltpalette2

			clc
			lda colptr+0
			adc #<160
			sta colptr+0
			lda colptr+1
			adc #>160
			sta colptr+1

			inx
			cpx #25
			beq endsetaltpalette
			jmp setaltpalette1

endsetaltpalette

			; --------------------------------------------------------------------------

			lda #<$0800										; set (offset!) pointer to colour ram
			sta $d064
			lda #>$0800
			sta $d065

			lda #%00000000									; set bits 6 and 7 to 00 so palette 0 is banked in
			sta $d070

			ldx #$00
:			lda palette+$0000,x
			sta $d100,x
			lda palette+$0100,x
			sta $d200,x
			lda palette+$0200,x
			sta $d300,x
			inx
			bne :-

			lda #%10000000									; set bits 6 and 7 to 01 so palette 1 is banked in
			sta $d070

			ldx #$00										; set bitmap palette
:			lda altpalette+$0000,x
			sta $d100,x
			lda altpalette+$0100,x
			sta $d200,x
			lda altpalette+$0200,x
			sta $d300,x
			inx
			bne :-

			lda #%00000000									; WHY? map the first bank back in
			sta $d070

			lda $d070
			and #%11000000									; set bits 4 and 5 (BTPALSEL) to 00 so bitmap palette is palette 0
			ora #%00000010									; set bits 0 and 1 (ABTPALSEL) to 01 so alt palette is palette 1
			sta $d070

			lda #$7f										; disable CIA interrupts
			sta $dc0d
			sta $dd0d
			lda $dc0d
			lda $dd0d

			lda #$00										; disable IRQ raster interrupts because C65 uses raster interrupts in the ROM
			sta $d01a

			lda #$ff										; setup IRQ interrupt
			sta $d012
			lda #<irq1
			sta $fffe
			lda #>irq1
			sta $ffff

			lda #$01										; ACK
			sta $d01a

			cli
		
loop
			lda $d020
			jmp loop

; ----------------------------------------------------------------------------------------------------

.align 256

irq1
			pha

			;lda #$b0
			;sta $d020

			jsr peppitoPlay

			jsr movescreen

			;lda #$40
			;sta $d020

			lda flipflop
			eor #$ff
			sta flipflop

			bne :+

			lda #<.loword(screen1)							; show screen1, draw to screen2
			sta $d060
			lda #>.loword(screen1)
			sta $d061
			lda #<.hiword(screen1)
			sta $d062
			lda #>.hiword(screen1)
			sta $d063
			lda #((screenchars2 >> 16) & $0f) ; 3
			sta linebuf
			DMA_RUN_JOB clearpartialbitmapjob2
			bra :++

:			lda #<.loword(screen2)							; show screen2, draw to screen1
			sta $d060
			lda #>.loword(screen2)
			sta $d061
			lda #<.hiword(screen2)
			sta $d062
			lda #>.hiword(screen2)
			sta $d063
			lda #((screenchars1 >> 16) & $0f) ; 2
			sta linebuf
			DMA_RUN_JOB clearpartialbitmapjob1
:		

			lda #$40
			sta $d020
			jsr setuprotationmatrix
			jsr buildrotationmatrix
			lda #$c0
			sta $d020
			jsr transformvertices
			jsr drawpolygons

			ldx #$00
			stx $d020

			inc frame

			pla
			asl $d019
			rti

; ----------------------------------------------------------------------------------------------------

movescreen:

			ldx frame
			lda rrbsin8,x
			sta rrbxpos

			; set up scr and col ptrs
			lda #<.loword(SAFE_COLOR_RAM+0*160+40*2)
			sta colptr+0
			lda #>.loword(SAFE_COLOR_RAM+0*160+40*2)
			sta colptr+1
			lda #<.hiword(SAFE_COLOR_RAM+0*160+40*2)
			sta colptr+2
			lda #>.hiword(SAFE_COLOR_RAM+0*160+40*2)
			sta colptr+3

			lda #<.loword(screen1+0*160+40*2)
			sta scrptr1+0
			lda #>.loword(screen1+0*160+40*2)
			sta scrptr1+1
			lda #<.hiword(screen1+0*160+40*2)
			sta scrptr1+2
			lda #>.hiword(screen1+0*160+40*2)
			sta scrptr1+3

			lda #<.loword(screen2+0*160+40*2)
			sta scrptr2+0
			lda #>.loword(screen2+0*160+40*2)
			sta scrptr2+1
			lda #<.hiword(screen2+0*160+40*2)
			sta scrptr2+2
			lda #>.hiword(screen2+0*160+40*2)
			sta scrptr2+3

			; ----------------------------------------- set up gotox attribs

			ldx #0
setatrbs2
			ldz #0
			lda rrbxpos
			sta [scrptr1],z
			sta [scrptr2],z
			inz
			lda #0
			sta [scrptr1],z
			sta [scrptr2],z

			clc
			lda scrptr1+0
			adc #<160
			sta scrptr1+0
			lda scrptr1+1
			adc #>160
			sta scrptr1+1

			clc
			lda scrptr2+0
			adc #<160
			sta scrptr2+0
			lda scrptr2+1
			adc #>160
			sta scrptr2+1

			inx
			cpx #25
			beq endsetatrbt2
			jmp setatrbs2

endsetatrbt2

			rts

; ----------------------------------------------------------------------------------------------------

clearcolorramjob
				.byte $0a										; Request format (f018a = 11 bytes (Command MSB is $00), f018b is 12 bytes (Extra Command MSB))
				.byte $80, $00									; source megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $81, ((SAFE_COLOR_RAM) >> 20)					; dest megabyte   ($0000000 >> 20) ($00 is  chip ram)
				.byte $84, $00									; Destination skip rate (256ths of bytes)
				.byte $85, $02									; Destination skip rate (whole bytes)

				.byte $00										; No more options

																; 11 byte DMA List structure starts here
				.byte %00000111									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*26										; Count LSB + Count MSB

				.word $0007										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((SAFE_COLOR_RAM) & $ffff)				; Destination Address LSB + Destination Address MSB
				.byte (((SAFE_COLOR_RAM) >> 16) & $0f)			; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

				.byte $00										; No more options
				.byte %00000011									; Command LSB
																;     0–1 DMA Operation Type (Only Copy and Fill implemented at the time of writing)
																;             %00 = Copy
																;             %01 = Mix (via MINTERMs)
																;             %10 = Swap
																;             %11 = Fill
																;       2 Chain (i.e., another DMA list follows)
																;       3 Yield to interrupts
																;       4 MINTERM -SA,-DA bit
																;       5 MINTERM -SA, DA bit
																;       6 MINTERM  SA,-DA bit
																;       7 MINTERM  SA, DA bit

				.word 80*26										; Count LSB + Count MSB

				.word $0000										; this is normally the source addres, but contains the fill value now
				.byte $00										; source bank (ignored)

				.word ((SAFE_COLOR_RAM)+1) & $ffff				; Destination Address LSB + Destination Address MSB
				.byte ((((SAFE_COLOR_RAM)+1) >> 16) & $0f)		; Destination Address BANK and FLAGS (copy to rbBaseMem)
																;     0–3 Memory BANK within the selected MB (0-15)
																;       4 HOLD,      i.e., do not change the address
																;       5 MODULO,    i.e., apply the MODULO field to wraparound within a limited memory space
																;       6 DIRECTION. If set, then the address is decremented instead of incremented.
																;       7 I/O.       If set, then I/O registers are visible during the DMA controller at $D000 – $DFFF.

				.word $0000

; -------------------------------------------------------------------------------------------------

clearpartialbitmapjob1
				; f018a = 11 bytes, f018b is 12 bytes
				.byte $0a ; Request format is F018A
				;.byte $80, (bmpchars >> 20) ; sourcebank
				.byte $81, (screenchars1 >> 20) ; destbank

				.byte $82, 0 ; Source skip rate (256ths of bytes)
				.byte $83, 1 ; Source skip rate (whole bytes)

				.byte $84, 0 ; Destination skip rate (256ths of bytes)
				.byte $85, 1 ; Destination skip rate (whole bytes)

				.byte $00 ; No more options

				.byte %00000011	; fill and don't chain
				.word 25*25*64 ; Size of Copy

				.word 0
				.byte 0

				.word screenchars1 & $ffff
				.byte ((screenchars1 >> 16) & $0f)

clearpartialbitmapjob2
				; f018a = 11 bytes, f018b is 12 bytes
				.byte $0a ; Request format is F018A
				;.byte $80, (bmpchars >> 20) ; sourcebank
				.byte $81, (screenchars2 >> 20) ; destbank

				.byte $82, 0 ; Source skip rate (256ths of bytes)
				.byte $83, 1 ; Source skip rate (whole bytes)

				.byte $84, 0 ; Destination skip rate (256ths of bytes)
				.byte $85, 1 ; Destination skip rate (whole bytes)

				.byte $00 ; No more options

				.byte %00000011	; fill and don't chain
				.word 25*25*64 ; Size of Copy

				.word 0
				.byte 0

				.word screenchars2 & $ffff
				.byte ((screenchars2 >> 16) & $0f)

; -------------------------------------------------------------------------------------------------

.segment "TABLES"

sin8
.byte 255, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254

.byte 254, 254, 254, 254, 254, 254, 253, 253, 252, 251, 251, 250, 249, 248, 247, 246, 245, 244, 242, 241, 240, 238, 236, 235, 233, 231, 230, 228, 226, 224, 222, 219
.byte 217, 215, 213, 210, 208, 206, 203, 201, 198, 195, 193, 190, 187, 185, 182, 179, 176, 173, 170, 167, 164, 161, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 097, 094, 091, 088, 085, 082, 079, 076, 073, 070, 068, 065, 062, 060, 057, 054, 052, 049, 047, 045, 042, 040
.byte 038, 036, 033, 031, 029, 027, 025, 024, 022, 020, 019, 017, 015, 014, 013, 011, 010, 009, 008, 007, 006, 005, 004, 004, 003, 002, 002, 001, 001, 001, 001, 001
.byte 001, 001, 001, 001, 001, 001, 002, 002, 003, 004, 004, 005, 006, 007, 008, 009, 010, 011, 013, 014, 015, 017, 019, 020, 022, 024, 025, 027, 029, 031, 033, 036
.byte 038, 040, 042, 045, 047, 049, 052, 054, 057, 060, 062, 065, 068, 070, 073, 076, 079, 082, 085, 088, 091, 094, 097, 100, 103, 106, 109, 112, 115, 118, 121, 124
.byte 127, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 161, 164, 167, 170, 173, 176, 179, 182, 185, 187, 190, 193, 195, 198, 201, 203, 206, 208, 210, 213, 215
.byte 217, 219, 222, 224, 226, 228, 230, 231, 233, 235, 236, 238, 240, 241, 242, 244, 245, 246, 247, 248, 249, 250, 251, 251, 252, 253, 253, 254, 254, 254, 254, 254

sin32

.byte $00,$00,$00,$00, $48,$06,$00,$00, $8F,$0C,$00,$00, $D5,$12,$00,$00
.byte $17,$19,$00,$00, $56,$1F,$00,$00, $90,$25,$00,$00, $C4,$2B,$00,$00
.byte $F1,$31,$00,$00, $17,$38,$00,$00, $33,$3E,$00,$00, $47,$44,$00,$00
.byte $50,$4A,$00,$00, $4D,$50,$00,$00, $3E,$56,$00,$00, $22,$5C,$00,$00
.byte $F7,$61,$00,$00, $BD,$67,$00,$00, $74,$6D,$00,$00, $19,$73,$00,$00
.byte $AD,$78,$00,$00, $2E,$7E,$00,$00, $9C,$83,$00,$00, $F5,$88,$00,$00
.byte $39,$8E,$00,$00, $68,$93,$00,$00, $7F,$98,$00,$00, $7F,$9D,$00,$00
.byte $67,$A2,$00,$00, $36,$A7,$00,$00, $EB,$AB,$00,$00, $85,$B0,$00,$00
.byte $04,$B5,$00,$00, $68,$B9,$00,$00, $AE,$BD,$00,$00, $D8,$C1,$00,$00
.byte $E4,$C5,$00,$00, $D1,$C9,$00,$00, $9F,$CD,$00,$00, $4D,$D1,$00,$00
.byte $DB,$D4,$00,$00, $48,$D8,$00,$00, $94,$DB,$00,$00, $BE,$DE,$00,$00
.byte $C5,$E1,$00,$00, $AA,$E4,$00,$00, $6B,$E7,$00,$00, $09,$EA,$00,$00
.byte $83,$EC,$00,$00, $D8,$EE,$00,$00, $09,$F1,$00,$00, $14,$F3,$00,$00
.byte $FA,$F4,$00,$00, $BA,$F6,$00,$00, $53,$F8,$00,$00, $C7,$F9,$00,$00
.byte $14,$FB,$00,$00, $3B,$FC,$00,$00, $3A,$FD,$00,$00, $13,$FE,$00,$00
.byte $C4,$FE,$00,$00, $4E,$FF,$00,$00, $B1,$FF,$00,$00, $EC,$FF,$00,$00

cos32 ; IMPORTANT - Sin32 table hasn't ended yet. Just reusing the end for cos32
.byte $00,$00,$01,$00, $EC,$FF,$00,$00, $B1,$FF,$00,$00, $4E,$FF,$00,$00
.byte $C4,$FE,$00,$00, $13,$FE,$00,$00, $3A,$FD,$00,$00, $3B,$FC,$00,$00
.byte $14,$FB,$00,$00, $C7,$F9,$00,$00, $53,$F8,$00,$00, $BA,$F6,$00,$00
.byte $FA,$F4,$00,$00, $14,$F3,$00,$00, $09,$F1,$00,$00, $D8,$EE,$00,$00
.byte $83,$EC,$00,$00, $09,$EA,$00,$00, $6B,$E7,$00,$00, $AA,$E4,$00,$00
.byte $C5,$E1,$00,$00, $BE,$DE,$00,$00, $94,$DB,$00,$00, $48,$D8,$00,$00
.byte $DB,$D4,$00,$00, $4D,$D1,$00,$00, $9F,$CD,$00,$00, $D1,$C9,$00,$00
.byte $E4,$C5,$00,$00, $D8,$C1,$00,$00, $AE,$BD,$00,$00, $68,$B9,$00,$00
.byte $04,$B5,$00,$00, $85,$B0,$00,$00, $EB,$AB,$00,$00, $36,$A7,$00,$00
.byte $67,$A2,$00,$00, $7F,$9D,$00,$00, $7F,$98,$00,$00, $68,$93,$00,$00
.byte $39,$8E,$00,$00, $F5,$88,$00,$00, $9C,$83,$00,$00, $2E,$7E,$00,$00
.byte $AD,$78,$00,$00, $19,$73,$00,$00, $74,$6D,$00,$00, $BD,$67,$00,$00
.byte $F7,$61,$00,$00, $22,$5C,$00,$00, $3E,$56,$00,$00, $4D,$50,$00,$00
.byte $50,$4A,$00,$00, $47,$44,$00,$00, $33,$3E,$00,$00, $17,$38,$00,$00
.byte $F1,$31,$00,$00, $C4,$2B,$00,$00, $90,$25,$00,$00, $56,$1F,$00,$00
.byte $17,$19,$00,$00, $D5,$12,$00,$00, $8F,$0C,$00,$00, $48,$06,$00,$00
.byte $00,$00,$00,$00, $B8,$F9,$FF,$FF, $71,$F3,$FF,$FF, $2B,$ED,$FF,$FF
.byte $E9,$E6,$FF,$FF, $AA,$E0,$FF,$FF, $70,$DA,$FF,$FF, $3C,$D4,$FF,$FF
.byte $0F,$CE,$FF,$FF, $E9,$C7,$FF,$FF, $CD,$C1,$FF,$FF, $B9,$BB,$FF,$FF
.byte $B0,$B5,$FF,$FF, $B3,$AF,$FF,$FF, $C2,$A9,$FF,$FF, $DE,$A3,$FF,$FF
.byte $09,$9E,$FF,$FF, $43,$98,$FF,$FF, $8C,$92,$FF,$FF, $E7,$8C,$FF,$FF
.byte $53,$87,$FF,$FF, $D2,$81,$FF,$FF, $64,$7C,$FF,$FF, $0B,$77,$FF,$FF
.byte $C7,$71,$FF,$FF, $98,$6C,$FF,$FF, $81,$67,$FF,$FF, $81,$62,$FF,$FF
.byte $99,$5D,$FF,$FF, $CA,$58,$FF,$FF, $15,$54,$FF,$FF, $7B,$4F,$FF,$FF
.byte $FC,$4A,$FF,$FF, $98,$46,$FF,$FF, $52,$42,$FF,$FF, $28,$3E,$FF,$FF
.byte $1C,$3A,$FF,$FF, $2F,$36,$FF,$FF, $61,$32,$FF,$FF, $B3,$2E,$FF,$FF
.byte $25,$2B,$FF,$FF, $B8,$27,$FF,$FF, $6C,$24,$FF,$FF, $42,$21,$FF,$FF
.byte $3B,$1E,$FF,$FF, $56,$1B,$FF,$FF, $95,$18,$FF,$FF, $F7,$15,$FF,$FF
.byte $7D,$13,$FF,$FF, $28,$11,$FF,$FF, $F7,$0E,$FF,$FF, $EC,$0C,$FF,$FF
.byte $06,$0B,$FF,$FF, $46,$09,$FF,$FF, $AD,$07,$FF,$FF, $39,$06,$FF,$FF
.byte $EC,$04,$FF,$FF, $C5,$03,$FF,$FF, $C6,$02,$FF,$FF, $ED,$01,$FF,$FF
.byte $3C,$01,$FF,$FF, $B2,$00,$FF,$FF, $4F,$00,$FF,$FF, $14,$00,$FF,$FF
.byte $00,$00,$FF,$FF, $14,$00,$FF,$FF, $4F,$00,$FF,$FF, $B2,$00,$FF,$FF
.byte $3C,$01,$FF,$FF, $ED,$01,$FF,$FF, $C6,$02,$FF,$FF, $C5,$03,$FF,$FF
.byte $EC,$04,$FF,$FF, $39,$06,$FF,$FF, $AD,$07,$FF,$FF, $46,$09,$FF,$FF
.byte $06,$0B,$FF,$FF, $EC,$0C,$FF,$FF, $F7,$0E,$FF,$FF, $28,$11,$FF,$FF
.byte $7D,$13,$FF,$FF, $F7,$15,$FF,$FF, $95,$18,$FF,$FF, $56,$1B,$FF,$FF
.byte $3B,$1E,$FF,$FF, $42,$21,$FF,$FF, $6C,$24,$FF,$FF, $B8,$27,$FF,$FF
.byte $25,$2B,$FF,$FF, $B3,$2E,$FF,$FF, $61,$32,$FF,$FF, $2F,$36,$FF,$FF
.byte $1C,$3A,$FF,$FF, $28,$3E,$FF,$FF, $52,$42,$FF,$FF, $98,$46,$FF,$FF
.byte $FC,$4A,$FF,$FF, $7B,$4F,$FF,$FF, $15,$54,$FF,$FF, $CA,$58,$FF,$FF
.byte $99,$5D,$FF,$FF, $81,$62,$FF,$FF, $81,$67,$FF,$FF, $98,$6C,$FF,$FF
.byte $C7,$71,$FF,$FF, $0B,$77,$FF,$FF, $64,$7C,$FF,$FF, $D2,$81,$FF,$FF
.byte $53,$87,$FF,$FF, $E7,$8C,$FF,$FF, $8C,$92,$FF,$FF, $43,$98,$FF,$FF
.byte $09,$9E,$FF,$FF, $DE,$A3,$FF,$FF, $C2,$A9,$FF,$FF, $B3,$AF,$FF,$FF
.byte $B0,$B5,$FF,$FF, $B9,$BB,$FF,$FF, $CD,$C1,$FF,$FF, $E9,$C7,$FF,$FF
.byte $0F,$CE,$FF,$FF, $3C,$D4,$FF,$FF, $70,$DA,$FF,$FF, $AA,$E0,$FF,$FF
.byte $E9,$E6,$FF,$FF, $2B,$ED,$FF,$FF, $71,$F3,$FF,$FF, $B8,$F9,$FF,$FF

.byte $00,$00,$00,$00, $48,$06,$00,$00, $8F,$0C,$00,$00, $D5,$12,$00,$00 ; EXTRA DATA TO COMPLETE COS32 TABLE
.byte $17,$19,$00,$00, $56,$1F,$00,$00, $90,$25,$00,$00, $C4,$2B,$00,$00
.byte $F1,$31,$00,$00, $17,$38,$00,$00, $33,$3E,$00,$00, $47,$44,$00,$00
.byte $50,$4A,$00,$00, $4D,$50,$00,$00, $3E,$56,$00,$00, $22,$5C,$00,$00
.byte $F7,$61,$00,$00, $BD,$67,$00,$00, $74,$6D,$00,$00, $19,$73,$00,$00
.byte $AD,$78,$00,$00, $2E,$7E,$00,$00, $9C,$83,$00,$00, $F5,$88,$00,$00
.byte $39,$8E,$00,$00, $68,$93,$00,$00, $7F,$98,$00,$00, $7F,$9D,$00,$00
.byte $67,$A2,$00,$00, $36,$A7,$00,$00, $EB,$AB,$00,$00, $85,$B0,$00,$00
.byte $04,$B5,$00,$00, $68,$B9,$00,$00, $AE,$BD,$00,$00, $D8,$C1,$00,$00
.byte $E4,$C5,$00,$00, $D1,$C9,$00,$00, $9F,$CD,$00,$00, $4D,$D1,$00,$00
.byte $DB,$D4,$00,$00, $48,$D8,$00,$00, $94,$DB,$00,$00, $BE,$DE,$00,$00
.byte $C5,$E1,$00,$00, $AA,$E4,$00,$00, $6B,$E7,$00,$00, $09,$EA,$00,$00
.byte $83,$EC,$00,$00, $D8,$EE,$00,$00, $09,$F1,$00,$00, $14,$F3,$00,$00
.byte $FA,$F4,$00,$00, $BA,$F6,$00,$00, $53,$F8,$00,$00, $C7,$F9,$00,$00
.byte $14,$FB,$00,$00, $3B,$FC,$00,$00, $3A,$FD,$00,$00, $13,$FE,$00,$00
.byte $C4,$FE,$00,$00, $4E,$FF,$00,$00, $B1,$FF,$00,$00, $EC,$FF,$00,$00

rrbsin8:
    .byte   64,  65,  67,  68,  70,  71,  73,  74,  76,  78,  79,  81,  82,  84,  85,  87
    .byte   88,  89,  91,  92,  94,  95,  96,  98,  99, 100, 102, 103, 104, 105, 106, 108
    .byte  109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 118, 119, 120, 121, 121, 122
    .byte  123, 123, 124, 124, 125, 125, 126, 126, 126, 127, 127, 127, 127, 127, 127, 127
    .byte  127, 127, 127, 127, 127, 127, 127, 127, 126, 126, 126, 125, 125, 124, 124, 123
    .byte  123, 122, 121, 121, 120, 119, 118, 118, 117, 116, 115, 114, 113, 112, 111, 110
    .byte  109, 108, 106, 105, 104, 103, 102, 100,  99,  98,  96,  95,  94,  92,  91,  89
    .byte   88,  87,  85,  84,  82,  81,  79,  78,  76,  74,  73,  71,  70,  68,  67,  65
    .byte   64,  62,  60,  59,  57,  56,  54,  53,  51,  49,  48,  46,  45,  43,  42,  40
    .byte   39,  38,  36,  35,  33,  32,  31,  29,  28,  27,  25,  24,  23,  22,  21,  19
    .byte   18,  17,  16,  15,  14,  13,  12,  11,  10,   9,   9,   8,   7,   6,   6,   5
    .byte    4,   4,   3,   3,   2,   2,   1,   1,   1,   0,   0,   0,   0,   0,   0,   0
    .byte    0,   0,   0,   0,   0,   0,   0,   0,   1,   1,   1,   2,   2,   3,   3,   4
    .byte    4,   5,   6,   6,   7,   8,   9,   9,  10,  11,  12,  13,  14,  15,  16,  17
    .byte   18,  19,  21,  22,  23,  24,  25,  27,  28,  29,  31,  32,  33,  35,  36,  38
    .byte   39,  40,  42,  43,  45,  46,  48,  49,  51,  53,  54,  56,  57,  59,  60,  62

times4lo
.repeat 256, I
		.byte <(I*4)
.endrepeat

times4hi
.repeat 256, I
		.byte >(I*4)
.endrepeat

lineartable
.repeat 256, I
		.byte I
.endrepeat		

slopetop
.repeat 256
		.byte 0
.endrepeat

slopebottom
.repeat 256
		.byte 0
.endrepeat

vertsxconv
.repeat 256
		.byte 0
.endrepeat

vertsyconv
.repeat 256
		.byte 0
.endrepeat

.align 256

dstcolumnlo
		.repeat 32, I
			.byte <((I*25*64) + 0), <((I*25*64) + 1), <((I*25*64) + 2), <((I*25*64) + 3), <((I*25*64) + 4), <((I*25*64) + 5), <((I*25*64) + 6), <((I*25*64) + 7)
		.endrepeat

dstcolumnhi
		.repeat 32, I
			.byte >((I*25*64) + 0), >((I*25*64) + 1), >((I*25*64) + 2), >((I*25*64) + 3), >((I*25*64) + 4), >((I*25*64) + 5), >((I*25*64) + 6), >((I*25*64) + 7)
		.endrepeat

flipflop		.byte 255
frame			.byte 0
angle			.word 0
screenrow		.byte 0
screencolumn	.byte 0
vertindex		.byte 0
polyindex		.byte 0
pihi			.byte 0
rrbxpos			.byte 0

verticalcenter	.word 0

q0				.byte $00, $00, $00, $00
q32				.byte $00, $00, $20, $00
q80				.byte $00, $00, $c0, $00
q100			.byte $00, $00, $60, $00
q128			.byte $00, $00, $80, $00

qlightadd		.byte $00, $00, $10, $00
qlightmult		.byte $00, $00, $2e, $00

qdistance		.byte $00, $00, $08, $00

lightvec		.byte $00, $80, $ff, $ff,    $00, $00, $00 ,$00,    $00, $e0, $00, $00

colorremap		.byte $00, $40

; x = 00 00 ff ff = light comes from left
; z = 00 00 01 00 = light comes from front