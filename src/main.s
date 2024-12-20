.define emptychar		$ff80							; size = 64

.define palette			$c000
.define altpalette		$c800

.define screen1			$e000	; 40*25*2 = $0800      ; 80*25*2 = $1000
.define screen2			$f000

.define bmpchars		$10000	; 320x200 = $fa00
.define screenchars1	$20000
.define screenchars2	$30000

.define moddata			$40000

.define colptr			$90
.define scrptr1			$94
.define scrptr2			$98

.define x1				$90								; overwrites rotation matrix in ZP/BP, but we're done with that anyway
.define y1				$94
.define y2				$98

.define leftDelta		$9c								; overwrites rotation matrix in ZP/BP, but we're done with that anyway
.define rightDelta		$a0
.define totalDelta		$a4

.define vxptr			$c0
.define vyptr			$c2
.define vzptr			$c4
.define vxcptr			$c6
.define vycptr			$c8

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
:		sta emptychar,x
		inx
		cpx #64
		bne :-

		ldx #$00
:		lda #<(emptychar/64)
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
		FLOPPY_IFFL_FAST_LOAD_ADDRESS $0000c000
		FLOPPY_IFFL_FAST_LOAD_ADDRESS $0000c800
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
		DMA_RUN_JOB cleare000

		; pal y border start
		lda #<104
		sta verticalcenter+0
		lda #>104
		sta verticalcenter+1

		bit $d06f
		bpl pal

ntsc	lda #<55
		sta verticalcenter+0
		lda #>55
		sta verticalcenter+1

pal		lda verticalcenter+0
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

put10	stx screen1+0
put11	sty screen1+1

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

put20	stx screen2+0
put21	sty screen2+1

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

put30	stx screen1+40*2+2
put31	sty screen1+40*2+3

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

put40	stx screen2+40*2+2
put41	sty screen2+40*2+3

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
:		lda palette+$0000,x
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
:		lda altpalette+$0000,x
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

.macro SWAP this, that
		ldq this
		stq FP_A
		ldq that
		stq this
		ldq FP_A
		stq that
.endmacro		

.macro DRAWLINE
		DMA_RUN_JOB_FAST drawlinejob
.endmacro


drawpoly

		;lda #$80
		;sta $d020

		; ----------------------------------------------- swap points if needed

		lda leftX+2
		cmp midX+2
		bmi :+
		SWAP leftX, midX
		SWAP leftY, midY
:		lda leftX+2
		cmp rightX+2
		bmi :+
		SWAP leftX, rightX
		SWAP leftY, rightY
:		lda midX+2
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

		MATH_DIV leftSpanY,  leftSpanX,  leftDelta
		MATH_DIV rightSpanY, rightSpanX, rightDelta
		MATH_DIV totalSpanY, totalSpanX, totalDelta

		; check if we're inverted (I.E. longest slope is running at the top)

		MATH_MUL leftSpanX, totalDelta, FP_A	; optimise this later. no need to store in temp Q reg
		ldq FP_A
		clc
		adcq leftY
		cmpq midY
		bmi :+
		lda #$00
		sta inverse
		bra :++
:		lda #$01
		sta inverse
:
		; ----------------------------------------------- set up polygon

		ldq leftY
		stq y1
		stq y2





.macro SETUP_START_XY xx, yy
		ldq xx
		stq x1
		ldq yy
		stq y1
.endmacro

.macro CALCULATE_SPAN
		sec
		lda y2+2
		sbc y1+2
		sta linesize+0
.endmacro

.macro SETUP_LINESTART
		ldx x1+2
		ldy y1+2
		clc
		lda pixelxlo,x
		adc pixelylo,y
		sta linestart+0
		lda pixelxhi,x
		adc pixelyhi,y
		sta linestart+1
.endmacro

.macro INCREASEX delta1, delta2
.scope
		;clc
		lda y1+0
		adc delta1+0
		sta y1+0
		lda y1+1
		adc delta1+1
		sta y1+1
		lda y1+2
		adc delta1+2
		sta y1+2

		;clc
		lda y2+0
		adc delta2+0
		sta y2+0
		lda y2+1
		adc delta2+1
		sta y2+1
		lda y2+2
		adc delta2+2
		sta y2+2

		inc x1+2										; increase x by 1
		lda x1+2
.endscope
.endmacro

		;lda #$bc
		;sta $d020

		; ---------------------------------------------- DRAW LEFT

drawleft
		ldq leftSpanX
		bne :+
		jmp drawright									; don't draw left side if it doesn't exist
:		lda inverse
		beq drawleftnoinverse
		jmp drawleftinverse

drawleftnoinverse
		SETUP_START_XY leftX, leftY

drawleftnoinverseloop
		CALCULATE_SPAN
		beq :+
		SETUP_LINESTART
		DRAWLINE
:		INCREASEX leftDelta, totalDelta
		cmp midX+2
		bmi drawleftnoinverseloop
		jmp drawright

drawleftinverse ; -------------------------------------- INVERSE
		SETUP_START_XY leftX, leftY

drawleftinverseloop
		CALCULATE_SPAN
		beq :+
		SETUP_LINESTART
		DRAWLINE
:		INCREASEX totalDelta, leftDelta
		cmp midX+2
		bmi drawleftinverseloop
		jmp drawright

		; ---------------------------------------------- DRAW RIGHT

drawright
		ldq rightSpanX
		bne :+
		rts												; don't draw right side if it doesn't exist
:		lda inverse
		beq drawrightnoinverse
		jmp drawrightinverse

drawrightnoinverse
		SETUP_START_XY midX, midY

drawrightnoinverseloop
		CALCULATE_SPAN
		beq :+
		SETUP_LINESTART
		DRAWLINE
:		INCREASEX rightDelta, totalDelta
		cmp rightX+2
		bmi drawrightnoinverseloop
		rts

drawrightinverse ; ------------------------------------- INVERSE
		ldq midX
		stq x1
		ldq midY
		stq y2											; refresh memory - why y2 and not y1?

drawrightinverseloop
		CALCULATE_SPAN
		beq :+
		SETUP_LINESTART
		DRAWLINE
:		INCREASEX totalDelta, rightDelta
		cmp rightX+2
		bmi drawrightinverseloop
		rts









; ----------------------------------------------------------------------------------------------------

flipflop
		.byte 255

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

:		lda #<.loword(screen2)							; show screen2, draw to screen1
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

		;lda #$20
		;sta $d020

		jsr calc_distance

		; initialize dma job pointers before we start drawing polygons
		DMA_INIT_FAST_JOB drawlinejob

		;lda #$b4
		;sta $d020

		;inc $d020

		lda frame
		sta angle+0
		lda #$00
		sta angle+1

		; multiply by 4 because sin/cos values are stored as 16.16 fixed point
		asl angle+0
		rol angle+1
		asl angle+0
		rol angle+1

		; feed angle in as value for sinx, siny, sinz

		clc
		lda angle+0
		adc #<sin32
		sta foo1+3		; +3 to skip LDQ (= NEG NEG LDA)
		lda angle+1
		adc #>sin32
		sta foo1+4

		; feed angle in as value for sinx, siny, sinz

		clc
		lda angle+0
		adc #<cos32
		sta foo2+3		; +3 to skip LDQ (= NEG NEG LDA)
		lda angle+1
		adc #>cos32
		sta foo2+4

foo1	ldq sin32
		stq sx
		stq sy
		;ldq q0
		stq sz
foo2	ldq cos32
		stq cx
		stq cy
		;ldq q1
		stq cz

		MATH_BUILD_ROTMAT

		;lda #$b8
		;sta $d020

		; ---------------------------- ROTATE POINTS

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

		lda #$00
rploop	sta vertindex

		;inc $d020

		ldz vertindex
		ldq (vxptr),z
		stq sx
		ldz vertindex
		ldq (vyptr),z
		stq sy
		ldz vertindex
		ldq (vzptr),z
		stq sz

		MATH_DOT sx, m11, sy, m12, sz, m13, t1, t2, t3, fx
		MATH_DOT sx, m21, sy, m22, sz, m23, t1, t2, t3, fy
		MATH_DOT sx, m31, sy, m32, sz, m33, t1, t2, t3, fz

		; take distance, sub z
		ldq qdistance
		sec
		sbcq fz
		stq fz

		; multiply by factor
		ldq q80
		MATH_DIV q80, fz, fz
		MATH_MUL fx, fz, fx
		MATH_MUL fy, fz, fy

		MATH_ADD fx, q100, fx
		MATH_ADD fy, q100, fy

		ldy vertindex
		lda fx+2
		sta vertsxconv+2,y
		lda fx+3
		sta vertsxconv+3,y
		lda fy+2
		sta vertsyconv+2,y
		lda fy+3
		sta vertsyconv+3,y

		clc
		lda vertindex
		adc #$04
		cmp #numverts*4
		beq :+
		jmp rploop
:

		;lda #$20
		;sta $d020

		; ---------------------------- DRAW POLYGONS

		lda #<vertsxconv
		sta vxcptr+0
		lda #>vertsxconv
		sta vxcptr+1

		lda #<vertsyconv
		sta vycptr+0
		lda #>vertsyconv
		sta vycptr+1

		lda #$00
dploop	sta polyindex

		ldx polyindex
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

		ldq midY										; calculate winding order
		sec
		sbcq leftY
		stq t1

		ldq rightX
		sec
		sbcq midX
		stq t2

		ldq midX
		sec
		sbcq leftX
		stq t3

		ldq rightY
		sec
		sbcq midY
		stq t4

		MATH_MUL t1, t2, t5
		MATH_MUL t3, t4, t6
		MATH_SUB t5, t6, t1

		bit t1+3
        bmi :+
		jmp skippolydraw
:

		; ROTATE/LIGHT NORMALS/POLYS

		ldx polyindex
		lda times4lo,x
		sta pilo
		lda times4hi,x
		sta pihi

		clc
		lda #<normalsx
		adc pilo
		sta vxptr+0
		lda #>normalsx
		adc pihi
		sta vxptr+1

		clc
		lda #<normalsy
		adc pilo
		sta vyptr+0
		lda #>normalsy
		adc pihi
		sta vyptr+1

		clc
		lda #<normalsz
		adc pilo
		sta vzptr+0
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

		MATH_DOT sx, m11, sy, m12, sz, m13, t1, t2, t3, fx
		MATH_DOT sx, m21, sy, m22, sz, m23, t1, t2, t3, fy
		MATH_DOT sx, m31, sy, m32, sz, m33, t1, t2, t3, fz

		MATH_DOT fx, lightvec+0, fy, lightvec+4, fz, lightvec+8, t1, t2, t3, fx

		MATH_MUL fx, q16, fx

		ldx polyindex
		sec
		lda orgcol,x
		sbc #$c0
		asl
		clc
		adc fx+2
		sta linecolour

		jsr drawpoly
		;lda #0
		;sta $d020

skippolydraw

		clc
		lda polyindex
		adc #$01
		cmp #numpolies
		beq :+
		jmp dploop
:


		ldx #$00
		stx $d020

		inc frame

		pla
		asl $d019
		rti

calc_distance
		lda #$00
		sta qdistance+0
		sta qdistance+1
		sta qdistance+2
		sta qdistance+3

		ldx frame
		lda sine,x
		lsr ; put into 0-128 range
		sta qdistance+2
		; divide by 32
		MATH_DIV qdistance, q32, qdistance
		clc
		lda qdistance+2
		adc #$04
		sta qdistance+2
		rts

.align 256

pixelxlo
		.repeat 64, I
			.byte <((I*25*64) + 0), <((I*25*64) + 1), <((I*25*64) + 2), <((I*25*64) + 3), <((I*25*64) + 4), <((I*25*64) + 5), <((I*25*64) + 6), <((I*25*64) + 7)
		.endrepeat

pixelxhi
		.repeat 64, I
			.byte >((I*25*64) + 0), >((I*25*64) + 1), >((I*25*64) + 2), >((I*25*64) + 3), >((I*25*64) + 4), >((I*25*64) + 5), >((I*25*64) + 6), >((I*25*64) + 7)
		.endrepeat

pixelylo
		.repeat 256, I
			.byte <(I*8)
		.endrepeat

pixelyhi
		.repeat 256, I
			.byte >(I*8)
		.endrepeat

frame
		.byte $00

angle
		.word $00

screenrow
		.byte 0

screencolumn
		.byte 0

vertindex
		.byte 0

normalindex
		.byte 0

polyindex
		.byte 0

pilo
		.byte 0

pihi
		.byte 0

rrbxpos	.byte 0


movescreen

		ldx frame
		lda rrbsine,x
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
				;DMA_HEADER $20000 >> 20, $30000 >> 20
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
				;DMA_HEADER $20000 >> 20, $30000 >> 20
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

cleare000
				;DMA_HEADER $20000 >> 20, $30000 >> 20
				; f018a = 11 bytes, f018b is 12 bytes
				.byte $0a ; Request format is F018A
				;.byte $80, (bmpchars >> 20) ; sourcebank
				.byte $81, ($e000 >> 20) ; destbank

				.byte $82, 0 ; Source skip rate (256ths of bytes)
				.byte $83, 1 ; Source skip rate (whole bytes)

				.byte $84, 0 ; Destination skip rate (256ths of bytes)
				.byte $85, 1 ; Destination skip rate (whole bytes)

				.byte $00 ; No more options

				.byte %00000011	; fill and don't chain
				.word 8190 ; Size of fill

				.word 0
				.byte 0

				.word $e000 & $ffff
				.byte (($e000 >> 16) & $0f)

; -------------------------------------------------------------------------------------------------

drawlinejob
				.byte $0a ; Request format is F018A
				.byte $81, (screenchars1 >> 20) ; destbank

				.byte $84, 0 ; Destination skip rate (256ths of bytes)
				.byte $85, 8 ; Destination skip rate (whole bytes)

				.byte $00 ; No more options

				.byte %00000011	; fill and last request

linesize		.word 1 ; Size of Copy

linecolour		.word $0001	; this is normally the source addres, but contains the fill value now
				.byte $00	; source bank (ignored)

linestart		.word (screenchars1 & $ffff)
linebuf			.byte ((screenchars1 >> 16) & $0f)

				.word $0000 ; MSB command

; -------------------------------------------------------------------------------------------------

.segment "TABLES"

sine
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

cos32

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

rrbsine:
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

verticalcenter	.word 0

leftX			.byte $00, $00, $00, $00
leftY			.byte $00, $00, $00, $00

midX			.byte $00, $00, $00, $00
midY			.byte $00, $00, $00, $00

rightX			.byte $00, $00, $00, $00
rightY			.byte $00, $00, $00, $00

midXEnd			.byte $00, $00, $00, $00
rightXEnd		.byte $00, $00, $00, $00

leftSpanX		.byte $00, $00, $00, $00
rightSpanX		.byte $00, $00, $00, $00
totalSpanX		.byte $00, $00, $00, $00

leftSpanY		.byte $00, $00, $00, $00
rightSpanY		.byte $00, $00, $00, $00
totalSpanY		.byte $00, $00, $00, $00

q0				.byte $00, $00, $00, $00
q1				.byte $00, $00, $01, $00
q16				.byte $00, $00, $3e, $00
q32				.byte $00, $00, $20, $00
q80				.byte $00, $00, $c0, $00
q100			.byte $00, $00, $60, $00

qdistance		.byte $00, $00, $08, $00

inverse			.byte $00

lightvec		.byte $00, $00, $00 ,$00,    $00, $00, $00, $00,    $00, $00, $01, $00

vertsxconv
.repeat 256
		.byte $00
.endrepeat

vertsyconv
.repeat 256
		.byte $00
.endrepeat

.align 256
litcol
.repeat 256
		.byte $40
.endrepeat






.include "model.s"