.define COLOR_RAM $ff80000
.define SAFE_COLOR_RAM COLOR_RAM+2048

.define COLOR_RAM_FREESPACE COLOR_RAM+2048+8192

.macro DMA_RUN_JOB jobPointer
		lda #(jobPointer & $ff0000) >> 16
		sta $d702
		sta $d704
		lda #>jobPointer
		sta $d701
		lda #<jobPointer
		sta $d705
.endmacro

.macro FLOPPY_IFFL_FAST_LOAD_INIT fname
.scope
			bra :+
FileName	.byte .sprintf("%s", fname), 0
:			
			ldx #<FileName
			ldy #>FileName
			jsr fl_set_filename

			lda #$01										; Request fastload job
			sta fastload_request
			jsr fl_waiting
.endscope
.endmacro

.macro FLOPPY_IFFL_FAST_LOAD_ADDRESS address
.scope
			lda #<.loword(address)
			sta iffl_unpackaddress+0
			lda #>.loword(address)
			sta iffl_unpackaddress+1
			lda #<.hiword(address)
			sta iffl_unpackaddress+2
			lda #>.hiword(address)
			sta iffl_unpackaddress+3
			
			jsr iffl_loadanddecrunchnextfile
.endscope
.endmacro

.macro BASIC_UPSTART addr64, addr65
			.byte $01, $20								; $2001 start address

line10		.word line20								; end of command marker (first byte after the 00 terminator)
			.word 10									; 10
			.byte $8b, $c2								; if peek
			.byte $28, $34, $34, $29					; (44)
			.byte $b2									; ==
			.byte $38									; 8
			.byte $a7									; then
			.byte $9e									; sys xxxx
			.byte .sprintf("%d", addr64)
			.byte $00

line20		.word line30
			.word 20									; 20
			.byte $fe, $02								; bank
			.byte $30									; 0
			.byte 0

line30		.word basicend
			.byte 30, $00								; 30
			.byte $9e									; sys xxxx
			.byte .sprintf("%d", addr65)				; sys xxxx
			.byte 0

basicend	.byte 0
			.byte 0
.endmacro
