.feature pc_assignment
.feature labels_without_colons
.feature c_comments
; .feature org_per_seg

.include "macros.s"
.include "mathmacros.s"

filebuffer = $0200

; -----------------------------------------------------------------------------------------------

.segment "BASIC"

			.word basicend
			.word 0										; 0
			.byte $fe, $02								; bank
			.byte $30									; 0
			.byte $3a									; :
			.byte $9e									; sys xxxx
			.byte .sprintf("%d", $2011)					; sys xxxx
			.byte 0
basicend	.byte 0
			.byte 0

			jmp entry_main

.include "main.s"
.include "irqload.s"
.include "modplay.s"

; ----------------------------------------------------------------------------------------------------
