; ----------------------------------------------------------------------------------------------------

; D080        IRQ     LED     MOTOR   SWAP    SIDE    DS      DS      DS      
;
; IRQ         The floppy controller has generated an interrupt (read only). Note that interrupts are not currently implemented on the 45GS27
; LED         Drive LED blinks when set
; MOTOR       Activates drive motor and LED (unless LED signal is also set, causing the drive LED to blink)
; SWAP        Swap upper and lower halves of data buffer (i.e. invert bit 8 of the sector buffer)
; DS          Drive select (0 to 7). Internal drive is 0. Second floppy drive on internal cable is 1. Other values reserved for C1565 external drive interface

; ----------------------------------------------------------------------------------------------------

; D081        WRCMD   RDCMD   FREE    STEP    DIR     ALGO    ALT     NOBUF   
;
; WRCMD       Command is a write operation if set
; RDCMD       Command is a read operation if set
; FREE        Command is a free-format (low level) operation
; STEP        Writing 1 causes the head to step in the indicated direction
; DIR         Sets the stepping direction (inward vs
; ALGO        Selects reading and writing algorithm (currently ignored)
; ALT         Selects alternate DPLL read recovery method (not implemented)
; NOBUF       Reset the sector buffer read/write pointers

; ----------------------------------------------------------------------------------------------------

; D082        BUSY    DRQ     EQ      RNF     CRC     LOST    PROT    TK0     
;
; BUSY        F011 FDC busy flag (command is being executed) (read only)
; DRQ         F011 FDC DRQ flag (one or more bytes of data are ready) (read only)
; EQ          F011 FDC CPU and disk pointers to sector buffer are equal, indicating that the sector buffer is either full or empty. (read only)
; RNF         F011 FDC Request Not Found (RNF), i.e., a sector read or write operation did not find the requested sector (read only)
; CRC         F011 FDC CRC check failure flag (read only)
; LOST        F011 LOST flag (data was lost during transfer, i.e., CPU did not read data fast enough) (read only)
; PROT        F011 Disk write protect flag (read only)
; TK0         F011 Head is over track 0 flag (read only)

; ----------------------------------------------------------------------------------------------------

; D083        RDREQ   WTREQ   RUN     WGATE   DISKIN  INDEX   IRQ     DSKCHG  
; 
; RDREQ       F011 Read Request flag, i.e., the requested sector was found during a read operation (read only)
; WTREQ       F011 Write Request flag, i.e., the requested sector was found during a write operation (read only
; RUN         F011 Successive match. A synonym of RDREQ on the 45IO47 (read only)
; WGATE       F011 write gate flag. Indicates that the drive is currently writing to media. Bad things may happen if a write transaction is aborted (read only)
; DISKIN      F011 Disk sense (read only)
; INDEX       F011 Index hole sense (read only)
; IRQ         The floppy controller has generated an interrupt (read only). Note that interrupts are not currently implemented on the 45GS27.
; DSKCHG      G F011 disk change sense (read only)

; ----------------------------------------------------------------------------------------------------

; D084        TRACK
; D085        SECTOR
; D086        SIDE
; D087        DATA
; D088        CLOCK
; D089        STEP
; D08A        PCODE

; ----------------------------------------------------------------------------------------------------

.feature pc_assignment
.feature labels_without_colons
.feature c_comments

; ----------------------------------------------------------------------------------------------------

fl_init
		lda #$60										; Start motor
		sta $d080

		rts

; ----------------------------------------------------------------------------------------------------

fl_exit
		lda #$00										; Stop motor
		sta $d080

		rts

; ----------------------------------------------------------------------------------------------------

fl_set_filename

		stx fl_fnptr+1
		sty fl_fnptr+2

		ldx #$0f
		lda #$a0
:		sta fastload_filename,x
		dex
		bpl :-

		ldx #$ff
filenamecopyloop
		inx
		cpx #$10
		beq endofname
fl_fnptr
		lda $1000,x
		beq endofname
		sta fastload_filename,x
		bne filenamecopyloop

endofname
		inx
		stx fastload_filename_len

		rts

; ----------------------------------------------------------------------------------------------------

fl_set_startaddress

		lda #$00										; Set load address (32-bit) $07ff ($0801 - 2 bytes for BASIC header)
		sta fastload_address + 0
		lda #$80
		sta fastload_address + 1
		lda #$00
		sta fastload_address + 2
		lda #$00
		sta fastload_address + 3

		rts

; ----------------------------------------------------------------------------------------------------

fl_waiting
		jsr fastload_irq
		lda fastload_request							; Then just wait for the request byte to
		bmi fl_error									; go back to $00, or to report an error by having the MSB
		bne fl_waiting									; set. The request value will continually update based on the
		beq fl_waiting_done								; state of the loading.

fl_error
		lda #$02
		sta $d020
		lda #$07
		sta $d020
		jmp fl_error

fl_waiting_done
		rts

; ----------------------------------------------------------------------------------------------------

fastload_irq_handler
		php
		pha
		txa
		pha
		tya
		pha

		;jsr fastload_irq

		ldx #$00
:		lda fastload_sector_buffer,x
		ldy #$00
:		dey
		bne :-
		sta $d021
		sta $d020
		dex
		bne :--

		ldx #$00
:		lda fastload_sector_buffer+$0100,x
		ldy #$00
:		dey
		bne :-
		sta $d021
		sta $d020
		dex
		bne :--

		lda #$00
		sta $d020
		sta $d021

		pla
		tay
		pla
		tax
		pla
		plp
		asl $d019
		rti

; ------------------------------------------------------------------------------------------------------------------------------
; Actual fast-loader code
; ------------------------------------------------------------------------------------------------------------------------------

fastload_filename
.repeat 16
		.byte 0
.endrepeat

fastload_filename_len
		.byte 0

fastload_address
		.byte 0, 0, 0, 0

fastload_request
		;.byte 4										; Start with seeking to track 0
		.byte 6											; Start by getting dir entries

		; $00 = fl_idle									; idle
		; $01 = fl_new_request							; requested
		; $02 = fl_directory_scan						; acknowledged
		; $03 = fl_read_file_block						; advance to next state
		; $04 = fl_seek_track_0							; seek to track 0
		; $05 = fl_reading_sector						; track stepping/sector reading state
		; $80 = File not found							; file not found

fastload_request_stashed								; Remember the state that requested a sector read
		.byte 0

fl_current_track
		.byte 0
fl_file_next_track
		.byte 0
fl_file_next_sector
		.byte 0

fl_prev_track
		.byte 0
fl_prev_sector
		.byte 0
fl_prev_side
		.byte 0

.align 256
fastload_sector_buffer
.repeat 512
		.byte 0
.endrepeat

fastload_directory_entries
.repeat 256
		.byte 0
.endrepeat

fastload_irq
		lda fastload_request							; If the FDC is busy, do nothing, as we can't progress.
		bne todo										; This really simplifies the state machine into a series of sector reads
		rts

todo	lda $d082
		bpl fl_fdc_not_busy								; wait for the BUSY flag (bit 7 of $D082)
		rts

fl_fdc_not_busy
		lda fastload_request							; FDC is not busy, so check what state we are in
		bpl fl_not_in_error_state

		rts

fl_not_in_error_state
		cmp #8
		bcc fl_job_ok
		rts												; Ignore request/status codes that don't correspond to actions

fl_job_ok
		asl												; Shift state left one bit, so that we can use it as a lookup
		tax												; into a jump table. Everything else is handled by the jump table
		jmp (fl_jumptable,x)

fl_jumptable
		.word fl_idle									; 0
		.word fl_new_request							; 1
		.word fl_directory_scan							; 2
		.word fl_read_file_block						; 3
		.word fl_seek_track_0							; 4
		.word fl_reading_sector							; 5
		.word fl_new_dir_request						; 6
		.word fl_collect_dir_entries					; 7

fl_idle
		rts

fl_seek_track_0
		lda $d082
		and #$01										; TK0 - F011 Head is over track 0 flag (read only)
		beq fl_not_on_track_0
		lda #$00
		sta fastload_request
		sta fl_current_track
		rts

fl_not_on_track_0
		lda #$10										; Step back towards track 0
		sta $d081
		rts

fl_select_side1
		lda #$01
		sta $d086										; requested side
		lda #$60										; Sides are inverted on the 1581
		sta $d080										; physical side selected of mechanical drive
		rts

fl_select_side0
		lda #$00
		sta $d086										; requested side
		lda #$68										; Sides are inverted on the 1581
		sta $d080										; physical side selected of mechanical drive
		rts

fl_new_request
		lda #2											; Acknowledge fastload request
		sta fastload_request

		lda #40-1										; Request Track 40 Sector 3 to start directory scan
		sta $d084										; (remember we have to do silly translation to real sectors)
		lda #(3/2)+1
		sta $d085
		jsr fl_select_side0

		jsr fl_read_sector								; Request read
		rts

fl_read_dir_for_ui

		lda #40-1										; Request Track 40 Sector 3 to start directory scan
		sta $d084										; (remember we have to do silly translation to real sectors)
		lda #(3/2)+1
		sta $d085
		jsr fl_select_side0

		jsr fl_read_sector								; Request read
		rts
		
; ------------------------------------------------------------------------------------------------------------------------------

fl_new_dir_request
		lda #7											; Acknowledge fastload request
		sta fastload_request

		lda #40-1										; Request Track 40 Sector 3 to start directory scan
		sta $d084										; (remember we have to do silly translation to real sectors)
		lda #(3/2)+1
		sta $d085
		jsr fl_select_side0

		lda #<fastload_sector_buffer
		sta fl_cde_buffaddr1+1
		sta fl_cde_buffaddr2+1
		sta fl_cde_buffaddr3+1
		sta fl_cde_buffaddr4+1
		clc
		lda #>fastload_sector_buffer
		adc #$01										; start in second half
		sta fl_cde_buffaddr1+2
		sta fl_cde_buffaddr2+2
		sta fl_cde_buffaddr3+2
		sta fl_cde_buffaddr4+2

		lda #$00
		sta fl_cde_idx

		jsr fl_read_sector								; Request read
		rts

; ------------------------------------------------------------------------------------------------------------------------------

fl_char0
		.byte $00
fl_char1
		.byte $00

fl_find_dir_entry

		stx fl_char0
		sty fl_char1

		ldy #$02
fl_fde_loop
		lda fastload_directory_entries+0,y
		cmp fl_char0
		bne next_entry
		lda fastload_directory_entries+1,y
		cmp fl_char1
		bne next_entry
		tya
		sec
		sbc #$02
		tax
		ldy fastload_directory_entries+0,x
		lda fastload_directory_entries+1,x
		jmp fl_got_file_track_and_sector

next_entry
		iny
		iny
		iny
		iny
		jmp fl_fde_loop

; ------------------------------------------------------------------------------------------------------------------------------

fl_cde_idx
		.byte $00

fl_collect_dir_entries
		jsr fl_copy_sector_to_buffer

		ldy fl_cde_idx
fl_cde_loop
		ldx #$03

fl_cde_buffaddr1
		lda fastload_sector_buffer+$0100,x				; track
		sta fastload_directory_entries,y
		inx
		iny
fl_cde_buffaddr2
		lda fastload_sector_buffer+$0100,x				; sector
		sta fastload_directory_entries,y
		inx
		iny
fl_cde_buffaddr3
		lda fastload_sector_buffer+$0100,x				; first char
		sta fastload_directory_entries,y
		inx
		iny
fl_cde_buffaddr4
		lda fastload_sector_buffer+$0100,x				; second char
		sta fastload_directory_entries,y
		inx
		iny
		txa												; Advance to next directory entry
		clc
		adc #$20-4
		tax
		bcc fl_cde_buffaddr1

		sty fl_cde_idx
		inc fl_cde_buffaddr1+2
		inc fl_cde_buffaddr2+2
		inc fl_cde_buffaddr3+2
		inc fl_cde_buffaddr4+2
		lda fl_cde_buffaddr1+2
		cmp #(>fastload_sector_buffer)+1
		bne fl_cde_checked_both_halves
		jmp fl_cde_loop

fl_cde_checked_both_halves
		inc $d085										; No matching name in this 512 byte sector.
		lda $d085										; Load the next one, or give up the search

		cmp #6											; COMPARE 6 TRACKS. INCREASE IF WE RUN OUT OF SPACE!!!

		beq fl_stop_collect_dir_entries

		lda #<fastload_sector_buffer
		sta fl_cde_buffaddr1+1
		sta fl_cde_buffaddr2+1
		sta fl_cde_buffaddr3+1
		sta fl_cde_buffaddr4+1
		lda #>fastload_sector_buffer
		sta fl_cde_buffaddr1+2
		sta fl_cde_buffaddr2+2
		sta fl_cde_buffaddr3+2
		sta fl_cde_buffaddr4+2

		jsr fl_read_sector								; Request read. No need to change state
		rts

fl_stop_collect_dir_entries
		lda #$00
		sta fastload_request

		rts

; ------------------------------------------------------------------------------------------------------------------------------

fl_directory_scan

		jsr fl_copy_sector_to_buffer					; Check if our filename we want is in this sector

														; (XXX we scan the last BAM sector as well, to keep the code simple.)
														; filenames are at offset 4 in each 32-byte directory entry, padded at
														; the end with $A0
		lda #<fastload_sector_buffer
		sta fl_buffaddr+1
		lda #>fastload_sector_buffer
		sta fl_buffaddr+2

fl_check_logical_sector
		ldx #$05
fl_filenamecheckloop
		ldy #$00

fl_check_loop_inner

fl_buffaddr
		lda fastload_sector_buffer+$100,x

		cmp fastload_filename,y	
		bne fl_filename_differs
		inx
		iny
		cpy #$10
		bne fl_check_loop_inner

fl_found_file											; Filename matches
		txa
		sec
		sbc #$12
		tax
		lda fl_buffaddr+2
		cmp #>fastload_sector_buffer
		bne fl_file_in_2nd_logical_sector

		lda fastload_sector_buffer,x					; Y=Track, A=Sector
		tay
		lda fastload_sector_buffer+1,x
		jmp fl_got_file_track_and_sector

fl_file_in_2nd_logical_sector

		lda fastload_sector_buffer+$100,x				; Y=Track, A=Sector
		tay
		lda fastload_sector_buffer+$101,x

fl_got_file_track_and_sector
		sty fl_file_next_track							; Store track and sector of file
		sta fl_file_next_sector

		lda #3											; Advance to next state
		sta fastload_request

		jsr fl_read_next_sector							; Request reading of next track and sector
		rts

fl_filename_differs
		cpy #$10										; Skip same number of chars as though we had matched
		beq fl_end_of_name
		inx
		iny
		jmp fl_filename_differs
fl_end_of_name
		txa												; Advance to next directory entry
		clc
		adc #$10
		tax
		bcc fl_filenamecheckloop
		inc fl_buffaddr+2
		lda fl_buffaddr+2
		cmp #(>fastload_sector_buffer)+1
		bne fl_checked_both_halves
		jmp fl_check_logical_sector

fl_checked_both_halves
		inc $d085										; No matching name in this 512 byte sector.
		lda $d085										; Load the next one, or give up the search
		cmp #11
		bne fl_load_next_dir_sector
														; Ran out of sectors in directory track
														; (XXX only checks side 0, and assumes DD disk)

		lda #$80 										; Mark load as failed. $80 = File not found
		sta fastload_request

:		inc $d020
		jmp :-

		rts

fl_load_next_dir_sector
		jsr fl_read_sector								; Request read. No need to change state
		rts

fl_read_sector

		lda fastload_request							; Remember the state that we need to return to
		sta fastload_request_stashed

		lda #5											; and then set ourselves to the track stepping/sector reading state
		sta fastload_request
														; FALLTHROUGH
fl_reading_sector
		lda $d084										; Check if we are already on the correct track/side
		cmp fl_current_track							; and if not, select/step as required
		beq fl_on_correct_track
		bcc fl_step_in

fl_step_out
		lda #$18										; We need to step first
		sta $d081
		inc fl_current_track
		rts

fl_step_in
		lda #$10										; We need to step first
		sta $d081
		dec fl_current_track
		rts

fl_on_correct_track
		lda $d084
		cmp fl_prev_track
		bne fl_not_prev_sector
		lda $d086
		cmp fl_prev_side
		bne fl_not_prev_sector
		lda $d085
		cmp fl_prev_sector
		bne fl_not_prev_sector

		lda fastload_request_stashed					; We are being asked to read the sector we already have in the buffer
		sta fastload_request							; Jump immediately to the correct routine
		jmp fl_fdc_not_busy

fl_not_prev_sector
		lda #$40
		sta $d081

		lda fastload_request_stashed					; Now that we are finally reading the sector,
		sta fastload_request							; restore the stashed state ID

		rts

fl_step_track
		lda #3											; advance to next state
		sta fastload_request
														; FALL THROUGH

fl_read_next_sector
		lda fl_file_next_track							; Check if we reached the end of the file first
		bne fl_not_end_of_file
		rts

fl_not_end_of_file
		jsr fl_logical_to_physical_sector				; Read next sector of file	
		jsr fl_read_sector
		rts

fl_logical_to_physical_sector
		lda $d084										; Remember current loaded sector, so that we can optimise when asked
		sta fl_prev_track								; to read other half of same physical sector
		lda $d085
		sta fl_prev_sector
		lda $d086
		sta fl_prev_side
														; Convert 1581 sector numbers to physical ones on the disk.
		jsr fl_select_side0								; Side = 0
		lda fl_file_next_track
		dec												; Track = Track - 1
		sta $d084

		lda fl_file_next_sector							; Sector = 1 + (Sector/2)
		lsr
		inc
		cmp #11											; If sector > 10, then sector=sector-10, side=1
		bcs fl_on_second_side							; but sides are inverted
		sta $d085

		rts

fl_on_second_side
		sec
		sbc #10
		sta $d085
		jsr fl_select_side1
		rts

fl_read_file_block
														; We have a sector from the floppy drive.
														; Work out which half and how many bytes, and copy them into place.

		jsr fl_copy_sector_to_buffer					; Get sector from FDC

		lda #254										; Assume full sector initially
		sta fl_bytes_to_copy

		lda fl_file_next_sector							; Work out which half we care about
		and #$01

		bne fl_read_from_second_half

fl_read_from_first_half
		lda #(>fastload_sector_buffer)+0
		sta fl_read_page+1
		lda fastload_sector_buffer+1
		sta fl_file_next_sector
		lda fastload_sector_buffer+0
		sta fl_file_next_track
		bne fl_1st_half_full_sector

fl_1st_half_partial_sector
		lda fastload_sector_buffer+1
		sta fl_bytes_to_copy	

		lda #$00										; Mark end of loading
		sta fastload_request

fl_1st_half_full_sector
		jmp fl_dma_read_bytes

fl_read_from_second_half
		lda #(>fastload_sector_buffer)+1
		sta fl_read_page+1
		lda fastload_sector_buffer+$101
		sta fl_file_next_sector
		lda fastload_sector_buffer+$100
		sta fl_file_next_track
		bne fl_2nd_half_full_sector

fl_2nd_half_partial_sector
		lda fastload_sector_buffer+$101
		sta fl_bytes_to_copy

		lda #$00										; Mark end of loading
		sta fastload_request

fl_2nd_half_full_sector
														; FALLTHROUGH

fl_dma_read_bytes
		lda fastload_address+3							; Update destination address
		asl
		asl
		asl
		asl
		sta fl_data_read_dmalist+2
		lda fastload_address+2
		lsr
		lsr
		lsr
		lsr
		ora fl_data_read_dmalist+2
		sta fl_data_read_dmalist+2
		lda fastload_address+2
		and #$0f
		sta fl_data_read_dmalist+12
		lda fastload_address+1
		sta fl_data_read_dmalist+11
		lda fastload_address+0
		sta fl_data_read_dmalist+10

		lda #$00										; Copy FDC data to our buffer
		sta $d704
		lda #>fl_data_read_dmalist
		sta $d701
		lda #<fl_data_read_dmalist
		sta $d705

		clc
		lda fastload_address+0							; Update load address
		adc fl_bytes_to_copy
		sta fastload_address+0
		lda fastload_address+1
		adc #0
		sta fastload_address+1
		lda fastload_address+2
		adc #0
		sta fastload_address+2
		lda fastload_address+3
		adc #0
		sta fastload_address+3

		jsr fl_read_next_sector							; Schedule reading of next block

		rts

fl_data_read_dmalist
		.byte $0b										; F011A type list
		.byte $81,$00									; Destination MB
		.byte 0											; no more options
		.byte 0											; copy
fl_bytes_to_copy
		.word 0											; size of copy
fl_read_page
		.word fastload_sector_buffer+2					; Source address. +2 is to skip track/header link
		.byte $00										; Source bank
		.word 0											; Dest address
		.byte $00										; Dest bank
		.byte $00										; sub-command
		.word 0											; modulo (unused)
		rts

fl_copy_sector_to_buffer
		lda #$80										; Make sure FDC sector buffer is selected
		trb $d689
		lda #$00										; Copy FDC data to our buffer
		sta $d704
		lda #>fl_sector_read_dmalist
		sta $d701
		lda #<fl_sector_read_dmalist
		sta $d705
		rts

fl_sector_read_dmalist
		.byte $0b	 									; F011A type list
		.byte $80,$ff									; MB of FDC sector buffer address ($FFD6C00)
		.byte 0											; no more options
		.byte 0											; copy
		.word 512										; size of copy
		.word $6c00										; low 16 bits of FDC sector buffer address
		.byte $0d										; next 4 bits of FDC sector buffer address
		.word fastload_sector_buffer					; Dest address	
		.byte $00										; Dest bank
		.byte $00										; sub-command
		.word 0											; modulo (unused)