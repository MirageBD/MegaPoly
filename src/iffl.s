; -----------------------------------------------------------------------------------------------

iffl_unpackaddress
	.dword 0

; -----------------------------------------------------------------------------------------------

iffl_loadanddecrunchnextfile

			lda fl_iffl_currentfile
			asl
			asl
			asl
			tax
			clc

			lda fastload_iffl_start_address_and_size+0,x
			adc iffl_unpackaddress+0
			sta fastload_address+0
			sta dc_get_zp+0											; set address at which decruncher starts reading
			lda fastload_iffl_start_address_and_size+1,x
			adc iffl_unpackaddress+1
			sta fastload_address+1
			sta dc_get_zp+1
			lda fastload_iffl_start_address_and_size+2,x
			adc iffl_unpackaddress+2
			sta fastload_address+2
			sta dc_get_zp+2
			lda fastload_iffl_start_address_and_size+3,x
			adc iffl_unpackaddress+3
			sta fastload_address+3
			sta dc_get_zp+3

			lda fastload_iffl_start_address_and_size+4,x
			sta fl_iffl_sizeremaining+0
			lda fastload_iffl_start_address_and_size+5,x
			sta fl_iffl_sizeremaining+1
			lda fastload_iffl_start_address_and_size+6,x
			sta fl_iffl_sizeremaining+2
			lda fastload_iffl_start_address_and_size+7,x
			sta fl_iffl_sizeremaining+3

			lda #$07
			sta fastload_request
			inc fl_iffl_currentfile
			jsr fl_waiting
			jsr decrunch_readstartaddress

			lda iffl_unpackaddress+0						; overwrite start address that was just read (00000000)
			sta dc_ldst+0									; by start address specified in calling argument
			sta dc_mdst+0
			lda iffl_unpackaddress+1
			sta dc_ldst+1
			sta dc_mdst+1
			lda iffl_unpackaddress+2
			sta dc_ldst+2
			sta dc_mdst+2
			lda iffl_unpackaddress+3
			asl
			asl
			asl
			asl
			sta dc_lsrcm+1
			sta dc_msrcm+1
			sta dc_ldstm+1
			sta dc_mdstm+1

			jsr decrunch_dowork

			rts

; -----------------------------------------------------------------------------------------------            