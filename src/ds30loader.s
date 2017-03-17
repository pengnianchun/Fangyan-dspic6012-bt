
;------------------------------------------------------------------------------
; Registers usage
;------------------------------------------------------------------------------
		;.equ	MIXED,		W0		;immediate
		;.equ	MIXED,		W1		;immediate
		.equ	WBUFPTR,	W2		;buffer pointer
		.equ	WCNT,		W3		;loop counter
		.equ	WADDR2,		W4		;memory pointer
		.equ	WADDR,		W5		;memory pointer
		;.equ	UNUSED,		W6		;
		;.equ	UNUSED,		W7		;
		.equ	WTEMP1,		W8		;
		.equ	WTEMP2,		W9		;
		.equ	WDEL1,		W10		;delay outer
		.equ	WDEL2,		W11		;delay inner
		;.equ	UNUSED,		W12		;
		.equ	WCMD,		W13		;command
		.equ 	WCRC, 		W14		;checksum
		.equ	WSTPTR,		W15		;stack pointer


;------------------------------------------------------------------------------
; Includes
;------------------------------------------------------------------------------
		.include "settings.inc"
;------------------------------------------------------------------------------
; CAN stuff
;------------------------------------------------------------------------------
		.ifdef USE_CAN1
			.ifdef USE_CAN2
				.error "Both CAN ports are specified"
			.endif

			.ifndef HAS_CAN1
				.error "CAN is specified for a device that don't have CAN"
			.endif

			.equ	USE_CAN,	1

			.equ	CiCFG1, 	C1CFG1
			.equ	CiCFG2, 	C1CFG2
			.equ	CiCTRL, 	C1CTRL
			.equ	CiRX0CON, 	C1RX0CON
			.equ	CiRXM0SID, 	C1RXM0SID
			.equ	CiRXF0SID, 	C1RXF0SID
			.equ	CiRXF1SID, 	C1RXF1SID
			.equ	CiRX1CON, 	C1RX1CON
			.equ	CiRXM1SID, 	C1RXM1SID
			.equ	CiRXF2SID, 	C1RXF2SID
			.equ	CiRXF3SID, 	C1RXF3SID
			.equ	CiRXF4SID, 	C1RXF4SID
			.equ	CiRXF5SID, 	C1RXF5SID

			.equ	CiTX0B1,	C1TX0B1
			.equ	CiTX0SID,	C1TX0SID
			.equ	CiCTRLH,	C1CTRLH
			.equ	CiCTRLL,	C1CTRLL
			.equ	CiTX0DLC,	C1TX0DLC
			.equ	CiTX0CON,	C1TX0CON
			.equ	CiINTF,		C1INTF
			.equ	CiRX0DLC,	C1RX0DLC
			.equ	CiRX0CON,	C1RX0CON
			.equ	CiRX0B1,	C1RX0B1
		.endif

		.ifdef USE_CAN2
			.ifndef HAS_CAN2
				.error "CAN2 specified for a device that don't have CAN2"
			.endif

			.equ	USE_CAN,	1

			.equ	CiCFG1, 	C2CFG1
			.equ	CiCFG2, 	C2CFG2
			.equ	CiCTRL, 	C2CTRL
			.equ	CiRX0CON, 	C2RX0CON
			.equ	CiRXM0SID, 	C2RXM0SID
			.equ	CiRXF0SID, 	C2RXF0SID
			.equ	CiRXF1SID, 	C2RXF1SID
			.equ	CiRX1CON, 	C2RX1CON
			.equ	CiRXM1SID, 	C2RXM1SID
			.equ	CiRXF2SID, 	C2RXF2SID
			.equ	CiRXF3SID, 	C2RXF3SID
			.equ	CiRXF4SID, 	C2RXF4SID
			.equ	CiRXF5SID, 	C2RXF5SID

			.equ	CiTX0B1,	C2TX0B1
			.equ	CiTX0SID,	C2TX0SID
			.equ	CiCTRLH,	C2CTRLH
			.equ	CiCTRLL,	C2TRLL
			.equ	CiTX0DLC,	C2TX0DLC
			.equ	CiTX0CON,	C2TX0CON
			.equ	CiINTF,		C2INTF
			.equ	CiRX0DLC,	C2RX0DLC
			.equ	CiRX0CON,	C2RX0CON
			.equ	CiRX0B1,	C2RX0B1
		.endif


;------------------------------------------------------------------------------
; Constants, don? change
;------------------------------------------------------------------------------
		.equ	VERMAJ,		3										/*firmware version major*/
		.equ	VERMIN,		0										/*firmware version minor*/
		.equ	VERREV,		0										/*firmware version revision*/

		.equ 	HELLO, 		0xC1
		.equ 	OK, 		'K'										/*erase/write ok*/
		.equ 	CHECKSUMERR,'N'										/*checksum error*/
		.equ	VERFAIL,	'V'										/*verification failed*/
		.equ   	BLPROT,     'P'                              		/*bl protection tripped*/
		.equ   	UCMD,     	'U'                              		/*unknown command*/

		.equ	BLDELAY,	( BLTIME * (FCY / 1000) / (65536 * 7) )	/*delay before user application is loaded*/
		
		.equ	PAGESIZER,	1										/*pagesize [rows]*/
		.equ	ROWSIZEW,	32										/*rowsize [words]*/
		.equ	STARTADDR,	( FLASHSIZE - BLPLR * PAGESIZER * ROWSIZEW * 2 )	/*bootloader placement*/
		.equ	BLSTARTROW,	(STARTADDR / ROWSIZEW / 2)
		.equ	BLENDROW,	(BLSTARTROW + BLSIZER - 1)


		.ifdef USE_CAN
			.equ	ECAN_MODE_NORMAL,			0x00
			.equ	ECAN_MODE_CONFIG,			0x04
		.endif

;------------------------------------------------------------------------------
; Global declarations
;------------------------------------------------------------------------------
        .global	__reset          	;the label for the first line of code, needed by the linker script

;------------------------------------------------------------------------------
; Uninitialized variables in data memory
;------------------------------------------------------------------------------
 		.bss	ptrRead, ( 2 )
		.bss	ptrWrite, ( 2 )
		.bss	iUnread, ( 2 )
		.bss	rxBuffer, ( 128 )
		.bss	buffer, ( ROWSIZEW * 3 + 1/*checksum*/ )


;------------------------------------------------------------------------------
; Send macro
;------------------------------------------------------------------------------
		.macro SendL char
			mov 	#\char, W0
			rcall Send
		.endm

		.ifdef USE_CAN
			.macro SetCANMode char
				mov 	#\char, W1
				rcall SetCANMode_
			.endm
		.endif

;------------------------------------------------------------------------------
; Start of code section in program memory
;------------------------------------------------------------------------------
		.section *, code,address(STARTADDR-4)
usrapp:	goto 0x100						;these two instructions will be replaced
		                      		     ; with a goto to the user app. by the pc program


;------------------------------------------------------------------------------
; Reset vector
;------------------------------------------------------------------------------
		.section *, code,address(STARTADDR)
__reset:mov 	#__SP_init, WSTPTR	;initalize the Stack Pointer

;------------------------------------------------------------------------------
; Init CAN
;------------------------------------------------------------------------------
		.ifdef USE_CAN
			;----------------------------------------------------------------------
			; Set CAN mode: configuration
			;----------------------------------------------------------------------
			SetCANMode	ECAN_MODE_CONFIG
			;----------------------------------------------------------------------
			; CAN module
			;----------------------------------------------------------------------

			; Baud rate prescalar
			; SJW
			mov		#( (CAN_BRP-1) + ((CAN_SJW-1)<<6) ), W0
			mov		W0, CiCFG1

			; Propagation
			; Segment 1
			;SJW
			; Segment2
			mov		#( (CAN_PROP-1) + ((CAN_SEG1-1)<<3) + (1<<6)/*SAM*/  + (1<<7)/*SEG2PHTS*/ + ((CAN_SEG2-1)<<8) ), W0
			mov		W0, CiCFG2

		    ;CAN control and status register
		    bset	CiCTRL, #CANCKS		;Fcan clock is Fcy
		    bset	CiCTRL, #ABAT		;abort pending transmissions in all TX buffers, clears TXREQ in tx buffer control registers

		    ; Clear interupt flags
		    clr		CiINTF


			;----------------------------------------------------------------------
			; CAN tx
			;----------------------------------------------------------------------
			; CAN transmit 0 SID
			;bset	CiTX0SID, #TXIDE		;extended identifier


			;----------------------------------------------------------------------
			; CAN rx buffer 0
			;----------------------------------------------------------------------
			; Receive buffer 0 control and status register
			;bset	CiRX0CON, #DBEN			;receive buffer 0 overflow will write to Receive Buffer 1
			bclr	CiRX0CON, #RXFUL		;receive full status

			; Acceptance mask 0 SID/EID
			setm	CiRXM0SID
			;bclr	CiRXM0SID, #MIDE		;0=match either standard or extended address message if the filters match
			;clr		CiRXM0EIDH
			;clr		CiRXM0EIDL
            
           ; BCLR 0x2CD, #5 ;CAN_WK_DR=0;
	   ; BCLR 0x2D1, #5 ;CAN_WK=0;
	    
            ;mov #LOW_ADDR_WORD,W0
            ;mov #HIGH_ADDR_WORD,W1
            ;mov W1,TBLPAG
            ;tblrdl [W0],W0

            ;sl W0,#2,W1
            ;mov #0x1FFC,W0
            ;and W1,W0,W0
            ;mov W0,CiRXF0SID

			; Acceptance filter  0 SID/EID
			mov		#( (ID_PIC<<2) & 0x1FFC), W0
			mov		W0, CiRXF0SID
			;bset	CiRXF0SID, #EXIDE		;1=enable filter for extended identifier
			;clr		CiRXF0EIDH
			;clr		CiRXF0EIDL

			; Acceptance filter  1 SID/EID, not used
			clr 	CiRXF1SID
			;bset	CiRXF1SID, #EXIDE		;1=enable filter for extended identifier
			;clr		CiRXF1EIDH
			;clr		CiRXF1EIDL


			;----------------------------------------------------------------------
			; CAN rx buffer 1
			;----------------------------------------------------------------------
			; Receive buffer 1 control and status register
			bclr	CiRX1CON, #RXFUL		;receive full status

			; Acceptance mask 1 SID/EID
			setm	CiRXM1SID
			;bclr	CiRXM1SID, #MIDE		;0=match either standard or extended address message if the filters match
			;clr		CiRXM1EIDH
			;clr		CiRXM1EIDL

			; Acceptance filter  2 SID/EID
			clr		CiRXF2SID
			;bset	CiRXF2SID, #EXIDE		;1=enable filter for extended identifier
			;clr		CiRXF2EIDH
			;clr		CiRXF2EIDL

			; Acceptance filter  3 SID/EID, not used
			clr 	CiRXF3SID
			;bset	CiRXF3SID, #EXIDE		;1=enable filter for extended identifier
			;clr		CiRXF3EIDH
			;clr		CiRXF3EIDL

			; Acceptance filter  4 SID/EID, not used
			clr 	CiRXF4SID
			;bset	CiRXF4SID, #EXIDE		;1=enable filter for extended identifier
			;clr		CiRXF4EIDH
			;clr		CiRXF4EIDL

			; Acceptance filter  5 SID/EID, not used
			clr 	CiRXF5SID
			;bset	CiRXF5SID, #EXIDE		;1=enable filter for extended identifier
			;clr		CiRXF5EIDH
			;clr		CiRXF5EIDL


			;----------------------------------------------------------------------
			; Set CAN mode: normal
			;----------------------------------------------------------------------
			SetCANMode ECAN_MODE_NORMAL


			;----------------------------------------------------------------------
			; Init can
			;----------------------------------------------------------------------
			mov		#rxBuffer, W0
			mov		W0, ptrRead
			mov		W0, ptrWrite
			clr		iUnread
		.endif


;------------------------------------------------------------------------------
; Receive hello
;------------------------------------------------------------------------------
		rcall 	Receive
		sub 	#HELLO, W0
		bra 	nz, exit			;unknown prompt


;------------------------------------------------------------------------------
; Send device id and firmware version
;------------------------------------------------------------------------------
		SendL 	DEVICEID
		SendL	VERMAJ
		SendL	(VERMIN*16 + VERREV)


;------------------------------------------------------------------------------
; Main
;------------------------------------------------------------------------------
Main:	SendL 	OK

		; Init checksum
main1:	clr 	WCRC

		; Init CAN rx buffer
		.ifdef USE_CAN
			mov		#rxBuffer, W0
			mov		W0, ptrRead
			mov		W0, ptrWrite
			;clr		iUnread		;should allready be 0 when we get here, error condition if not
		.endif


		;----------------------------------------------------------------------
		; Receive address
		;----------------------------------------------------------------------
		; Upper
		rcall 	Receive
		mov 	W0, NVMADRU
		mov 	W0, TBLPAG
		; High
		rcall 	Receive
		mov.b 	WREG, NVMADR+1
		; Low
		rcall 	Receive
		mov.b 	WREG, NVMADR
		;
		mov		NVMADR, WREG
		mov		W0, WADDR
		mov		W0, WADDR2


		;----------------------------------------------------------------------
		; Receive command
		;----------------------------------------------------------------------
		rcall 	Receive
		mov		W0, WCMD


		;----------------------------------------------------------------------
		; Receive nr of data bytes that will follow
		;----------------------------------------------------------------------
		rcall 	Receive
		mov 	W0, WCNT


		;----------------------------------------------------------------------
		; Receive data
		;----------------------------------------------------------------------
		mov 	#buffer, WBUFPTR
rcvdata:rcall 	Receive
		mov.b 	W0, [WBUFPTR++]
		dec		WCNT, WCNT
		bra 	nz, rcvdata			;last byte received is checksum


		;----------------------------------------------------------------------
		; 0x00 goto protection
		;----------------------------------------------------------------------
		.ifdef	PROT_GOTO
			cp0		NVMADRU
			bra		nz, chksum
			cp0		NVMADR
			bra		nz, chksum

			;
			mov 	#buffer, WBUFPTR
			; 1st word upper byte = goto instruction
			mov.b 	#0x04, W0
			mov.b	W0, [WBUFPTR++]
			; 1st word  low byte = low address byte
			mov.b 	#(0xff & STARTADDR), W0
			mov.b 	W0, [WBUFPTR++]
			; 1st word high byte = high address byte
			mov.b 	#(0xff & (STARTADDR>>8)), W0
			mov.b 	W0, [WBUFPTR++]
			;2nd word upper byte = unused
			clr.b	[WBUFPTR++]
			; 2nd word  low byte = upper address byte
			mov.b 	#(0xff & (STARTADDR>>16)), W0
			mov.b 	W0, [WBUFPTR++]
			; 2nd word high byte = unused
			clr.b 	[WBUFPTR++]
		.endif


		;----------------------------------------------------------------------
		; Check checksum
		;----------------------------------------------------------------------
chksum:	cp0.b 	WCRC
		bra 	z, ptrinit
		SendL 	CHECKSUMERR
		bra 	main1


		;----------------------------------------------------------------------
		; Init pointer
		;----------------------------------------------------------------------
ptrinit:mov 	#buffer, WBUFPTR

		;----------------------------------------------------------------------
		; Check command
		;----------------------------------------------------------------------
		; Write row
		btsc	WCMD, #1
		bra		blprot
		; Write EEROM word
		btsc	WCMD, #2
		bra		eeprom
		; Write Config word
		btsc	WCMD, #3
		bra		config
		; Else, unknown command
		SendL   UCMD
		bra     main1


		;----------------------------------------------------------------------
		; Bootloader protection
		;----------------------------------------------------------------------
blprot:	nop
		.ifdef PROT_BL
			; Calculate row number of received address
			mov		TBLPAG, W1
			mov		WADDR, W0
			mov		#(ROWSIZEW*2), WTEMP1
			repeat	#17
			div.ud	W0, WTEMP1;		W = received address / (rowsizew*2)
			; Received row < bl start row = OK
			mov		#BLSTARTROW, WTEMP1
			cp		W0, WTEMP1
			bra		N, blprotok
			; Received row > bl end row = OK
			mov		#BLENDROW, WTEMP1
			cp		WTEMP1, W0
			bra		N, blprotok
			; Protection tripped
			SendL   BLPROT
		    bra     main1
			; Restore WADDR2
blprotok:	mov		WADDR, WADDR2
		.endif


		;----------------------------------------------------------------------
		; Erase & write row
		;----------------------------------------------------------------------
		; Erase row
errow:	mov 	#0x4041, W0
		rcall 	Write
		; Load latches
		mov 	#ROWSIZEW, WCNT
latlo:	tblwth.b 	[WBUFPTR++], [WADDR] 	;upper byte
		tblwtl.b	[WBUFPTR++], [WADDR++] 	;low byte
		tblwtl.b	[WBUFPTR++], [WADDR++] 	;high byte
		dec 	WCNT, WCNT
		bra 	nz, latlo
		; Write
		mov 	#0x4001, W0
		rcall	Write


		;----------------------------------------------------------------------
		; Verify row
		;----------------------------------------------------------------------
		mov 	#ROWSIZEW, WCNT
		mov 	#buffer, WBUFPTR
		mov		WADDR2, W0
		mov		WREG, NVMADR
		; Verify upper byte
verloop:tblrdh.b [WADDR2], W0
		cp.b	W0, [WBUFPTR++]
		bra		NZ, vfail
		; Verify low byte
		tblrdl.b [WADDR2++], W0
		cp.b	W0, [WBUFPTR++]
		bra		NZ, vfail
		; Verify high byte
		tblrdl.b [WADDR2++], W0
		cp.b	W0, [WBUFPTR++]
		bra		NZ, vfail
		; Loop
		dec		WCNT, WCNT
		bra 	nz, verloop
		; Verify finished
		bra		Main


		;----------------------------------------------------------------------
		; Erase, write & verify eeprom word
		;----------------------------------------------------------------------
		;Erase eeprom word
eeprom:	mov 	#0x4044, W0
		rcall 	Write
		; Load latch
		tblwtl 	[WBUFPTR], [WADDR]
		; Write eeprom word
		mov 	#0x4004, W0
		rcall 	Write
		; Verify eeprom word
		tblrdl	[WADDR], W0
		cp		W0, [WBUFPTR]
		bra		Z, Main
		; Else verify fail (below)


		;----------------------------------------------------------------------
		; Verify fail
		;----------------------------------------------------------------------
vfail:	SendL	VERFAIL
		bra		main1


		;----------------------------------------------------------------------
		; Write config word, does not need erase
		;----------------------------------------------------------------------
		; Load latch
config:	tblwtl 	[WBUFPTR], [WADDR]
		; Write config word
		mov 	#0x4008, W0
		rcall 	Write
		; Write finished
		bra		Main


;------------------------------------------------------------------------------
; SetCANMode()
; Arg: mode in W1
;------------------------------------------------------------------------------
.ifdef USE_CAN
SetCANMode_:
		mov		#0xF8, W0
		and.b	CiCTRLH
		mov		W1, W0
		ior.b	CiCTRLH
scm:	mov.b	CiCTRLH, WREG
		mov		W0, W1
		and		#7, W1
		mov.b	CiCTRLL, WREG
		lsr		W0, #5, W0
		and		#7, W0
		cp		W0, W1
		bra		nz, scm
		return
.endif



;------------------------------------------------------------------------------
; Write()
;------------------------------------------------------------------------------
Write:	mov 	W0, NVMCON
		mov 	#0x55, W0
		mov 	W0, NVMKEY
		mov 	#0xAA, W0
		mov 	W0, NVMKEY
		bset 	NVMCON, #WR
		nop
		nop
		; Wait for erase/write to finish
compl:	btsc	NVMCON, #WR
		bra 	compl
		return



;------------------------------------------------------------------------------
; Send()
; Arg: data in W0
;------------------------------------------------------------------------------
.ifdef USE_CAN
Send:	; Data
		mov		W0, CiTX0B1
		; Standard identifier
		mov		#( ((ID_GUI<<2)&0xfc) + ((ID_GUI<<5)&0xf800) ), W0
		mov		W0, CiTX0SID
		; Standard identifier bits 0-5
		;clr		CiTX0SID
		;mov		#NODENR_BL, W0
		;sl		W0, #2, W0
		;and		#0xFC, W0
		;mov		W0, CiTX0SID
		; Standard identifier bits 10-6
		;mov		#NODENR_BL, W0
		;sl		W0, #5, W0
		;mov		#0xF800, W1
		;and		W0, W1, W0
		;ior		CiTX0SID
		; Extended identifier
		;mov		#0, W0
		;mov		W0, CiTX0EID
		; Data length code, 1 byte
		clr		CiTX0DLC
		bset	CiTX0DLC, #3
		; Request message transmission
		bset	CiTX0CON, #TXREQ
		; Verify successfull transmission
sendw:	btsc	CiTX0CON, #TXREQ
		bra		sendw
		return
.endif


;------------------------------------------------------------------------------
; Receive()
;------------------------------------------------------------------------------

.ifdef USE_CAN
;------------------------------------------------------------------------------
; Receive(), return data from buffer
; Return: data in W0
;------------------------------------------------------------------------------
Receive:rcall	CheckRx
		; Data not available in rx buffer?
		cp0		iUnread
		bra		z, rcv
received:
		mov		ptrRead, W1
		mov.b	[W1], W0
		and		#0xFF, W0
		inc		ptrRead
		dec		iUnread
		return


;----------------------------------------------------------------------
; CheckRx()
;----------------------------------------------------------------------
CheckRx:btss 	CiINTF, #RX0IF
		return

		; Move data from rx0 to buffer
rx0buf:	mov		#CiRX0B1, W0
		mov		CiRX0DLC, WTEMP1
		and		#0xF, WTEMP1
		mov		ptrWrite, W1
		dec		WTEMP1, WTEMP1
		; Loop
		do		WTEMP1, movbyte
		;clr		WTEMP2
		add.b 	WCRC, [W0], WCRC		;add to checksum
		inc		iUnread
movbyte:mov.b	[W0++], [W1++]
		; Save write pointer
		mov		W1, ptrWrite;

		bclr	CiINTF, #RX0IF
		bclr	CiRX0CON, #RXFUL
		return


;------------------------------------------------------------------------------
; Receive, wait for data from CAN bus
;------------------------------------------------------------------------------
		; Init delay
rcv:	mov 	#BLDELAY, WDEL1
		; Check for received byte
rpt1:	clr		WDEL2
rptc:	clrwdt						;clear watchdog
		rcall	CheckRx
		cp0		iUnread
		bra 	nz, received

		;----------------------------------------------------------------------
 		; Delay
 		;----------------------------------------------------------------------
		dec 	WDEL2, WDEL2
		bra 	nz, rptc
		dec 	WDEL1, WDEL1
		bra 	nz, rpt1
		; If we get here, receive timed out
        mov 	#__SP_init, WSTPTR	;reinitialize the Stack Pointer
.endif


;------------------------------------------------------------------------------
; Exit point, clean up and load user application
;------------------------------------------------------------------------------
exit:
        bra 	usrapp				;change to jump code for first address


;------------------------------------------------------------------------------
; End of code
;------------------------------------------------------------------------------
.end
