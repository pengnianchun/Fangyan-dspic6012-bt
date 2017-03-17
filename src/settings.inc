;------------------------------------------------------------------------------
;
; Title:			ds30 Loader for dsPIC30F
;                                                                             
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
; Device
;------------------------------------------------------------------------------

	.include	"xc.inc"
	.equ		VALID_DEV,		1
	.equ		FLASHSIZE,		0x18000
	.equ		DEVICEID,		5
	.equ		HAS_UART2,		1
	.equ		HAS_CAN1,		1
	.equ		HAS_CAN2,		1



;------------------------------------------------------------------------------
; User preferences
;------------------------------------------------------------------------------
		;.equ	DEV_MODE,	1					;xxx comment or delete this line
		
		.equ 	FCY, 		16000000			;xxx speed of internal cycle clock[Hz], used to calculate uart brg and delay
		.equ	BLTIME,		200				;xxx time in ms before bootloader loads the user application
     
        .equ	USE_CAN1,	1					;xxx uncomment to use CAN1 
        ;.equ	USE_CAN2,	1					;xxx uncomment to use CAN2, not implemented	
		
;------------------------------------------------------------------------------
; CAN settings
;------------------------------------------------------------------------------
; Baudrate table, not confirmed
;
;	     	   BRP    PPROP	   SEG1	   SEG2
;	250k		1		 7	     6		 1
;	125k	    2		 7		 6		 1
;
;------------------------------------------------------------------------------		
		.equ	ID_PIC,			4				;xxx node number for this device
		.equ 	ID_GUI, 		127				;xxx node number of the ds30 Loader gui
		
		.equ	CAN_BRP,		2				;xxx baud rate prescalar, should be 1-64
		.equ	CAN_PROP,		2				;xxx propagation, should be 1-8
		.equ	CAN_SEG1,		7				;xxx segment 1, should be 1-8
		.equ	CAN_SEG2,		6				;xxx segment 2, should be 1-8, should be less than prop+seg1
		.equ	CAN_SJW,		1				;xxx sjw, should be 1-4
		
				
;------------------------------------------------------------------------------
; Advanced settings
;------------------------------------------------------------------------------		
		.equ	PROT_GOTO,		1				;xxx protect goto at 0x00
		.equ 	PROT_BL, 		1				;xxx protect bootloader
		
		.equ	BLPLR,			8				;bootloader placement, rows from end
		.equ	BLSIZER,		8				;bootloader size [rows]
		

                ;.equ   LOW_ADDR_WORD,    0xFFFE
                ;.equ   HIGH_ADDR_WORD,   0x007F
		
;------------------------------------------------------------------------------
; Configuration bits, these macros can be found at the end of the inc-files located in 
;------------------------------------------------------------------------------
		config 	__FOSC, 	CSW_FSCM_OFF & XT_PLL8	;xxx oscillator
		;config 	__FWDT, 	WDT_OFF						;xxx watchdog
		;config 	__FBORPOR,	PBOR_OFF & MCLR_EN			;xxx power-on reset, brown-out reset, master clear
		;config 	__FGS, 		CODE_PROT_OFF 				;xxx code protection
		;config 	__FICD, 	ICS_NONE					;xxx comm channel select
