;***********************************************************
;*
;*	Wei_Huang_Final_Project.asm
;*
;*	Enter the description of the program here
;*
;*	This is the skeleton file for the ECE 375 final project
;*
;***********************************************************
;*
;*	 Author: Huang Wei
;*	 Date: 12/2/2020 4:09:24 PM
;*
;***********************************************************
.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;*	(feel free to edit these or add others)
;***********************************************************
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def    A = r7
.def	mpr = r16				; Multipurpose register 
.def	LETTER = r23		; LETTER Counter
.def	BIT = r24				; BIT Counter
.def	dataptr = r25			; data ptr


.equ	EngDirR = 5				; right Engine Direction Bit
.equ	EngDirL = 6				; left Engine Direction Bit


.equ    FIRST = 0				;INIT 0
.equ	Forward = 6				;INIT 6
.equ	REVERSE = 7				;INIT7
.equ	ACTVIE = 4				;INIT4
.equ    NONE = 5				;INIT5
.equ    WTime = 15
.equ	SpeedMIN = 0


;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
.dseg
.org	$0200						; data memory allocation for operands
operand1:		.byte 2				; allocate 2 bytes for a variable named operand1
.org	$0300
operand2:		.byte 26

; Important Reminder:
; The LCD driver expects its display data to be arranged as follows:
; - Line 1 data is in address space $0100-$010F
; - Line 2 data is in address space $0110-$010F

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment
;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp  INIT				; Reset interrupt
.org	$0046					; End of Interrupt Vectors
;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:	; The initialization routine
	clr  zero
	; To do
	; your code goes in this area
	   ; Initialize Stack Pointer
    ldi mpr, low(RAMEND)	; Load low byte of end SRAM address into mpr
    out SPL, mpr			; Write byte to SPL
    ldi mpr, high(RAMEND)	;Load high byte of end SRAM address into mpr
    out SPH, mpr			; Write byte to SPH
	
	; Configure I/O ports
	;configure port B
	ldi mpr, $FF					;set port B data direction register
	out DDRB, mpr					;for output
	ldi mpr, $00					; initialize port B data register
	out PORTB, mpr					; so all port B outputs are low

    ; Initialize port D for input
    ldi mpr, $00	;Set port D Data Direcrion register
    out DDRD, mpr   ; for output
    ldi mpr,  $FF	; Initialize Port D Data register
    out PORTD, mpr  ; so all Port D inputs are Tri-state
    
    ; Initialize LCD Display
    rcall LCDInit
	;initialize	the begin words show on the LCD display 
	rcall PROGRAM_START		
	;rcall TIME		
	ldi BIT, -1						; define the bit
	ldi LETTER, 65					;define A to letter
	ldi type, 0


	
;-----------------------------------------------------------
; Main procedure
;-----------------------------------------------------------
MAIN:
	; more code
	; you will probably have an infinite loop in your code that handles input from the user
	in mpr, PIND		;get the whisker input from port D
	sbrs mpr, NONE
	breq MAIN

	sbrs mpr, FIRST		;Skip if Bit in Register First Set
	rcall INIT0_STEP	;call function

	sbrs mpr, Forward	;Skip if Bit in Register Forware Set
	rcall HIT_FORWARD	;call function

	sbrs mpr, REVERSE		;Skip if Bit in Register REVERSE Set
	rcall HIT_REVERSE	;call function

	sbrs mpr, ACTVIE	;skip if bit in register ACTIVE
	rcall PIND_4		;CALL FUNCTION

	rjmp	MAIN		;jump back to MAin to create infinite loop


;***********************************************************
;*	Procedures and Subroutines
;***********************************************************
; your code can go here as well
;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: LoadLine
; Desc: load the stirng from Stored Program Data to
;		the LCD display
;-----------------------------------------------------------
LoadLine:
	;push save register
	push mpr		;save mpr
	in mpr, SREG	;save status register
	push mpr		;
	push ZL			;save Z register
	push ZH			;
	push YL			;save Y register 
	push YH			;
	
;	clr count
LOADSTRINGLOOP:
	lpm mpr, Z+     ; load byte from Z point to next byte
	st Y+, mpr      ; store byte into memory and incremeny 
	cp ZL, XL       ; check the low byte match
	brne LOADSTRINGLOOP ; not end of the line, continue
	cp ZH, XH       ; check the high byte match
	brne LOADSTRINGLOOP ; not end of the line, continue

	;pop restore
	pop YH			;restore Y register
	pop YL
	pop ZH			;restore Z register 
	pop ZL
	pop mpr			;restore mpr
	out SREG, mpr	;restore the status register	
	pop mpr
	ret				;end the function

;-----------------------------------------------------------
; Func: PROGRAM_START
; Desc: set the stirng shows on the LCD display begin of the
;		program
;-----------------------------------------------------------
PROGRAM_START:
	;push save
	push XL		;save X register
	push XH		;
	push YL		;save Y register
	push YH		;
	push ZL		;save Z register
	push ZH		;
	
	ldi XL,low (STRING_END1<<1)		;load the low byte of the end of string
	ldi XH, high(STRING_END1<<1)	;load the high byte of the end of string

	ldi YL, low(LCDLn1Addr)			;load the low byte of the LCDLn1Addr
	ldi YH, high(LCDLn1Addr)		;load the high byte of the LCDLn1Addr

	ldi ZL, low(STRING_LINE1<<1)	;load the low byte of the begin of string
	ldi ZH, high(STRING_LINE1<<1)	;load the high byte of the begin of string
	rcall LoadLine


	ldi XL,low (STRING_END2<<1)		;load the low byte of the end of string
	ldi XH, high(STRING_END2<<1)	;load the high byte of the end of string

	ldi YL, low(LCDLn2Addr)			;load the low byte of the LCDLn2Addr
	ldi YH, high(LCDLn2Addr)		;load the high byte of the LCDLn2Addr

	ldi ZL, low(STRING_LINE2<<1)	;load the low byte of the begin of string
	ldi ZH, high(STRING_LINE2<<1)	;load the high byte of the begin of string
	rcall LoadLine					;call function

	rcall LCDWrite					;call function to write word to LDC

	;pop restore register
	pop ZH	;retsore Z register
	pop ZL	;
	pop YH	;restore Y register
	pop YL	;
	pop XH	;restore X register
	pop XL	;
	ret		;end function

;-----------------------------------------------------------
; Func: INIT0_STEP
; Desc: When press INIT0, the string wil change, and INIT0 
;		also is confirm button
;-----------------------------------------------------------
INIT0_STEP:
	push	mpr			; Save mpr register
	push	wait		; Save wait register
	in		mpr, SREG	; Save program state
	push	mpr			;
    push XL         ; save X register
    push XH         ; 
    push YL         ; save Y register
    push YH         ; 
    push ZL         ; save Z register
    push ZH         ; 
	
	;test if PD0 press
	ldi type,1

	ldi		wait, WTime	; Wait for 1 second
	rcall	Waitt			; Call wait function

	ldi XL,low (STRING_END3<<1)		;load the low byte of the end of string
	ldi XH, high(STRING_END3<<1)	;load the high byte of the end of string

	ldi YL, low(LCDLn1Addr)			;load the low byte of the LCDLn1Addr
	ldi YH, high(LCDLn1Addr)		;load the high byte of the LCDLn1Addr

	ldi ZL, low(STRING_LINE3<<1)	;load the low byte of the begin of string
	ldi ZH, high(STRING_LINE3<<1)	;load the high byte of the begin of string

	rcall LoadLine					;call function
	rcall LCDWrLn1					;calll function to write word to LDC


	ldi YL, low (operand2)
	ldi YH, high(operand2)



	inc BIT							;increase BIT 
	add YL,BIT
	ldi LETTER, 65
	st Y+, LETTER
	cpi BIT, 16						;compare BIT and 16 bit
	breq FULL_BIT					;BIT = 16 bite jump to PIND_4
	cpi BIT,0						;compare BIT and 0
	breq FIRST_LETTER				;if BIT equal 0 jump to FIRST_LETTER
	rjmp SECOND_LETTER				;otherwise jump to SECOND_LETTER


FULL_BIT:
	rcall PIND_4
	rjmp INIT0_STEP				

FIRST_LETTER:
	rcall LCDClrLn2					;clear the line in LCD
	mov mpr, LETTER					;move A to mpr
	ldi line, 2						;show the second line
	mov count, BIT					; show on the first bit
	rjmp INIT0_END					;jump to end function

SECOND_LETTER:
	ldi LETTER, 65
	mov mpr, LETTER					;move A to mpr
	ldi line, 2						;show the second line
	mov count, BIT					; show on the first bit
	rjmp INIT0_END					;jump to end function

INIT0_END:
	rcall LCDWriteByte				;call function wirte the byte
	;add YL, BIT

	//ldi		mpr, $FF		;clear queued interrupt on INT0-3
	//out		EIFR, mpr		;

	pop ZH	;restore Z register
	pop ZL	;
	pop YH	;restore Y register
	pop YL	;
	pop XH	;restore X register
	pop XL	;
	pop mpr			;restore mpr
	out SREG, mpr	;restore the status register	
	pop	wait		; Save wait register
	pop mpr
	ret

;-----------------------------------------------------------
; Func: HIT_FORWARD
; Desc: When press INIT6, the second line letter will be 
;		move forward. ex. A -> B
;-----------------------------------------------------------
HIT_FORWARD:
	;push save register
	push	mpr			; Save mpr register
	push	wait		; Save wait register
	in		mpr, SREG	; Save program state
	push	mpr			;
	push YL
	push YH

	ldi		wait, WTime	; Wait for 1 second
	rcall	Waitt			; Call wait function
	;test if PD0 press
	ldi mpr, 1
	cp  mpr, type
	breq HIT_MOVE
	rjmp MAIN


HIT_MOVE:
	ldi YL, low(operand2)	
	ldi YH, high(operand2)
	
	ldi q, -1
	mov A, q

	inc LETTER		;change letter to B
	cpi LETTER, 91	; compare the letter if Z
	breq BACK_A		; if z jum to A
	mov mpr, LETTER	;move the ltter to mpr
	ldi line, 2		; show on the second line
	mov count, BIT	; show on first bit
	rjmp DONE

BACK_A:
	ldi LETTER, 65	;;set the letter is A
	mov mpr, LETTER	;move the ltter to mpr
	ldi line, 2		; show on the second line
	mov count, BIT	; show on first bit
	rjmp DONE
DONE:
	rcall LCDWriteByte ;call function wirte byte	

	;compare count and letter bit
COMPAREA1:
	 inc A
	  cp BIT, A
	breq NEXT_A1
     brne COMPAREA1
NEXT_A1:
	add YL, A
	st Y+, LETTER



	pop YH
	pop YL
	pop mpr			;restore mpr
	pop wait
	out SREG, mpr	;restore the status register	
	pop mpr
	ret				;end the function

;-----------------------------------------------------------
; Func: HIT_FORWARD
; Desc: When press INIT6, the second line letter will be 
;		move forward. ex. A -> B
;-----------------------------------------------------------
HIT_REVERSE:
	;push save register
	push	mpr			; Save mpr register
	push	wait		; Save wait register
	in		mpr, SREG	; Save program state
	push	mpr			;
	ldi		wait, WTime	; Wait for 1 second
	rcall	Waitt			; Call wait function

	;test if PD0 press
	ldi mpr, 1
	cp  mpr, type
	breq HIT_REVE
	rjmp MAIN

HIT_REVE:
	ldi YL, low(operand2)
	ldi YH, high(operand2)
	add YL, BIT	
	ldi q, -1
	mov A, q

	dec LETTER		;change letter to Z
	cpi LETTER, 64	; compare the letter if Z
	breq MOVE_Z		; A->Z
	mov mpr, LETTER	;move the ltter to mpr
	ldi line, 2		; show on the second line
	mov count, BIT	; show on first bit
	rjmp DONE_BACK

MOVE_Z:
	ldi LETTER, 90	;set the letter is z
	mov mpr, LETTER	;move the ltter to mpr
	ldi line, 2		; show on the second line
	mov count, BIT	; show on first bit
	rjmp DONE_BACK

DONE_BACK:
	rcall  LCDWriteByte ;call function wirte byte

;COMPARE the count and letter bit
COMPAREA2:
   inc A
   cp BIT, A
   breq NEXT_A2
   brne COMPAREA2

NEXT_A2:
	add YL, A
	st Y+, LETTER


	pop mpr			;restore mpr
	pop wait
	out SREG, mpr	;restore the status register	
	pop mpr
	ret				;end the function
;-----------------------------------------------------------
; Func: PIND_4
; Desc: Active the Mors code
;-----------------------------------------------------------
PIND_4:
	push	mpr			; Save mpr register
	push	wait		; Save wait register
	in		mpr, SREG	; Save program state
	push	mpr			;
    push XL         ; save X register
    push XH         ; 
    push YL         ; save Y register
    push YH         ; 
    push ZL         ; save Z register
    push ZH         ; 

	ldi		wait, WTime	; Wait for 1 second
	rcall	Waitt			; Call wait function

	;test if PD0 press
	ldi r21, 1
	cp  r21, type
	breq ACTIVE_START
	rjmp MAIN




	
ACTIVE_START:
	ldi ZL, low(UserMode <<1)	;load the low byte time of usermode
	ldi ZH, high(UserMode <<1)	;load the high byte time of usermode
	ldi YL, low(operand1)
	ldi YH, high(operand1)
	ldi XL, low(operand2)
	ldi XH, high(operand2)
	lpm r19, Z+
	st Y+, r19
	cpi r19, 0x01
	breq TO_WAIT_1		;usermode == 00 use 200ms
	rjmp TO_WAIT_200				;otherwise use 1sec

TO_WAIT_1:
	cpi BIT, -1
	breq ACTIVE_END1
	rjmp WAIT_1

TO_WAIT_200:				;jump function to 200ms
	cpi BIT,-1
	breq ACTIVE_END1
	rjmp WAIT_200

ACTIVE_END1:
rjmp ACTIVE_END


WAIT_1:			
	ld mpr, X+			;move the LETTER TO mpr			
	cpi mpr, 65				;compare the letter
	nop
	breq TO_WAIT_A_1		;letter == 65 to A 
	brne CHECKB1
TO_WAIT_A_1:
	rjmp WAIT_A_1
CHECKB1:
	cpi mpr, 66				;
	breq TO_WAIT_B_1		;letter ==66 to B
	brne CHECKC1
TO_WAIT_B_1:
	rjmp WAIT_B_1
CHECKC1:
	cpi mpr, 67				;
	breq TO_WAIT_C_1		;letter ==67 to C
	brne CHECKD1
TO_WAIT_C_1:
	rjmp WAIT_C_1
CHECKD1:
	cpi mpr, 68				;
	breq TO_WAIT_D_1		;letter ==68 to D
	brne CHECKE1
TO_WAIT_D_1:
	rjmp WAIT_D_1
CHECKE1:
	cpi mpr, 69				;
	breq TO_WAIT_E_1		;letter ==69 to E
	brne CHECKF1
TO_WAIT_E_1:
	rjmp WAIT_E_1
CHECKF1:
	cpi mpr, 70				;
	breq TO_WAIT_F_1		;letter ==70 to F
	brne CHECKG1
TO_WAIT_F_1:
	rjmp WAIT_F_1
CHECKG1:
	cpi mpr, 71				;
	breq TO_WAIT_G_1		;letter ==71 to G
	brne CHECKH1
TO_WAIT_G_1:
	rjmp WAIT_G_1
CHECKH1:
	cpi mpr, 72				;
	breq TO_WAIT_H_1		;letter ==72 to H
	brne CHECKI1
TO_WAIT_H_1:
	rjmp WAIT_H_1
CHECKI1:
	cpi mpr, 73				;
	breq TO_WAIT_I_1		;letter ==73 to I
	brne CHECKJ1
TO_WAIT_I_1:
	rjmp WAIT_I_1
CHECKJ1:
	cpi mpr, 74				;		
	breq TO_WAIT_J_1		;letter ==74 to J
	brne CHECKK1
TO_WAIT_J_1:
	rjmp WAIT_J_1
CHECKK1:
	cpi mpr, 75				;
	breq TO_WAIT_K_1		;letter ==75 to K
	brne CHECKL1
TO_WAIT_K_1:
	rjmp WAIT_K_1
CHECKL1:
	cpi mpr, 76				;		
	breq TO_WAIT_L_1		;letter ==76 to L
	brne CHECKM1
TO_WAIT_L_1:
	rjmp WAIT_L_1
CHECKM1:
	cpi mpr, 77				;
	breq TO_WAIT_M_1		;letter ==77 to M
	brne CHECKN1
TO_WAIT_M_1:
	rjmp WAIT_M_1
CHECKN1:
	cpi mpr, 78				;
	breq TO_WAIT_N_1		;letter ==78 to N
	brne CHECKO1
TO_WAIT_N_1:
	rjmp WAIT_N_1
CHECKO1:
	cpi mpr, 79				;	
	breq TO_WAIT_O_1		;letter ==79 to O
	brne CHECKP1
TO_WAIT_O_1:
	rjmp WAIT_O_1
CHECKP1:
	cpi mpr, 80				;
	breq TO_WAIT_P_1		;letter ==80 to P
	brne CHECKQ1
TO_WAIT_P_1:
	rjmp WAIT_P_1
CHECKQ1:
	cpi mpr, 81				;
	breq TO_WAIT_Q_1		;letter ==81 to Q
	brne CHECKR1
TO_WAIT_Q_1:
	rjmp WAIT_Q_1
CHECKR1:
	cpi mpr, 82				;
	breq TO_WAIT_R_1		;letter ==82 to R
	brne CHECKS1
TO_WAIT_R_1:
	rjmp WAIT_R_1
CHECKS1:
	cpi mpr, 83				;
	breq TO_WAIT_S_1		;letter ==83 to S
	brne CHECKT1
TO_WAIT_S_1:
	rjmp WAIT_S_1

CHECKT1:
	cpi mpr, 84				;
	breq TO_WAIT_T_1		;letter ==84 to T
	brne CHECKU1
TO_WAIT_T_1:
	rjmp WAIT_T_1
CHECKU1:
	cpi mpr, 85				;
	breq TO_WAIT_U_1		;letter ==85 to U
	brne CHECKV1
TO_WAIT_U_1:
	rjmp WAIT_U_1
CHECKV1:
	cpi mpr, 86				;
	breq TO_WAIT_V_1		;letter ==86 to V
	brne CHECKW1
TO_WAIT_V_1:
	rjmp WAIT_V_1
CHECKW1:
	cpi mpr, 87				;
	breq TO_WAIT_W_1		;letter ==87 to W
	brne CHECKX1
TO_WAIT_W_1:
	rjmp WAIT_W_1
CHECKX1:
	cpi mpr, 88				;
	breq TO_WAIT_X_1		;letter ==88 to X
	brne CHECKY1
TO_WAIT_X_1:
	rjmp WAIT_X_1
CHECKY1:
	cpi mpr, 89				;
	breq TO_WAIT_Y_1		;letter ==89 to Y
	brne CHECKZ1
TO_WAIT_Y_1:
	rjmp WAIT_Y_1
CHECKZ1:
	cpi mpr, 90				;
	breq TO_WAIT_Z_1		;letter ==90 to Z
TO_WAIT_Z_1:
	rjmp WAIT_Z_1
;MORSE CODE	
WAIT_A_1:
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_B_1:
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_C_1:
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_D_1:
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_E_1:
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_F_1:
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_G_1:
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_H_1:
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_I_1:
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_J_1:
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_K_1:
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_L_1:
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec
	rjmp NEXT
WAIT_M_1:
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_N_1:
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec	
	rjmp NEXT		
WAIT_O_1:
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_P_1:
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_Q_1:
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_R_1:
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_S_1:
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_T_1:
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_U_1:
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT		
WAIT_V_1:
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT	
WAIT_W_1:
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT		
WAIT_X_1:
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_Y_1:
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_3sec	
	rjmp NEXT
WAIT_Z_1:
	rcall WAIT_DASH_1
	rcall WAIT_DASH_1
	rcall WAIT_DOT_1
	rcall WAIT_DOT_1
	rcall WAIT_3sec	
	rjmp NEXT

NEXT:
	dec BIT
	rjmp TO_WAIT_1

NEXT_200:
	dec BIT
	rjmp TO_WAIT_200

;200ms
WAIT_200:					
	ld mpr, X+			;move the LETTER TO mpr
	cpi mpr, 65				;compare the letter
	breq TO_WAIT_A_0		;letter == 65 to A 
	brne CHECKB0
TO_WAIT_A_0:
	rjmp WAIT_A_0
CHECKB0:
	cpi mpr, 66				;
	breq TO_WAIT_B_0		;letter ==66 to B
	brne CHECKC0
TO_WAIT_B_0:
	rjmp WAIT_B_0
CHECKC0:
	cpi mpr, 67				;
	breq TO_WAIT_C_0		;letter ==67 to C
	brne CHECKD0
TO_WAIT_C_0:
	rjmp WAIT_C_0
CHECKD0:
	cpi mpr, 68				;
	breq TO_WAIT_D_0		;letter ==68 to D
	brne CHECKE0
TO_WAIT_D_0:
	rjmp WAIT_D_0
CHECKE0:
	cpi mpr, 69				;
	breq TO_WAIT_E_0		;letter ==69 to E
	brne CHECKF0
TO_WAIT_E_0:
	rjmp WAIT_E_0
CHECKF0:
	cpi mpr, 70				;
	breq TO_WAIT_F_0		;letter ==70 to F
	brne CHECKG0
TO_WAIT_F_0:
	rjmp WAIT_F_0
CHECKG0:
	cpi mpr, 71				;
	breq TO_WAIT_G_0		;letter ==71 to G
	brne CHECKH0
TO_WAIT_G_0:
	rjmp WAIT_G_0
CHECKH0:
	cpi mpr, 72				;
	breq TO_WAIT_H_0		;letter ==72 to H
	brne CHECKI0
TO_WAIT_H_0:
	rjmp WAIT_H_0
CHECKI0:
	cpi mpr, 73				;
	breq TO_WAIT_I_0		;letter ==73 to I
	brne CHECKJ0
TO_WAIT_I_0:
	rjmp WAIT_I_0
CHECKJ0:
	cpi mpr, 74				;		
	breq TO_WAIT_J_0		;letter ==74 to J
	brne CHECKK0
TO_WAIT_J_0:
	rjmp WAIT_J_0
CHECKK0:
	cpi mpr, 75				;
	breq TO_WAIT_K_0		;letter ==75 to K
	brne CHECKL0
TO_WAIT_K_0:
	rjmp WAIT_K_0
CHECKL0:
	cpi mpr, 76				;		
	breq TO_WAIT_L_0		;letter ==76 to L
	brne CHECKM0
TO_WAIT_L_0:
	rjmp WAIT_L_0
CHECKM0:
	cpi mpr, 77				;
	breq TO_WAIT_M_0		;letter ==77 to M
	brne CHECKN0
TO_WAIT_M_0:
	rjmp WAIT_M_0
CHECKN0:
	cpi mpr, 78				;
	breq TO_WAIT_N_0		;letter ==78 to N
	brne CHECKO0
TO_WAIT_N_0:
	rjmp WAIT_N_0
CHECKO0:
	cpi mpr, 79				;	
	breq TO_WAIT_O_0		;letter ==79 to O
	brne CHECKP0
TO_WAIT_O_0:
	rjmp WAIT_O_0
CHECKP0:
	cpi mpr, 80				;
	breq TO_WAIT_P_0		;letter ==80 to P
	brne CHECKQ0
TO_WAIT_P_0:
	rjmp WAIT_P_0
CHECKQ0:
	cpi mpr, 81				;
	breq TO_WAIT_Q_0		;letter ==81 to Q
	brne CHECKR0
TO_WAIT_Q_0:
	rjmp WAIT_Q_0
CHECKR0:
	cpi mpr, 82				;
	breq TO_WAIT_R_0		;letter ==82 to R
	brne CHECKS0
TO_WAIT_R_0:
	rjmp WAIT_R_0
CHECKS0:
	cpi mpr, 83				;
	breq TO_WAIT_S_0		;letter ==83 to S
	brne CHECKT0
TO_WAIT_S_0:
	rjmp WAIT_S_0
CHECKT0:
	cpi mpr, 84				;
	breq TO_WAIT_T_0		;letter ==84 to T
	brne CHECKU0
TO_WAIT_T_0:
	rjmp WAIT_T_0
CHECKU0:
	cpi mpr, 85				;
	breq TO_WAIT_U_0		;letter ==85 to U
	brne CHECKV0
TO_WAIT_U_0:
	rjmp WAIT_U_0
CHECKV0:
	cpi mpr, 86				;
	breq TO_WAIT_V_0		;letter ==86 to V
	brne CHECKW0
TO_WAIT_V_0:
	rjmp WAIT_V_0
CHECKW0:
	cpi mpr, 87				;
	breq TO_WAIT_W_0		;letter ==87 to W
	brne CHECKX0
TO_WAIT_W_0:
	rjmp WAIT_W_0
CHECKX0:
	cpi mpr, 88				;
	breq TO_WAIT_X_0		;letter ==88 to X
	brne CHECKY0
TO_WAIT_X_0:
	rjmp WAIT_X_0
CHECKY0:
	cpi mpr, 89				;
	breq TO_WAIT_Y_0		;letter ==89 to Y
	brne CHECKZ0
TO_WAIT_Y_0:
	rjmp WAIT_Y_0
CHECKZ0:
	cpi mpr, 90				;
	breq TO_WAIT_Z_0		;letter ==90 to Z
TO_WAIT_Z_0:
	rjmp WAIT_Z_0

;MORSE CODE	
WAIT_A_0:
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_B_0:
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_C_0:
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_D_0:
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_E_0:
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_F_0:
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_G_0:
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_H_0:
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_I_0:
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_J_0:
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_K_0:
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_L_0:
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_M_0:
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_N_0:
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms	
	rjmp NEXT_200		
WAIT_O_0:
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms	
	rjmp NEXT_200
WAIT_P_0:
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms	
	rjmp NEXT_200
WAIT_Q_0:
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms	
	rjmp NEXT_200
WAIT_R_0:
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_S_0:
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms	
	rjmp NEXT_200
WAIT_T_0:
	rcall WAIT_DASH_200
	rcall WAIT_600ms	
	rjmp NEXT_200
WAIT_U_0:
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms
	rjmp NEXT_200		
WAIT_V_0:
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms
	rjmp NEXT_200	
WAIT_W_0:
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms
	rjmp NEXT_200		
WAIT_X_0:
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms	
	rjmp NEXT_200
WAIT_Y_0:
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_600ms
	rjmp NEXT_200
WAIT_Z_0:
	rcall WAIT_DASH_200
	rcall WAIT_DASH_200
	rcall WAIT_DOT_200
	rcall WAIT_DOT_200
	rcall WAIT_600ms	
	rjmp NEXT_200


ACTIVE_END:
	cbi PORTB, PB4	;turn off OC0
	cbi PORTB, PB5	;turn off OC0
	cbi PORTB, PB6	;turn off OC0
	cbi PORTB, PB7	;turn off OC0
	rjmp INIT

	pop ZH	;restore Z register
	pop ZL	;
	pop YH	;restore Y register
	pop YL	;
	pop XH	;restore X register
	pop XL	;
	pop mpr			;restore mpr
	out SREG, mpr	;restore the status register	
	pop	wait		; Save wait register
	pop mpr

	ret						; End a function with RET

;-----------------------------------------------------------
; Func: WAIT_DOT_1
; Desc: set the time for DOT of UserMode:	.DB	0x01, 0x00
;-----------------------------------------------------------
WAIT_DOT_1:

	sbi PORTB, PB4
	sbi PORTB, PB5	;TURN ON OC0
	sbi PORTB, PB6	;TURN ON OC0
	sbi PORTB, PB7	;turn on OC0
	rcall WAIT_1sec	;wait 1 second
	sbi PORTB, PB4
	cbi PORTB, PB5	;turn off OC0
	cbi PORTB, PB6	;turn off OC0
	cbi PORTB, PB7	;turn off OC0
	rcall WAIT_1sec	;wait 1 second
	ret

;-----------------------------------------------------------
; Func: WAIT_DASH_1
; Desc: set the time for DOT of UserMode:	.DB	0x01, 0x00
;-----------------------------------------------------------
WAIT_DASH_1:
	sbi PORTB, PB4
	sbi PORTB, PB5	;TURN ON OC0
	sbi PORTB, PB6	;TURN ON OC0
	sbi PORTB, PB7	;turn on OC0
	rcall WAIT_3sec	;wait 1 second
	sbi PORTB,PB4
	cbi PORTB, PB5	;turn off OC0
	cbi PORTB, PB6	;turn off OC0
	cbi PORTB, PB7	;turn off OC0
	rcall WAIT_1sec	;wait 1 second
	ret

;-----------------------------------------------------------
; Func: WAIT_DOT_200
; Desc: set the time for DOT of UserMode:	.DB	0x00, 0x00
;-----------------------------------------------------------
WAIT_DOT_200:
	sbi PORTB, PB4
	sbi PORTB, PB5	;TURN ON OC0
	sbi PORTB, PB6	;TURN ON OC0
	sbi PORTB, PB7	;turn on OC0
	rcall WAIT_200ms	;wait 200 ms
	cbi PORTB, PB5	;turn off OC0
	cbi PORTB, PB6	;turn off OC0
	cbi PORTB, PB7	;turn off OC0
	rcall WAIT_200ms	;wait 200 ms
	ret

;-----------------------------------------------------------
; Func: WAIT_DASH_200
; Desc: set the time for DOT of UserMode:	.DB	0x01, 0x00
;-----------------------------------------------------------
WAIT_DASH_200:
	sbi PORTB, PB4
	sbi PORTB, PB5	;TURN ON OC0
	sbi PORTB, PB6	;TURN ON OC0
	sbi PORTB, PB7	;turn on OC0
	rcall WAIT_600ms	;wait 600ms
	cbi PORTB, PB5	;turn off OC0
	cbi PORTB, PB6	;turn off OC0
	cbi PORTB, PB7	;turn off OC0
	rcall WAIT_200ms;wait 200ms
	ret

;-----------------------------------------------------------
; Func: WAIT_1sec
; Desc: set the time for UserMode:	.DB	0x01, 0x00
; Reference: ECE375 TEXTBOOK P160
;-----------------------------------------------------------
WAIT_1sec:
	;Timer/Counter		
	sbi DDRB, PB4
	sbi DDRB, PB5				;Turn on OC0
	sbi DDRB, PB6				;turn on OC0
	sbi DDRB, PB7				;turn on OC0
	ldi mpr, 0b00000000			;active normal mode, OC0 disconnected
	out TCCR1A, mpr				; set the prescalar
	ldi mpr, 0b00000100		; active normal mode, OC0 disconnected
	out TCCR1B, mpr				;set the pre scalar to 256

	ldi		count, 1			;load loop count=1

WAIT_10msec1:
	ldi		dataptr, 100		;loade value for delay
	out		TCNT1L, dataptr		;wait for TCNT1L
CHECK_1sec:
	in		dataptr, TIFR		;read in TIFR
	andi	dataptr, 0b00000100	; check if its set
	breq	CHECK_1sec			; loop iF TOV1 not set
	ldi		dataptr, 0b00000100	;Reset TOV1
	out 	TIFR, dataptr		;note - write 1 to reset
	dec		count				;decrement count
	brne	WAIT_10msec1			;loop if count to not equal to 0
	ret

;-----------------------------------------------------------
; Func: WAIT_3sec
; Desc: set the time for UserMode:	.DB	0x01, 0x00
; Reference: ECE375 TEXTBOOK P160
;-----------------------------------------------------------
WAIT_3sec:
	;Timer/Counter		
	sbi DDRB, PB4
	sbi DDRB, PB5				;Turn on OC0
	sbi DDRB, PB6				;turn on OC0
	sbi DDRB, PB7				;turn on OC0
	ldi mpr, 0b00000000			;active normal mode, OC0 disconnected
	out TCCR1A, mpr				; set the prescalar
	ldi mpr, 0b00000100		; active normal mode, OC0 disconnected
	out TCCR1B, mpr				;set the pre scalar to 256

	ldi		count, 3			;load loop count=100
WAIT_10msec:
	ldi		dataptr, 100		;loade value for delay(30*10)
	out		TCNT1L, dataptr		;wait for TCNT1L
CHECK_3sec:
	in		dataptr, TIFR		;read in TIFR
	andi	dataptr, 0b00000100	; check if its set
	breq	CHECK_3sec			; loop iF TOV1 not set
	ldi		dataptr, 0b00000100	;Reset TOV1
	out 	TIFR, dataptr		;note - write 1 to reset
	dec		count				;decrement count
	brne	WAIT_10msec			;loop if count to not equal to 0
	ret

;-----------------------------------------------------------
; Func: WAIT_200ms
; Desc: set the time for UserMode:	.DB	0x00, 0x00
; Reference: ECE375 TEXTBOOK P160
;-----------------------------------------------------------
WAIT_200ms:
	;Timer/Counter
	sbi DDRB, PB4
	sbi DDRB, PB5				;Turn on OC0
	sbi	DDRB, PB6				;Turn on OC0
	sbi DDRB, PB7				;turn on OC0
	ldi mpr, 0b00000000			;active normal mode, OC0 disconnected
	out TCCR1A, mpr				;set the prescalar
	ldi mpr, 0b00000010			;active normal mode, OC0 disconnected
	out TCCR1B, mpr				;set the presalar 8

	ldi		count, 6		;load loop count=100
WAIT_10msec2:
	ldi		dataptr, 100		;loade value for delay(30*10)
	out		TCNT1L, dataptr		;wait for TCNT1L
CHECK_200ms:
	in		dataptr, TIFR		;read in TIFR
	andi	dataptr, 0b00000100	; check if its set
	breq	CHECK_200ms			; loop iF TOV1 not set
	ldi		dataptr, 0b00000100	;Reset TOV1
	out 	TIFR, dataptr		;note - write 1 to reset
	dec		count				;decrement count
	brne	WAIT_10msec2		;loop if count to not equal to 0
	ret

;-----------------------------------------------------------
; Func: WAIT_600ms
; Desc: set the time for UserMode:	.DB	0x00, 0x00
; Reference: ECE375 TEXTBOOK P160
;-----------------------------------------------------------
WAIT_600ms:	
	;Timer/Counter
	sbi DDRB, PB4
	sbi DDRB, PB5				;Turn on OC0
	sbi	DDRB, PB6				;Turn on OC0
	sbi DDRB, PB7				;turn on OC0
	ldi mpr, 0b00000000			;active normal mode, OC0 disconnected
	out TCCR1A, mpr				;set the prescalar
	ldi mpr, 0b00000010			;active normal mode, OC0 disconnected
	out TCCR1B, mpr				;set the presalar 8

	ldi		count, 18			;load loop count=100
WAIT_10msec6:
	ldi		dataptr, 100		;loade value for delay(30*10)
	out		TCNT1L, dataptr		;wait for TCNT1L
CHECK_600ms:
	in		dataptr, TIFR		;read in TIFR
	andi	dataptr, 0b00000100	; check if its set
	breq	CHECK_600ms			; loop iF TOV1 not set
	ldi		dataptr, 0b00000100	;Reset TOV1
	out 	TIFR, dataptr		;note - write 1 to reset
	dec		count				;decrement count
	brne	WAIT_10msec		;loop if count to not equal to 0
	ret
;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;reference: lab1 code
;----------------------------------------------------------------
Waitt:
		;push save registers
		push	wait			; Save wait register
		push	r			; Save ilcnt register
		push	q			; Save olcnt register

Loop:	ldi		q, 224		; load olcnt register
OLoop:	ldi		r, 237		; load ilcnt register
ILoop:	dec		r			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		q			; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		wait			; Decrement wait 
		brne	Loop			; Continue Wait loop	
		
		;pop restore registers
		pop		q		; Restore olcnt register
		pop		r		; Restore ilcnt register
		pop		wait		; Restore wait register
		ret					; Return from subroutine


;***********************************************************
;*	Stored Program Data
;***********************************************************
STRING_LINE1:
.DB  "Welcome!        "  ; Extra spaces added to make 16 bits
STRING_END1:

STRING_LINE2:
.DB  "Please press PD0"  ; Extra spaces added to make 16 bits
STRING_END2:

STRING_LINE3:
.DB "Enter word:     "  ; Extra spaces added to make 16 bits
STRING_END3:

;***end of your code***end of your code***end of your code***end of your code***end of your code***
;******************************* Do not change below this point************************************
;******************************* Do not change below this point************************************
;******************************* Do not change below this point************************************


;***********************************************************
;*	Stored Program Data
;***********************************************************
; Contents of program memory will be changed during testing
; The label names are not changed

; If UserMode is 0x01, then one unit of time is 1 second
UserMode:	.DB	0x01, 0x00
; You can ignore the second byte (it's only included so that there is an even number of bytes)

; If UserMode is 0x00, then one unit of time is 200 milliseconds
; This would look like the following:
;UserMode:	.DB	0x00, 0x00
; (again, ignore the second byte)

; UserMode will always be set to either 0x00 or 0x01


;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver from Lab 4
