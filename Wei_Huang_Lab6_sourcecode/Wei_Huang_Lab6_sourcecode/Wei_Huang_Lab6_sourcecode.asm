;***********************************************************
;*
;*	Wei_Huang_Lab6_sourcecode.asm
;*
;*	External Interrupt.
;*
;*	This is the skeleton file for Lab 6 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Wei Huang
;*	   Date: 11/9/2020 6:08:12 PM
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	waitcnt = r23			; Wait loop counter
.def	ilcnt = r24				; inner loop counter
.def	olcnt = r25				; outer loop counter
.def	rightCounter = r4		; Right whisker hit counter
.def	leftCounter = r5		; Left whisker hit counter

.equ	WTime	= 100			; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit
.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit
 

.equ	MovForward = (1<<EngDirR|1<<EngDirL)		; move forward command
.equ	MovBack = $00								; reverse command
.equ	TurnR = (1<<EngDirL)						; turn right command
.equ	TurnL = (1<<EngDirR)						; turn left command
.equ	Halt = (1<<EngEnR|1<<EngEnL)				; Halt command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used
.org	$0002					;INT0
		rcall	HitRight		;call function to handle right whisker hit
		reti					;return from interrupt
.org	$0004					;INT1
		rcall	HitLeft			;call function to handle left whisker hit
		reti					; return from interrupt
.org	$0006					;INT2
		rcall	ClearRight		;call function clear right whisker hit
		reti					;return from interrupt
.org	$0008					;INT3
		rcall	ClearLeft		;call function clear left whisker hit
		reti					;return from interrupt
	
		; This is just an example:
;.org	$002E					; Analog Comparator IV
;		rcall	HandleAC		; Call function to handle interrupt
;		reti					; Return from interrupt

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		ldi				mpr, low(RAMEND)	; set low byte of SPto low byte of RAMEND
		out				SPL, mpr			;load SPL with low byte of RAMEND
		ldi				mpr, high(RAMEND)	;set hight byte of SP to high byte of RAMEND
		out				SPH, mpr			;load SPH with hight byte of RAMEND
		
		; Initialize Port B for output
		ldi				mpr, $FF			;set Port B Data Direction register
		out				DDRB, mpr			;FOR OUTPUT
		ldi				mpr, $00			;initialize Port B Data register
		out				PORTB, mpr			;so all port B outputs are low	
		
		; Initialize Port D for input
		ldi				mpr, $00			;set Port D data direction resgister
		out				DDRD, mpr			;for output
		ldi				mpr, $FF			;initial Port D Data register
		out				PORTD, mpr			;so all port D inputs are Tri-State

		;initialize LDC Display
		rcall LCDInit

		;initialize whisker hit counters
		clr	rightCounter		;set right counter to 0
		clr leftCounter			;set left counter to 0
		rcall WriterightCounter	; initial value of right counter on  LCD
		rcall WriteleftCounter	; initial value of left counter on LCD

		; Initialize external interrupts
			; Set the Interrupt Sense Control to falling edge 
		ldi				mpr, (1<<ISC01)|(1<<ISC11)|(1<<ISC21)|(1<ISC31)	; set falling-edge interrupts for INT0-3
		sts				EICRA, mpr			; store EIRA
		; Configure the External Interrupt Mask
		ldi				mpr, (1<<INT0)|(1<<INT1)|(1<<INT2)|(1<<INT3)	; Enable interrupts INT0-3
		out				EIMSK, mpr			;for ouput

		; Turn on interrupts
			; NOTE: This must be the last thing to do in the INIT function
		sei						;set gobal interrupt bit to enable interrupts
;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		ldi		mpr, MovForward	;load move forward command
		out		PORTB, mpr		;send to ouput

		rjmp	MAIN			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
;	You will probably want several functions, one to handle the 
;	left whisker interrupt, one to handle the right whisker 
;	interrupt, and maybe a wait function
;------------------------------------------------------------

;-----------------------------------------------------------
; Func: WriterightCounter 
; Desc: Display the vaule on line one 
;-----------------------------------------------------------
WriterightCounter:		
					
		rcall		LCDClrLn1				;clear first line for LCD
		mov			mpr,rightCounter		;load right counter to mpr
		ldi			XL, low(LCDLn1Addr)		;load first memory address of line one to X
		ldi			XH, high(LCDLn1Addr)	
		rcall		Bin2ASCII				;convert bin to ASCII
		rcall		LCDWrLn1				;call LCDWrLn1 to write the value on line one
		ret						; End a function with RET

;-----------------------------------------------------------pp\p
; Func: WriteleftCounter 
.; Desc: Display the vaule on line two 
;-----------------------------------------------------------
WriteleftCounter:

		rcall		LCDClrLn2				;clear first line for LCD
		mov			mpr,leftCounter			;load right counter to mpr
		ldi			XL, low(LCDLn2Addr)		;load first memory address of line one to X
		ldi			XH, high(LCDLn2Addr)	
		rcall		Bin2ASCII				;convert bin to ASCII
		rcall		LCDWrLn2				;call LCDWrLn1 to write the value on line one
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: HitRight 
; Desc: handle right whicker hit
;reference: lab1 code
;-----------------------------------------------------------
HitRight:
		;push save registers
		push	mpr			; Save mpr register
		push	waitcnt		; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		inc		rightCounter		;increment hit counter
		rcall	WriterightCounter	;Display increment counter

		;move backward for a second
		ldi		mpr, MovBack		;load move backward command
		out		PORTB, mpr			;send the command to port
		ldi		waitcnt, WTime		; wait 1 second
		rcall	Waitt				;call wait function

		;turn left for a second
		ldi		mpr,TurnL			;load turn left command
		out		PORTB, mpr			;send the command to port
		ldi		waitcnt, WTime		;wait 1 second
		rcall	Waitt				;call wait function

		;move forward again
		ldi		mpr, MovForward		;load move forward command
		out		PORTB, mpr			;send the command to port

		ldi		mpr, $FF			;clear queued interrupt on INT0-3
		out		EIFR, mpr			;
	
		;pop restore registers
		pop		mpr					; Restore program state
		out		SREG, mpr			;
		pop		waitcnt				; Restore wait register
		pop		mpr					; Restore mpr
		ret							; Return from subroutine
;-----------------------------------------------------------
; Func: HitLeft 
; Desc: handle left whicker hit
;reference: lab1 code
;-----------------------------------------------------------
HitLeft:
		;push save registers
		push	mpr			; Save mpr register
		push	waitcnt		; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		inc		leftCounter			;increment hit counter
		rcall	WriteleftCounter		;Display increment counter

		; Move Backwards for a second
		ldi		mpr, MovBack	; Load Move Backward command
		out		PORTB, mpr		; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Waitt			; Call wait function

		; Turn right for a second
		ldi		mpr, TurnR		; Load Turn Left Command
		out		PORTB, mpr		; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Waitt			; Call wait function

		; Move Forward again	
		ldi		mpr, MovForward	; Load Move Forward command
		out		PORTB, mpr		; Send command to port

		ldi		mpr, $FF		;clear queued interrupt on INT0-3
		out		EIFR, mpr		;

		;pop restore registers
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr

		ret						; Return from subroutine

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
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt			; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt			; Decrement wait 
		brne	Loop			; Continue Wait loop	
		
		;pop restore registers
		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret					; Return from subroutine

;-----------------------------------------------------------
; Func: ClearRigh
; Desc: clear the right counter LCD
;-----------------------------------------------------------
ClearRight:
		clr		rightCounter		; set right counter to zero
		rcall	WriterightCounter	; show value on line one
		ldi		mpr, $FF			;clear queued interrupt on INT0-3
		out		EIFR, mpr			; 
		ret							; Return from subroutine

;-----------------------------------------------------------
; Func: ClearLeft
; Desc: clear the left counter LCD
;-----------------------------------------------------------
ClearLeft:
		clr		leftCounter			;set left counter to zero
		rcall	WriteleftCounter	;show value on line two
		ldi		mpr, $FF			;clear queued interrupt on INT0-3
		out		EIFR, mpr			;
		ret							;return from subroutine

;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		;inlcude the LCD Driver
