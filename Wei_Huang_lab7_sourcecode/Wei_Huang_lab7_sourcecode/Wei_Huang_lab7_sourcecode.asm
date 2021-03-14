;***********************************************************
;*
;*	Wei_Huang_lab7_sourcecode.asm
;*
;*	Timer/Counter
;*
;*	This is the skeleton file for Lab 7 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Huang Wei
;*	   Date: 11/16/2020 5:57:00 PM
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	speed = r17				;current speed 
.def	waitcnt = r18			; wait loop counter
.def	ilcnt = r19				;inner loop counter
.def	olcnt =	r20				;outer loop counter

.equ	WTime = 15				;equalent to a 150ms delay		 

.equ	EngEnR = 4				; right Engine Enable Bit
.equ	EngEnL = 7				; left Engine Enable Bit
.equ	EngDirR = 5				; right Engine Direction Bit
.equ	EngDirL = 6				; left Engine Direction Bit

.equ	MovFwd = (1<< EngDirR)|(1<<EngDirL)	; move forward command

;8-bit Timer/Counter
.equ TcntDelta = 17				; 16 spped levels, 255/15 =17
.equ TcntMAX = 255				;max  speed value
.equ SpeedMAX =	15				;max speed level
.equ SpeedMIN =	0				;min speed level	

 

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

		; place instructions in interrupt vectors here, if needed
.org	$0002					;INT0
		rcall SPEED_DOWN		;decrease speed 
		reti					;return from interrupt

.org	$0004					;INT1
		rcall SPEED_UP			;increase speed
		reti					;return from interrupt

.org	$0006					;INT2
		rcall SPEED_MIN			;decrease speed to lowest
		reti					;return  from interrupt

.org	$0008					;INT3
		rcall SPEED_MAX			;increase speed to highest
		reti					;return from interrupt

.org	$0046					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer
		ldi mpr, low(RAMEND)			;low byte
		out SPL, mpr					;output to SPL with low byte
		ldi mpr, high(RAMEND)			;high byte 
		out SPH, mpr					;output to SPH with high byte

		; Configure I/O ports
		;configure port B
		ldi mpr, $FF					;set port B data direction register
		out DDRB, mpr					;for output
		ldi mpr, $00					; initialize port B data register
		out PORTB, mpr					; so all port B outputs are low

		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State

		; Configure External Interrupts, if needed
		ldi mpr, (1<<ISC01)|(1<<ISC11)|(1<<ISC21)|(1<<ISC31)	;set falling-edge interrupts for INT3-0
		sts	EICRA, mpr					;
		;configure ecternal interrupts mask
		ldi	mpr, (1<<INT0)|(1<<INT1)|(1<<INT2)|(1<<INT3) ;PORTD pins 3-0
		out EIMSK, mpr								;

		; Configure 8-bit Timer/Counters
		ldi mpr, 0b01101001		;configure Timer/Counter 1 and 2 with faste PWM
								;no prescaling
		out	TCCR0, mpr			;
		out TCCR2, mpr			; 

		; Set TekBot to Move Forward (1<<EngDirR|1<<EngDirL)
		ldi mpr, MovFwd						; move forward
		out PORTB, mpr						;

		; Set initial speed, display on Port B pins 3:0
		ldi	speed, SpeedMAX		;initial speed to 15
		eor	mpr, speed			;inidcatie speed 3:0
		out PORTB, mpr			; output
		ldi mpr, $00			;set initial duty cycle of 0%
		out OCR0, mpr			; output 
		out OCR2, mpr			; output 

		; Enable global interrupts (if any are used)
		sei						;set gobal interrupts
;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func:	SPEED_DOWN
; Desc:	decrements the motor speed in fast PWM.
;-----------------------------------------------------------
SPEED_DOWN:
		;push save register
		push mpr		;save mpr
		in mpr, SREG	; copy SREG to mpr
		push mpr		; save mpr, SREG
		
		ldi waitcnt, WTime		;load wait time to waitcnt
		rcall Wait				;recall wait function
		
		mov mpr, speed			;load speed to mpr
		cpi mpr, SpeedMIN		;check if min speed 
		breq SPEED_DOWN_DONE	; do nothing

		dec speed				;decrement speed by leve one
		ldi mpr, MovFwd			;keep move forward
		eor mpr, speed			;indicate speed 
		out PORTB, mpr			;
		ldi mpr, TcntDelta		;get speed level increment
		mul mpr, speed			;speed increment

		ldi mpr, TcntMAX		; get the max speed value
		sub mpr, r0				; get duty cycle value
		out OCR0, mpr			; new duty cycle 
		out OCR2, mpr			;

SPEED_DOWN_DONE:
		;clear EIFR to reset interrupt
		ldi mpr, (1<<INT0)|(1<<INT1)|(1<<INT2)|(1<<INT3)			; clear queued interrupt
		out EIFR, mpr			;

		;pop restore register
		pop mpr					;restore mpr, SREG
		out SREG, mpr			; copt mpr to SREG
		pop mpr					; restore mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	SPEEP_UP
; Desc:	increment the motor speed in fast PWM.
;-----------------------------------------------------------
SPEED_UP:
		;push save register
		push mpr		;save mpr
		in mpr, SREG	; copy SREG to mpr
		push mpr		; save mpr, SREG
		
		ldi waitcnt, WTime		;load wait time to waitcnt
		rcall Wait				;recall wait function
		
		mov mpr, speed			;load speed to mpr
		cpi mpr, SpeedMAX		;check if max speed 
		breq SPEED_UP_DONE		; do nothing

		inc speed				;decrement speed by leve one
		ldi mpr, MovFwd			;keep move forward
		eor mpr, speed			;indicate speed 
		out PORTB, mpr			;
		ldi mpr, TcntDelta		;get speed level increment
		mul mpr, speed			;speed increment

		ldi mpr, TcntMAX		; get the max speed value
		sub mpr, r0				;get duty cycle value
		out OCR0, mpr			; new duty cycle 
		out OCR2, mpr			;

SPEED_UP_DONE:
		;clear EIFR to reset interrupt
		ldi mpr, (1<<INT0)|(1<<INT1)|(1<<INT2)|(1<<INT3)			; clear queued interrupt
		out EIFR, mpr			;

		;pop restore register
		pop mpr					;restore mpr, SREG
		out SREG, mpr			; copt mpr to SREG
		pop mpr					; restore mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	SPEED_MIN
; Desc:	decrement the motor speed ito minimum level.
;-----------------------------------------------------------
SPEED_MIN:
;push save register
	push mpr	;save mpr
	
	ldi waitcnt, WTime		;load wait time to waitcnt
	rcall Wait				;recall wait function

	ldi	mpr, $FF			;load 100% duty cycle
	out OCR0, mpr			; signals 100% duty cycle 
	out OCR2, mpr			;

	ldi speed, SpeedMIN		;load mini speed level
	ldi mpr, MovFwd			;mobe forward
	out PORTB, mpr			; output

	ldi mpr, (1<<INT0)|(1<<INT1)|(1<<INT2)|(1<<INT3)			; clear queued interrupt
	out EIFR, mpr			;

	;pop restore register
	pop mpr		;restore mpr
	ret						; End a function with RET

;-----------------------------------------------------------
; Func:	SPEED_MAX
; Desc:	decrement the motor speed ito minimum level.
;-----------------------------------------------------------
SPEED_MAX:
;push save register
	push mpr	;save mpr
	
	ldi waitcnt, WTime		;load wait time to waitcnt
	rcall Wait				;recall wait function

	ldi	mpr, $00			;load 0% duty cycle
	out OCR0, mpr			; signals 0% duty cycle 
	out OCR2, mpr			;

	ldi speed, SpeedMAX		;load mini speed level
	ldi mpr, $6F			;move forward
	out PORTB, mpr			; output

	ldi mpr, (1<<INT0)|(1<<INT1)|(1<<INT2)|(1<<INT3)			; clear queued interrupt
	out EIFR, mpr			;

	;pop restore register
	pop mpr		;restore mpr
	ret			; End a function with RET

;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;reference: lab1
;----------------------------------------------------------------
Wait:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt		; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret				; Return from subroutine

;***********************************************************
;*	Stored Program Dat}|

;***********************************************************
		; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program
