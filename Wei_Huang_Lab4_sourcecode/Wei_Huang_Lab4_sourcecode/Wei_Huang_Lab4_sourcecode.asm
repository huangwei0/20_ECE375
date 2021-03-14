;***********************************************************
;*
;*	Wei_Huang_Lab4_sourcecode.asm
;*
;*	Dispalys the text on the board 
;*
;*	This is the skeleton file for Lab 4 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Wei Huang
;*	   Date: 10/26/2020 3:47:32 PM
;*
;***********************************************************

.include "m128def.inc"      ; Include definition file

;***********************************************************
;*  Internal Register Definitions and Constants
;***********************************************************
.def  mpr = r16       ; Multipurpose register is
                      ; required for LCD Driver

;***********************************************************
;*  Start of Code Segment
;***********************************************************
.cseg               ; Beginning of code segment

;***********************************************************
;*  Interrupt Vectors
;***********************************************************
.org  $0000         ; Beginning of IVs
    rjmp INIT       ; Reset interrupt

.org  $0046         ; End of Interrupt Vectors

;***********************************************************
;*  Program Initialization
;***********************************************************
INIT:               ; The initialization routine
    ; Initialize Stack Pointer
    ldi mpr, low(RAMEND)	; Load low byte of end SRAM address into mpr
    out SPL, mpr			; Write byte to SPL
    ldi mpr, high(RAMEND)	;Load high byte of end SRAM address into mpr
    out SPH, mpr			; Write byte to SPH
	

    ; Initialize port D for input
    ldi mpr, $00	;Set port D Data Direcrion register
    out DDRD, mpr   ; for output
    ldi mpr,  $FF	; Initialize Port D Data register
    out PORTD, mpr  ; so all Port D inputs are Tri-state
    
    ; Initialize LCD Display
    rcall LCDInit



		; NOTE that there is no RET or RJMP from INIT, this
		; is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*  Main Program
;***********************************************************
MAIN:							; The Main program

		in r20, PIND			; get the whisker input from port D
		cpi r20, 0b11111110		; check for whisker 0 input
		breq WHISKERPD0			; if hit whisker 0 call WHISKERPD0 function
		cpi r20, 0b11111101		; check for whisker 1 input
		breq WHISKERPD1			; if hit whisker 1 call WHISKERPD1 function
		cpi r20, 0b10111111		; check whisker 7 input
		breq WHISKERPD7			; if hit whisker 7 call WHISKERPD7 function	
		rjmp MAIN				; jump back to main and create an infinite
								; while loop.  Generally, every main program is an
								; infinite while loop, never let the main program
								; just run off
;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: WHISKERPD0
; Desc: show name first line, and hello world second line
;-----------------------------------------------------------
WHISKERPD0:
    push XL         ; push the low byte of X
    push XH         ; push the high byte of X
    push YL         ; push the low byte of Y
    push YH         ; push the high byte of Y
    push ZL         ; push the low byte of Z
    push ZH         ; push the high byte of Z

	ldi   ZL, low(STRING_BEG<<1)	;load low byte STRING_BEG to Z
    ldi   ZH, high(STRING_BEG<<1)	;load high byte STRING_BEG  to Z
    ldi   YL, low(LCDLn1Addr)		;load low byte of LCDLn1Addr to Y
    ldi   YH, high(LCDLn1Addr)		;load high byte of LCDLn1Addr to Y
    ldi   XL, low(STRING_END<<1)	;load low byte of STRING_END to X
    ldi   XH, high(STRING_END<<1)	;load high byte of STRING_END to X
    
    rcall LOADSTRING	;call LOADSTRING fucntion

    ldi   ZL, low(STRING_BEGI<<1)	;load low byte STRING_BEGi to Z
    ldi   ZH, high(STRING_BEGI<<1)	;load high byte STRING_BEGi  to Z
    ldi   YL, low(LCDLn2Addr)		;load low byte of LCDLn2Addr to Y
    ldi   YH, high(LCDLn2Addr)		;load high byte of LCDLn2Addr to Y
    ldi   XL, low(STRING_ENDI<<1)	;load low byte of STRING_ENDi to X
    ldi   XH, high(STRING_ENDI<<1)	;load high byte of STRING_ENDi to X	
  
    rcall LOADSTRING	;call LOADSTRING fucntion
	rcall LCDWrite		;load two lines of LCD

 	pop ZH	;restore hight byte of Z
	pop ZL	;restore low byte of Z
	pop YH	;restore high byte of Y
	pop YL	;restore low byte of Y
	pop XH	;restore high byte of X
	pop XL	;restore low byte of X
	ret						; End a function with RET
;-----------------------------------------------------------
; Func: WHISKERPD1
; Desc: show hello world first line, and name second line
;-----------------------------------------------------------
WHISKERPD1:
	push XL	;push the low byte of X
	push XH	;push the high byte of X
	push YL	;push the low byte of Y
	push YH	;push the high byte of Y
	push ZL	;push the low byte of Z
	push ZH	;push the high byte of Z

	ldi ZL, low (STRING_BEGI<<1) ;load low byte STRING_BEGi to Z
	ldi ZH, high(STRING_BEGI<<1) ;load high byte STRING_BEGi  to Z	
	ldi YL, low	(LCDLn1Addr)	;load low byte of LCDLn2Addr to Y
	ldi YH, high(LCDLn1Addr)	;load high byte of LCDLn2Addr to Y
	ldi XL, low (STRING_ENDI<<1)	;load low byte of STRING_ENDi to X
	ldi XH, high(STRING_ENDI<<1)	;load high byte of STRING_ENDi to X		

	rcall LOADSTRING					;call LOADSTRING fucntion

	ldi ZL, low (STRING_BEG<<1) ;load low byte STRING_BEG to Z
	ldi ZH, high(STRING_BEG<<1) ;load high byte STRING_BEG  to Z		
	ldi YL, low	(LCDLn2Addr)	;load low byte of LCDLn1Addr to Y
	ldi YH, high(LCDLn2Addr)	;load high byte of LCDLn1Addr to Y
	ldi XL, low (STRING_END<<1)	;load low byte of STRING_END to X
	ldi XH, high(STRING_END<<1)	;load high byte of STRING_END to X

	rcall LOADSTRING				;call LOADSTRING fucntion

	rcall LCDWrite		;load two lines of LCD

	pop ZH	;restore hight byte of Z
	pop ZL	;restore low byte of Z
	pop YH	;restore high byte of Y
	pop YL	;restore low byte of Y
	pop XH	;restore high byte of X
	pop XL	;restore low byte of X
	ret		; End a function with RET
;-----------------------------------------------------------
; Func: WHISKERPD7
; Desc: clear
;-----------------------------------------------------------
WHISKERPD7:
		rcall LCDClr	;clear the line in LCD
		rjmp INIT		;jump to INIT for new call

LOADSTRING:							; Begin a function with a label
	; Save variables by pushing them to the stack
    push mpr        ; push mpr to stack
    push ZL         ; push the low byte Z to stack
    push ZH         ; push the high byte Z to stack
    push YL         ; push the low byte Y to stack
    push YH         ; push the high byte Y to stack

	; Execute the function here
	LOADSTRINGLOOP:
		lpm mpr, Z+     ; load byte from Z point to next byte
		st Y+, mpr      ; store byte into memory and incremeny 
		cp ZL, XL       ; check the low byte match
		brne LOADSTRINGLOOP ; not end of the line, continue
		cp ZH, XH       ; check the high byte match
		brne LOADSTRINGLOOP ; not end of the line, continue

	; Restore variables by popping them from the stack,
	; in reverse order
	pop YH          ; restore the high byte of Y from stack
	pop YL          ; restore the low byte of Y from stack
	pop ZH          ; restore the high byte of Z from stack
	pop ZL          ; restore the high byte of Z from stack
	pop mpr         ; restore mpr from stack
	ret             ; End a function with RET


;***********************************************************
;*	Stored Program Data
;***********************************************************

;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING_BEG:
.DB "WEI HUANG "  ; Extra spaces added to make 16 bits
STRING_END:
;-----------------------------------------------------------
; Hello world string; displayed on line 1 when PD1 is hit
; and on line 2 when PD0 is hit.
;-----------------------------------------------------------
STRING_BEGI:
.DB "Hello World "  ; Extra spaces added to make 16 bits
STRING_ENDI:
;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver
