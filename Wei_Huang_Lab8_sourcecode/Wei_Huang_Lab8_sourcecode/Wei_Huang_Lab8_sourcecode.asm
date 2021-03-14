;***********************************************************
;*
;*	Wei_Huang_Lab8_sourcecode.asm
;*
;*	square root caculate
;*
;*	This is the skeleton file for Lab 8 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Wei Huang
;*	   Date: 11/24/2020 11:39:43 PM
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
.def	A = r3					; A variable
.def	B = r4					; Another variable
.def	C = r5					; Another variable
.def	D = r6					;Another variable
.def	E = r7					;Another variable
.def	F = r8					;Another variable
.def	mpr = r16				; Multipurpose register 
.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter
.def	dataptr = r19			; data ptr
.def	mpr0 = r20				;Multipurpose register 
.def	best = r21				; best register
.def	root_low = r22			;root _low register
.def	root_high = r23			;root_high high register
.def	counter_low = r24		;counter_low 			
.def	counter_high = r25		;counter_high
;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
.dseg
.org	$0100						; data memory allocation for operands
operand1:		.byte 2				; allocate 2 bytes for a variable named operand1
operand2:		.byte 2				; allocate 2 bytes for a variable named operand2
		
operand3:		.byte 2				; allocate 2 bytes for a variable named operand3
operand4:		.byte 2				; allocate 2 bytes for a variable named operand4
		
operand5:		.byte 2				; allocate 2 bytes for a variable named operand3
operand6:		.byte 2				; allocate 2 bytes for a variable named operand4
		
.org $0116		
MUL_RESULT1:	.byte 3				;allocate 3 bytes for a variabke named MUL_RESULTX1
MUL_RESULT2:	.byte 3				;allocate 3 bytes for a variabke named MUL_RESULTY2
		
MUL_RESULT3:	.byte 3				;allocate 3 bytes for a variabke named MUL_RESULTX3
MUL_RESULT4:	.byte 3				;allocate 3 bytes for a variabke named MUL_RESULTY4
		
MUL_RESULT5:	.byte 3				;allocate 3 bytes for a variabke named MUL_RESULTX5
MUL_RESULT6:	.byte 3				;allocate 3 bytes for a variabke named MUL_RESULTY6

.org $012C
ADD_RESULT1:	.byte 3				;allocate 3 bytes for a variabke named ADD_RESULT1
ADD_RESULT2:	.byte 3				;allocate 3 bytes for a variabke named ADD_RESULT2
ADD_RESULT3:	.byte 3				;allocate 3 bytes for a variabke named ADD_RESULT3

.org $0142	
ROOT_RESULT1:	.byte 2				;allocate 3 bytes for a variabke named ROOT_RESULT1
ROOT_RESULT2:	.byte 2				;allocate 3 bytes for a variabke named ROOT_RESULT2
ROOT_RESULT3:	.byte 2				;allocate 3 bytes for a variabke named ROOT_RESULT3





;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment
;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt
.org	$0046					; End of Interrupt Vectors
;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:	; The initialization routine
		clr		zero
		clr		best

; To do
; your code goes here
;initial stack pointer
		ldi mpr, low(RAMEND)	;load low bits of RAMEND into mpr
		out SPL, mpr			;output mpr into stack pointer low
		ldi mpr, high(RAMEND)	;load high bits of RAMEND into mpr
		out SPH, mpr			;output mpr into stack pointer high
		
		;first number
		ldi ZL, low(TreasureInfo <<1)	;loade low byte address into Z
		ldi ZH, high(TreasureInfo <<1)	;loade high byte address into Z
		ldi YL, low(operand1)			;load low byte address into Y
		ldi YH, high(operand1)			;load high byte address into Y
		lpm mpr, Z+						;load memory program by Z low
		lpm mpr0, Z						;load memory program by Z
		lsl mpr0						;shift left 0
		rol mpr							;rotate left 0
		rol best						;rotate left to best 0
		lsl mpr0						;shift letf 1
		rol mpr							;rotate left 1
		rol best						;rotate left 0
		st Y+, mpr						;store mpr to YL
		st Y, best						;store best register to Y	
		;first number end
		
		;second number
		ldi YL, low(operand2)			;load low byte adress into Y
		ldi YH, high(operand2)			;load high byte address into Y
		lpm mpr, Z+						;load memory program by Z low
		lpm mpr0, Z						;load memory program by Z
		lsl mpr0						;left shift 1
		rol mpr							;rotate left 1
		rol best						;rotate left 1 to best
		mov best, zero					;move to zero
		lsl	mpr0						;left shift 0
		rol mpr							;rotate left 0 
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shitf 0
		rol mpr							;rotate left 0
		rol best						;rortate left 0
		lsl mpr0						;left shift 1
		rol mpr							;rotate left 1
		rol best						;rotate left 1
		st Y+, mpr						;store mpr to Y low
		st Y, best						;store best to Y
		;scond number end
		
		;third number
		ldi YL, low(operand3)			;load low byte adress into Y
		ldi YH, high(operand3)			;load high byte address into Y
		lpm mpr, Z+						;load memory program by Z low
		lpm mpr0, Z						;load memory program by Z
		lsl mpr0						;left shift 1
		rol mpr							;rotate left 1
		rol best						;rotate left 1 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		st Y+, mpr						;store mpr to Y low
		st Y, best						;store best to Y
		;third number end
		
		;fourth number
		ldi YL, low(operand4)			;load low byte adress into Y
		ldi YH, high(operand4)			;load high byte address into Y
		lpm mpr, Z+						;load memory program by Z low
		lpm mpr0, Z						;load memory program by Z
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 1
		rol mpr							;rotate left 1
		rol best						;rotate left 1 to best
		lsl mpr0						;left shift 0
		rol mpr							;rotate left 0
		rol best						;rotate left 0 to best
		st Y+, mpr						;store mpr to Y low
		st Y, best						;store best to Y
		;fourth numbe end
		
		;fifth number
		ldi YL, low(operand5)			;load low byte adress into Y
		ldi YH, high(operand5)			;load high byte address into Y
		lpm mpr, Z+						;load memory program by Z low
		lpm mpr, Z+
		st Y+, mpr						;store mpr to Y low
		;fifth number end
	
		;sixth number
		ldi YL, low(operand6)			;load low byte adress into Y
		ldi YH, high(operand6)			;load high byte address into Y
		lpm mpr, Z+						;load memory program by Z low
		lpm mpr0, Z						;load memory program by Z
		lsl mpr0						;left shift 1
		rol mpr							;rotate left 1
		rol best						;rotate left 1 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 1
		rol mpr							;rotate left 1
		rol best						;rotate left 1 to best
		mov best, zero					;move to zero
		lsl mpr0						;left shift 1
		rol mpr							;rotate left 1
		rol best						;rotate left  0 to best
		lsl mpr0						;left shift 1
		rol mpr							;rotate left 1
		rol best						;rotate left 1 to best
		st Y+, mpr						;store mpr to Y low
		st Y, best						;store best to Y
		;sixth number end
;-----------------------------------------------------------
; MAIN PROGRAM
;-----------------------------------------------------------
MAIN:
		;MUL16
		;operand1^2
		rcall	MUL1
		;operand2^2
		rcall	MUL2
		;operand3^2
		rcall	MUL3
		;operand4^2
		rcall	MUL4
		;operand5^2
		rcall	MUL5
		;operand6^2
		rcall	MUL6

		;ADD32
		;ADD MUL_RESULT1 and MUL_RESULT2
		rcall	ADD1
		;ADD MUL_RESULT3 and MUL_RESULT4
		rcall	ADD2
		;ADD MUL_RESULT5 and MUL_RESULT6
		rcall	ADD3

		;Root40
		;ROOT square ADD_RESULT1
		rcall ROOT1
		;ROOT square ADD_RESULT2
		rcall ROOT2
		;ROOT square ADD_RESULT3
		rcall ROOT3

		;BEST CHOICE
		rcall CHOICE

		;AVERAGE Distance
		rcall DISTANCE
		
	

 		jmp	Grading

;***********************************************************
;*	Procedures and Subroutines
;***********************************************************
; your code can go here as well
;***********************************************************
;*	Function: MUL1
;*  Describtion: cactulate operand1^2
;*	reference: lab5 MUL16 fucntion
;***********************************************************
MUL1:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(operand1)	; Load low byte
		ldi		YH, high(operand1)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL_RESULT1)	; Load low byte
		ldi		ZH, high(MUL_RESULT1); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL1_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(operand1)	; Load low byte
		ldi		XH, high(operand1)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL1_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL1_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL1_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;***********************************************************
;*	Function: MUL2
;*  Describtion: cactulate operand2^2
;*	reference: lab5 MUL16 fucntion
;***********************************************************
MUL2:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(operand2)	; Load low byte
		ldi		YH, high(operand2)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL_RESULT2)	; Load low byte
		ldi		ZH, high(MUL_RESULT2); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL2_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(operand2)	; Load low byte
		ldi		XH, high(operand2)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL2_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL2_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL2_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;***********************************************************
;*	Function: MUL3
;*  Describtion: cactulate operand3^2
;*	reference: lab5 MUL16 fucntion
;***********************************************************
MUL3:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(operand3)	; Load low byte
		ldi		YH, high(operand3)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL_RESULT3)	; Load low byte
		ldi		ZH, high(MUL_RESULT3); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL3_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(operand3)	; Load low byte
		ldi		XH, high(operand3)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL3_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL3_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL3_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;***********************************************************
;*	Function: MUL4
;*  Describtion: cactulate operand4^2
;*	reference: lab5 MUL16 fucntion
;***********************************************************
MUL4:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(operand4)	; Load low byte
		ldi		YH, high(operand4)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL_RESULT4)	; Load low byte
		ldi		ZH, high(MUL_RESULT4); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL4_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(operand4)	; Load low byte
		ldi		XH, high(operand4)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL4_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL4_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL4_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;***********************************************************
;*	Function: MUL5
;*  Describtion: cactulate operand5^2
;*	reference: lab5 MUL16 fucntion
;***********************************************************
MUL5:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(operand5)	; Load low byte
		ldi		YH, high(operand5)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL_RESULT5)	; Load low byte
		ldi		ZH, high(MUL_RESULT5); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL5_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(operand5)	; Load low byte
		ldi		XH, high(operand5)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL5_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL5_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL5_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET


;***********************************************************
;*	Function: MUL6
;*  Describtion: cactulate operand6^2
;*	reference: lab5 MUL16 fucntion
;***********************************************************
MUL6:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(operand6)	; Load low byte
		ldi		YH, high(operand6)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL_RESULT6)	; Load low byte
		ldi		ZH, high(MUL_RESULT6); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL6_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(operand6)	; Load low byte
		ldi		XH, high(operand6)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL6_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL6_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL6_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET


;***********************************************************
;*	Function: ADD1
;*  Describtion: cactulate sqaure operand1 + square operand2
;*	reference: lab5 ADD16 fucntion
;***********************************************************
ADD1:
		; Load beginning address of first operand into X
		ldi		XL, low(MUL_RESULT1)	; Load low byte of address
		ldi		XH, high(MUL_RESULT1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(MUL_RESULT2)	;load low byte of address
		ldi		YH, high(MUL_RESULT2)	;load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(Result1);load low byte of address
		ldi		ZH, high(Result1);load high byte of address

		; Execute the function
		ld		A, X+	;load byte of first operand, post increment
		ld		B, Y+	;load byte of second operand, post increment
		add		A, B	;add the two oprands together
		st		Z+, A	;store the sum into Z, post increment
		ld		A, X+	;load byte of first operand
		ld		B, Y+	;Load byte of second operand
		adc		A, B	;add upper byte with carry
		st		Z+, A	;store the sum into Z, post increment
		ld		A, X+	;load byte of first operand
		ld		B, Y+	;Load byte of second operand
		adc		A, B	;add upper byte with carry
		st		Z+, A	;store the sum into Z, post increment
		clr		A		;clear A
		adc		A, zero	;add A with carry bit
		st		Z, A	;store result into Z  
		ret						; End a function with RET


;***********************************************************
;*	Function: ADD2
;*  Describtion: cactulate sqaure operand3 + square operand4
;*	reference: lab5 ADD16 fucntion
;***********************************************************
ADD2:
		; Load beginning address of first operand into X
		ldi		XL, low(MUL_RESULT3)	; Load low byte of address
		ldi		XH, high(MUL_RESULT3)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(MUL_RESULT4)	;load low byte of address
		ldi		YH, high(MUL_RESULT4)	;load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(Result2);load low byte of address
		ldi		ZH, high(Result2);load high byte of address

		; Execute the function
		ld		A, X+	;load byte of first operand, post increment
		ld		B, Y+	;load byte of second operand, post increment
		add		A, B	;add the two oprands together
		st		Z+, A	;store the sum into Z, post increment
		ld		A, X+	;load byte of first operand
		ld		B, Y+	;Load byte of second operand
		adc		A, B	;add upper byte with carry
		st		Z+, A	;store the sum into Z, post increment
		ld		A, X+	;load byte of first operand
		ld		B, Y+	;Load byte of second operand
		adc		A, B	;add upper byte with carry
		st		Z+, A	;store the sum into Z, post increment
		clr		A		;clear A
		adc		A, zero	;add A with carry bit
		st		Z, A	;store result into Z  
		ret						; End a function with RET


;***********************************************************
;*	Function: ADD3
;*  Describtion: cactulate sqaure operand5 + square operand6
;*	reference: lab5 ADD16 fucntion
;***********************************************************
ADD3:
		; Load beginning address of first operand into X
		ldi		XL, low(MUL_RESULT5)	; Load low byte of address
		ldi		XH, high(MUL_RESULT5)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(MUL_RESULT6)	;load low byte of address
		ldi		YH, high(MUL_RESULT6)	;load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(Result3);load low byte of address
		ldi		ZH, high(Result3);load high byte of address

		; Execute the function
		ld		A, X+	;load byte of first operand, post increment
		ld		B, Y+	;load byte of second operand, post increment
		add		A, B	;add the two oprands together
		st		Z+, A	;store the sum into Z, post increment
		ld		A, X+	;load byte of first operand
		ld		B, Y+	;Load byte of second operand
		adc		A, B	;add upper byte with carry
		st		Z+, A	;store the sum into Z, post increment
		ld		A, X+	;load byte of first operand
		ld		B, Y+	;Load byte of second operand
		adc		A, B	;add upper byte with carry
		st		Z+, A	;store the sum into Z, post increment
		clr		A		;clear A
		adc		A, zero	;add A with carry bit
		st		Z, A	;store result into Z  
		ret						; End a function with RET

;***********************************************************
;*	Function: ROOT1
;*  Describtion: cactulate sqaure root  result x^2+y^2
;***********************************************************
ROOT1:
	ldi YL, low(Result1)		;low byte of Result x^2+y^2
	ldi YH, high(Result1)		;high byte of Result x^2+y^2
	ldi ZL, low (Result1)		;low byte of root  
	ldi ZH, high(Result1)		;high byte of root

	ld A, Y+					;load low byte of result x^2+y^2
	ld B, Y+					;load low byte of result x^2+y^2
	ld C, Y						;load high byte of result x^2+y^2
	ldi best, 0					
	ldi counter_low, 1			;load 1 to counter low
	ldi counter_high, 0			;load 0 to counter high
	ldi root_low, 0				;load 0 to root low
	ldi root_high, 0			;load 0 to root low 


ROOT_LOOP1:
	sub A, counter_low				;sub A 
	sbc B, best						;Subtract with Carry
	sbc C, best						;Subtract with Carry
	brcs ROOT_GET_G1				;if A < counter_low
	breq ROOT_GET1					; if A =  counter_low
	cpi counter_low, 255			;compare conter_low and 255
	breq ROOT_LOW1					;if counter_low,255
	inc counter_low					;counter_low incresement
	inc counter_low					;counter_low incresement
	inc root_low					;root_low incresement
	rjmp ROOT_LOOP1					;re loop

ROOT_HIGH1:
	inc root_high					;high byte root increse
	clr root_low					;clear low byte root 
	rjmp ROOT_LOOP_LOOP1					;re loop
	
ROOT_LOW1:
	inc counter_high				;counter_high byte ;	
	clr counter_low					;clear counter low byte
	inc counter_low					;re start sub 
	sub A, counter_low				;sub A and counter_low
	sbc B, counter_high				; high byte sub
	sbc C, best						;Subtract with Carry
	inc root_low
	cpi root_low, 255				;compare conter_low and 255
	breq ROOT_HIGH1					;if root_low,255

ROOT_LOOP_LOOP1:
	cpi counter_low, 255			;compare conter_low and 255
	breq ROOT_LOW1					;if counter_low,255
	inc counter_low					;counter_low incresement
	inc counter_low					;counter_low incresement
	sub A, counter_low				;sub A and counter_low
	sbc B, counter_high				; high byte sub
	sbc C, best						;Subtract with Carry
	brcs ROOT_GET1					;if A < counter_low
	breq ROOT_GET_G1				; if A =  counter_low
	inc root_low
	cpi root_low, 255				;compare conter_low and 255
	breq ROOT_HIGH1					;if root_low,255
	rjmp ROOT_LOOP_LOOP1

ROOT_GET1:
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	st Z+, root_low					;store root_low to Z+
	st Z, root_high					;store root_high to Z	
	ret								; End a function with RET

ROOT_GET_G1:
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	inc root_low
	st Z+, root_low					;store root_low to Z+
	st Z, root_high					;store root_high to Z
	ret								; End a function with RET

;***********************************************************
;*	Function: ROOT2
;*  Describtion: cactulate sqaure root  result x^2+y^2
;***********************************************************
ROOT2:
	ldi YL, low(Result2)		;low byte of Result x^2+y^2
	ldi YH, high(Result2)		;high byte of Result x^2+y^2
	ldi ZL, low (Result2)		;low byte of root  
	ldi ZH, high(Result2)		;high byte of root

	ld A, Y+					;load low byte of result x^2+y^2
	ld B, Y+					;load low byte of result x^2+y^2
	ld C, Y						;load high byte of result x^2+y^2
	ldi best, 0					
	ldi counter_low, 1			;load 1 to counter low
	ldi counter_high, 0			;load 0 to counter high
	ldi root_low, 0				;load 0 to root low
	ldi root_high, 0			;load 0 to root low 


ROOT_LOOP2:
	sub A, counter_low				;sub A 
	sbc B, best						;Subtract with Carry
	sbc C, best						;Subtract with Carry
	brcs ROOT_GET_G2					;if A < counter_low
	breq ROOT_GET2					; if A =  counter_low
	cpi counter_low, 255			;compare conter_low and 255
	breq ROOT_LOW2					;if counter_low,255
	inc counter_low					;counter_low incresement
	inc counter_low					;counter_low incresement
	inc root_low					;root_low incresement
	rjmp ROOT_LOOP2					;re loop
ROOT_HIGH2:
	inc root_high					;high byte root increse
	clr root_low					;clear low byte root 
	rjmp ROOT_LOOP_LOOP2					;re loop
	
ROOT_LOW2:
	inc counter_high				;counter_high byte ;	
	clr counter_low					;clear counter low byte
	inc counter_low					;re start sub 
	sub A, counter_low				;sub A and counter_low
	sbc B, counter_high				; high byte sub
	sbc C, best						;Subtract with Carry
	inc root_low
	cpi root_low, 255				;compare conter_low and 255
	breq ROOT_HIGH2					;if root_low,255

ROOT_LOOP_LOOP2:
	cpi counter_low, 255			;compare conter_low and 255
	breq ROOT_LOW2					;if counter_low,255
	inc counter_low					;counter_low incresement
	inc counter_low					;counter_low incresement
	sub A, counter_low				;sub A and counter_low
	sbc B, counter_high				; high byte sub
	sbc C, best						;Subtract with Carry
	brcs ROOT_GET2					;if A < counter_low
	breq ROOT_GET_G2					; if A =  counter_low
	inc root_low
	cpi root_low, 255				;compare conter_low and 255
	breq ROOT_HIGH2					;if root_low,255
	rjmp ROOT_LOOP_LOOP2

ROOT_GET2:
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	st Z+, root_low					;store root_low to Z+
	st Z, root_high					;store root_high to Z	
	ret								; End a function with RET

ROOT_GET_G2:
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	inc root_low
	st Z+, root_low					;store root_low to Z+
	st Z, root_high					;store root_high to Z
	ret								; End a function with RET	

;***********************************************************
;*	Function: ROOT3
;*  Describtion: cactulate sqaure root  result x^2+y^2
;***********************************************************
ROOT3:
	ldi YL, low(Result3)		;low byte of Result x^2+y^2
	ldi YH, high(Result3)		;high byte of Result x^2+y^2
	ldi ZL, low (Result3)		;low byte of root  
	ldi ZH, high(Result3)		;high byte of root

	ld A, Y+					;load low byte of result x^2+y^2
	ld B, Y+					;load low byte of result x^2+y^2
	ld C, Y						;load high byte of result x^2+y^2
	ldi best, 0					
	ldi counter_low, 1			;load 1 to counter low
	ldi counter_high, 0			;load 0 to counter high
	ldi root_low, 0				;load 0 to root low
	ldi root_high, 0			;load 0 to root low 


ROOT_LOOP3:
	sub A, counter_low				;sub A 
	sbc B, best						;Subtract with Carry
	sbc C, best						;Subtract with Carry
	brcs ROOT_GET_G3				;if A < counter_low
	breq ROOT_GET3					; if A =  counter_low
	cpi counter_low, 255			;compare conter_low and 255
	breq ROOT_LOW3					;if counter_low,255
	inc counter_low					;counter_low incresement
	inc counter_low					;counter_low incresement
	inc root_low					;root_low incresement
	rjmp ROOT_LOOP3					;re loop
ROOT_HIGH3:
	inc root_high					;high byte root increse
	clr root_low					;clear low byte root 
	rjmp ROOT_LOOP_LOOP3					;re loop
	
ROOT_LOW3:
	inc counter_high				;counter_high byte ;	
	clr counter_low					;clear counter low byte
	inc counter_low					;re start sub 
	sub A, counter_low				;sub A and counter_low
	sbc B, counter_high				; high byte sub
	sbc C, best						;Subtract with Carry
	inc root_low
	cpi root_low, 255				;compare conter_low and 255
	breq ROOT_HIGH3					;if root_low,255

ROOT_LOOP_LOOP3:
	cpi counter_low, 255			;compare conter_low and 255
	breq ROOT_LOW3					;if counter_low,255
	inc counter_low					;counter_low incresement
	inc counter_low					;counter_low incresement
	sub A, counter_low				;sub A and counter_low
	sbc B, counter_high				; high byte sub
	sbc C, best						;Subtract with Carry
	brcs ROOT_GET3					;if A < counter_low
	breq ROOT_GET_G3				; if A =  counter_low
	inc root_low
	cpi root_low, 255				;compare conter_low and 255
	breq ROOT_HIGH3					;if root_low,255
	rjmp ROOT_LOOP_LOOP3

ROOT_GET3:
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	st Z+, root_low					;store root_low to Z+
	st Z, root_high					;store root_high to Z	
	ret								; End a function with RET

ROOT_GET_G3:
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	lpm mpr, Z+						;load memory program by Z+
	inc root_low
	st Z+, root_low					;store root_low to Z+
	st Z, root_high					;store root_high to Z
	ret								; End a function with RET

;******************************************************************
;*	Function: CHOICE
;*  Describtion: Find the closerest distance
;******************************************************************
CHOICE:
		ldi XL, low(Result1)	;low byte of Result1
		ldi XH, high(Result1)	;high byte of Result1
		ldi YL, low	(Result2)	;low byte of Result2
		ldi YH, high(Result2)	;high byte of Result2
		ldi ZL, low (Result3)	;low byte of Result3
		ldi ZH, high(Result3)	;high byte of Result3

		ld A, X+		;move byte
		ld A, X+		;move byte
		ld A, X+		;move byte
		ld A, X+		;get low btye of Result1
		ld B, X			;get high byte of Result1
		ld C, Y+		;move byte 
		ld C, Y+		;move byte
		ld C, Y+		;move byte
		ld C, Y+		;get low btye of Result2
		ld D, Y			;get high byte of Result2
		ld E, Z+		;move byte
		ld E, Z+		;move byte
		ld E, Z+		;move byte
		ld E, Z+		;get low btye of Result3
		ld F, Z			; get high byte of Result3			
		ldi best, 0

COMPARE_BD:
		cp	B,D					;compare high byte
		brcs COMPARE_BF		    ;B<D picke BA and cp EF
		breq CPMARE_LOW_AC		;B=D TO  CP A C
		rjmp COMPARE_DF			;B>D Pick D

CPMARE_LOW_AC:
		cp A, C					;compare low byte
		brcs COMPARE_BF			;A<C compare bf
		breq COMPARE_BF			;A=C continue cp thrid point
		rjmp COMPARE_DF			;A>C compare df

COMPARE_BF:
		cp B,F ;compare high byte BF
		brcs CHOICE_AB		;B<F  TO pick AB
		breq COMPARE_LOW_AE	;B=F TO cp AE
		rjmp CHOICE_EF		;B>F to pick EF	

COMPARE_LOW_AE:
		cp A, E ;cpmpare low byte A E
		brcs CHOICE_AB			;A<E pick AB
		breq COMPARE_SAME		; A=E  is same
		rjmp CHOICE_EF			; A>E PICK EF

COMPARE_DF:
		cp D, F			;COMPARE HIGH BYTE D F
		brcs CHOICE_CD			;D<F pick cd
		breq COMPARE_LOW_CE		; D=F cp low byte
		rjmp CHOICE_EF			; D>F pick EF

COMPARE_LOW_CE:
		cp C, E			;compare low byte ce
		brcs CHOICE_CD			;C<E pick cd
		breq COMPARE_SAME		;C=E
		rjmp CHOICE_EF			;C>F pick ef

CHOICE_AB:
	ldi	XL,low (BestChoice)		;loade low byte
	ldi XH, high(BestChoice)	;loade high byte
	ldi best, 1					;lode value
	st X+, best
	ret

CHOICE_CD:
	ldi	YL,low (BestChoice)		;loade low byte
	ldi YH, high(BestChoice)	;loade high byte
	ld best, Y+					;load byte
	ldi best, 2					;lode value
	st Y+, best					;store value
	ret							;end 

CHOICE_EF:
	ldi	ZL,low (BestChoice)		;loade low byte
	ldi ZH, high(BestChoice)	;loade high byte
	ld best, Z+					;load byte
	ldi best, 3					;lode value
	st Z+, best					;store value
	ret							;end
COMPARE_SAME:
	ldi	XL,low (BestChoice)		;loade low byte
	ldi XH, high(BestChoice)	;loade high byte
	ld best, X+					;load byte
	ldi best, -1				;store value
	st X+, best					;store value
	ret							;end


;******************************************************************
;*	Function: Distance
;*  Describtion: cactulate the average distance of Three distance
;******************************************************************
DISTANCE:
		;get tatal distance
		rcall ADD_Distance1 
		rcall ADD_Distance2

		ldi	YL, low(ADD_RESULT2)	;load low byte of address
		ldi YH, high(ADD_RESULT2)	;load high byte of address

		ldi ZL, low(AvgDistance)	;load low byte of address
		ldi ZH, high(AvgDistance)	;load high byte of address

		ld	A, Y+	;load low byte 
		ld	B, Y		;load high byte
		ldi best, 0					
		ldi counter_low, 3			;load 1 to counter low
		ldi root_low, 0				;load 0 to root low
		ldi root_high, 0			;load 0 to root low 

DISTANCE_LOOP:
	sub A, counter_low				;sub A 
	sbc B, best						;Subtract with Carry
	brcs DIS_LOOP1					;if A < counter_low
	breq DIS_LOOP1					; if A =  counter_low
	inc  root_low					;increse root_low
	cpi root_low, 255				;compare conter_low and 255
	breq DIS_HIGH					;if counter_low,255
	rjmp DISTANCE_LOOP				;re loop

DIS_HIGH:
	inc root_high					;high byte root increse
	clr root_low					;clear low byte root 
	rjmp DISTANCE_LOOP					;re loop

DIS_LOOP1:
	st Z+, root_low					;store root_low to Z+
	st Z, root_high					;store root_high to Z
	ret								; End a function with RET

	
;******************************************************************
;*	Function: ADD_Distance1
;*  Describtion: cactulate add result1 distance + result2 distance
;******************************************************************
ADD_Distance1:
		ldi		XL, low(Result1)	; Load low byte of address
		ldi		XH, high(Result1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(Result2)	;load low byte of address
		ldi		YH, high(Result2)	;load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(ADD_RESULT1);load low byte of address
		ldi		ZH, high(ADD_RESULT1);load high byte of address

		; Execute the function
		ld		A, X+	;load first byte of first operand, post increment	
		ld		A, X+	;load first byte of first operand, post increment
		ld		A, X+	;load first byte of first operand, post increment
		ld		A, X+	;load first byte of first operand, post increment
		ld		B, Y+	;load first byte of second operand, post increment
		ld		B, Y+	;load first byte of second operand, post increment
		ld		B, Y+	;load first byte of second operand, post increment
		ld		B, Y+	;load first byte of second operand, post increment
		add		A, B	;add the two oprands together
		st		Z+, A	;store the sum into Z, post increment
		ld		A, X	;load second byte of first operand
		ld		B, Y	;Load second byte of second operand
		adc		A, B	;add upper byte with carry
		st		Z+, A	;store the sum into Z, post increment
		clr		A		;clear A
		adc		A, zero	;add A with carry bit
		st		Z, A	;store result into Z  

		
		ret						; End a function with RET

;******************************************************************
;*	Function: ADD_Distance2
;*  Describtion: cactulate add result1 distance + result2 distance
;******************************************************************
ADD_Distance2:
		ldi		XL, low(ADD_RESULT1)	; Load low byte of address
		ldi		XH, high(ADD_RESULT1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(Result3)	;load low byte of address
		ldi		YH, high(Result3)	;load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(ADD_RESULT2);load low byte of address
		ldi		ZH, high(ADD_RESULT2);load high byte of address

		; Execute the function
		ld		A, X+	;load first byte of first operand, post increment
		ld		B, Y+	;load first byte of second operand, post increment
		ld		B, Y+	;load first byte of second operand, post increment
		ld		B, Y+	;load first byte of second operand, post increment
		ld		B, Y+	;load first byte of second operand, post increment
		add		A, B	;add the two oprands together
		st		Z+, A	;store the sum into Z, post increment
		ld		A, X	;load second byte of first operand
		ld		B, Y	;Load second byte of second operand
		adc		A, B	;add upper byte with carry
		st		Z+, A	;store the sum into Z, post increment
		clr		A		;clear A
		adc		A, zero	;add A with carry bit
		st		Z, A	;store result into Z  

		
		ret						; End a function with RET


;***end of your code***end of your code***end of your code***end of your code***end of your code***
;******************************* Do not change below this point************************************
;******************************* Do not change below this point************************************
;******************************* Do not change below this point************************************

Grading:
		nop					; Check the results and number of cycles (The TA will set a breakpoint here)
rjmp Grading


;***********************************************************
;*	Stored Program Data
;***********************************************************

; Contents of program memory will be changed during testing
; The label names (Treasures, UserLocation) are not changed
; See the lab instructions for an explanation of TreasureInfo. The 10 bit values are packed together.
; In this example, the three treasures are located at (5, 25), (35, -512), and (0, 511)
TreasureInfo:	.DB	0x01, 0x41, 0x90, 0x8E, 0x00, 0x00, 0x1F, 0xF0		
;UserLocation:	.DB 0x00, 0x00, 0x00 	; this is only used for the challenge code

;***********************************************************
;*	Data Memory Allocation for Results
;***********************************************************
.dseg
.org	$0E00						; data memory allocation for results - Your grader only checks $0E00 - $0E11
Result1:		.byte 5				; x2_plus_y2, square_root (for treasure 1)
Result2:		.byte 5				; x2_plus_y2, square_root (for treasure 2)
Result3:		.byte 5				; x2_plus_y2, square_root (for treasure 3)
BestChoice:		.byte 1				; which treasure is closest? (indicate this with a value of 1, 2, or 3)
									; this should have a value of -1 in the special case when the 3 treasures
									; have an equal (rounded) distance
AvgDistance:	.byte 2				; the average distance to a treasure chest (rounded upward if the value was not already an integer)

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
