;
; Wei_Huang_Lab5_sourcecode.asm
;
; Created: 11/1/2020 6:38:30 PM
; Author : Huang, Wei
;

;***********************************************************
;*
;*	Wei_Huang_Lab5_sourcecode.asm
;*
;*	This program inlcude a 16-bit addtion, 16-bit subtraction, 24-bit multipucation 
;*
;*	This is the skeleton file for Lab 5 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Wei Huang
;*	   Date: 11/1/2020 6:38:30 PM
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter


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
INIT:							; The initialization routine
		; Initialize Stack Pointer
		; TODO					; Init the 2 stack pointer registers
		ldi mpr, low(RAMEND)	; Load low bits of RAMEND into mpr
		out SPL, mpr			; output mpr into stack pointer low
		ldi mpr, high(RAMEND)	; load high bits of RAMEND into mpr
		out SPH, mpr			; output mpr into stack pointer high

		clr		zero			; Set the zero register to zero, maintain
								; these semantics, meaning, don't
								; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program
		; Setup the ADD16 function direct test
		ldi YL, low(ADD16_OP1)		;Load low byte address into Y 
		ldi	YH, high(ADD16_OP1)		;Load high byte address into Y
		ldi ZL, low(OperandA<<1)	;load low byte address into Z
		ldi ZH, high(OperandA<<1)	;loda low byte address into Z
		lpm mpr, Z+					;load byte from memory to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm mpr, Z+					;load byte from memory to mpr,post-increment
		st  Y+, mpr					;store post-increment into Y   
		ldi YL, low(ADD16_OP2)		;Load low byte address into Y 
		ldi	YH, high(ADD16_OP2)		;Load high byte address into Y 
		ldi	ZL, low(OperandB<<1)	;load low byte address into Z
		ldi ZH, high(OperandB<<1)	;load lowe byte address into Z
		lpm	mpr, Z+					;load byte from memort to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm	mpr, Z+					;load byte from memory to mpr,post-increment  
		st	Y+, mpr					;store post-increment into Y                                        
				; Move values 0xFCBA and 0xFFFF in program memory to data memory
				; memory locations where ADD16 will get its inputs from
				; (see "Data Memory Allocation" section below)

                nop ; Check load ADD16 operands (Set Break point here #1)  
				; Call ADD16 function to test its correctness
				; (calculate FCBA + FFFF)
					rcall ADD16
                nop ; Check ADD16 result (Set Break point here #2)
				; Observe result in Memory window

		; Setup the SUB16 function direct test
		ldi YL, low(SUB16_OP1)		;Load low byte address into Y 
		ldi	YH, high(SUB16_OP1)		;Load high byte address into Y
		ldi ZL, low(OperandC<<1)	;load low byte address into Z
		ldi ZH, high(OperandC<<1)	;loda low byte address into Z
		lpm mpr, Z+					;load byte from memory to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm mpr, Z+					;load byte from memory to mpr,post-increment
		st  Y+, mpr					;store post-increment into Y   
		ldi YL, low(SUB16_OP2)		;Load low byte address into Y 
		ldi	YH, high(SUB16_OP2)		;Load high byte address into Y 
		ldi	ZL, low(OperandG<<1)	;load low byte address into Z
		ldi ZH, high(OperandG<<1)	;load lowe byte address into Z
		lpm	mpr, Z+					;load byte from memort to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm	mpr, Z+					;load byte from memory to mpr,post-increment 
		st	Y+, mpr					;store byte into Y   
				; Move values 0xFCB9 and 0xE420 in program memory to data memory
				; memory locations where SUB16 will get its inputs from

                nop ; Check load SUB16 operands (Set Break point here #3)  
				; Call SUB16 function to test its correctness
				; (calculate FCB9 - E420)
				rcall SUB16

                nop ; Check SUB16 result (Set Break point here #4)
				; Observe result in Memory window

		; Setup the MUL24 function direct test
		ldi YL, low(MUL24_OP1)		;Load low byte address into Y 
		ldi	YH, high(MUL24_OP1)		;Load high byte address into Y
		ldi ZL, low(OperandH<<1)	;load low byte address into Z
		ldi ZH, high(OperandH<<1)	;loda low byte address into Z
		lpm mpr, Z+					;load byte from memory to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm mpr, Z+					;load  byte from memory to mpr,post-increment
		st  Y+, mpr					;store post-increment into Y   
		lpm mpr, Z+					;rinse and repead
		st	Y+, mpr					;store final value

		ldi YL, low(MUL24_OP2)		;Load low byte address into Y 
		ldi	YH, high(MUL24_OP2)		;Load high byte address into Y 
		ldi	ZL, low(OperandK<<1)	;load low byte address into Z
		ldi ZH, high(OperandK<<1)	;load lowe byte address into Z
		lpm	mpr, Z+					;load byte from memort to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm	mpr, Z+					;load byte from memory to mpr,post-increment 
		st	Y+, mpr					;store post-increment into Y
		lpm	mpr, Z+					;rinse and repead
		st	Y+, mpr					;store final value
				; Move values 0xFFFFFF and 0xFFFFFF in program memory to data memory  
				; memory locations where MUL24 will get its inputs from

                nop ; Check load MUL24 operands (Set Break point here #5)  
				; Call MUL24 function to test its correctness
				; (calculate FFFFFF * FFFFFF)
				rcall MUL24

                nop ; Check MUL24 result (Set Break point here #6)
				; Observe result in Memory window

		; Setup the COMPOUND function direct test
		ldi YL, low(COMPOUND_OP1)		;Load low byte address into Y 
		ldi	YH, high(COMPOUND_OP1)		;Load high byte address into Y
		ldi ZL, low(OperandD<<1)	;load low byte address into Z
		ldi ZH, high(OperandD<<1)	;loda low byte address into Z
		lpm mpr, Z+					;load byte from memory to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm mpr, Z+					;load byte from memory to mpr,post-increment
		st  Y+, mpr					;store post-increment into Y    
		ldi	ZL, low(OperandE<<1)	;load low byte address into Z
		ldi ZH, high(OperandE<<1)	;load lowe byte address into Z
		lpm	mpr, Z+					;load byte from memort to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm	mpr, Z+					;load byte from memory to mpr,post-increment 
		st	Y+, mpr					;store byte into Y  
		ldi YL, low(COMPOUND_OP2)		;Load low byte address into Y 
		ldi	YH, high(COMPOUND_OP2)		;Load high byte address into Y
		ldi ZL, low(OperandF<<1)	;load low byte address into Z
		ldi ZH, high(OperandF<<1)	;loda low byte address into Z
		lpm mpr, Z+					;load byte from memory to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm mpr, Z+					;load byte from memory to mpr,post-increment
		st  Y+, mpr					;store post-increment into Y   

                nop ; Check load COMPOUND operands (Set Break point here #7)  
		; Call the COMPOUND function
		rcall COMPOUND
                nop ; Check COMPUND result (Set Break point here #8)
				; Observe final result in Memory window

DONE:	rjmp	DONE			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;		where the high byte of the result contains the carry
;		out bit.
;-----------------------------------------------------------
ADD16:
		; Load beginning address of first operand into X
		ldi		XL, low(ADD16_OP1)	; Load low byte of address
		ldi		XH, high(ADD16_OP1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(ADD16_OP2)	;load low byte of address
		ldi		YH, high(ADD16_OP2)	;load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(ADD16_RESULT);load low byte of address
		ldi		ZH, high(ADD16_RESULT);load high byte of address

		; Execute the function
		ld		A, X+	;load first byte of first operand, post increment
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

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;		result.
;-----------------------------------------------------------
SUB16:
		
		;Load beginning address of first operand into X
		ldi		XL, low(SUB16_OP1)	;load low byte of address
		ldi		XH, high(SUB16_OP1)	;load high byte of address

		;Load beginning address of second operand into Y
		ldi		YL, low(SUB16_OP2)	;load low byte of address
		ldi		YH, high(SUB16_OP2)	;load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(SUB16_RESULT);load low byte of address
		ldi		ZH, high(SUB16_RESULT);load high byte of address

		; Execute the function here
		ld		A, X+			;load first byte of first operand, post increment
		ld		B, Y+			;load first byte of second operand, post increment
		sub		A, B			;subtract value, first from second
		st		Z+, A			;store result into Z, post increment
		ld		A, X			;load sceond byte of second operand
		ld		B, Y			;load second byte of second operand
		sub		A, B			;two operands subtract with carry
		st		Z, A			;store the result into Z

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit 
;		result.
;-----------------------------------------------------------
MUL24:
		; Execute the function here
		push A					;save A register
		push B					;save B register
		push rhi				;save rhi register
		push rlo				;save rlo register
		push zero				;save zero register 
		push XH					;save X-ptr
		push XL
		push YH					;save Y-ptr
		push YL
		push ZH					;save Z-ptr
		push ZL
		push oloop				;save oloop register
		push iloop				;save iloop register

		clr		zero			;Maintain zero semantics

		ldi		YL, low(MUL24_OP2)			;load low byte
		ldi		YH, high(MUL24_OP2)		;load high byte	
		ldi		ZL, low(MUL24_RESULT)		;load low byte
		ldi		ZH, high(MUL24_RESULT)		;load high byte

		ldi		oloop, 3					;load counter
MUL24_OLOOP:
		ldi		XL, low(MUL24_OP1)			;load low btye
		ldi		XH, high(MUL24_OP1)			;load high byte
		
		;begin inner loop
		ldi		iloop, 3					;load counter
		MUL24_ILOOP:
			ld		A, X+			;byte of first operand
			ld		B, Y			; byte of second operand
			mul		A, B			;mul A and B
			ld		A, Z+			;get result byte
			ld		B, Z+			;get next result byte
			add		rlo, A			;add A to rlo
			adc		rhi, B			;add B to rhi with carry
			ld		A, Z			;get third byte from result
			adc		A, zero			; add carry to A
			st		Z+, A			;store byte to memory
			ld		A, Z			;get byte from result
			adc		A, zero			;add carry to A
			st		Z, A			;store byte to memory
			sbiw	ZH:ZL, 2		;Z<-Z-2
			st		-Z, rhi			;store second byte
			st		-Z, rlo			;store first byte
			adiw	ZH:ZL, 1		;Z<-Z+1
			dec		iloop			;decrement counter
			brne	MUL24_ILOOP		;if loop !=0

			sbiw	ZH:ZL, 2		;Z<-Z-2
			adiw	ZH:ZL, 1		;Z<-Z+1
			dec		oloop			;decrement counter
			brne	MUL24_OLOOP		;if loop !=0

		pop		iloop		;store iloop register
		pop		oloop		;store ollop register
		pop		ZL			;store Z-ptr
		pop		ZH			
		pop		YL			;store Y-ptr
		pop		YH
		pop		XL			;store X-ptr
		pop		XH
		pop		zero		;store zero register
		pop		rlo			;store rlo register
		pop		rhi			;store rhi register
		pop		B			;store B register
		pop		A			;store	A register

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((D - E) + F)^2
;		by making use of SUB16, ADD16, and MUL24.
;
;		D, E, and F are declared in program memory, and must
;		be moved into data memory for use as input operands.
;
;		All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:

		; Setup SUB16 with operands D and E
		; Perform subtraction to calculate D - E
		ldi YL, low(SUB16_OP1)		;Load low byte address into Y 
		ldi	YH, high(SUB16_OP1)		;Load high byte address into Y
		;load address of D into Z
		ldi ZL, low(COMPOUND_OP1)	;load low byte address into Z
		ldi ZH, high(COMPOUND_OP1)	;loda low byte address into Z
		;copy D into first operand
		lpm mpr, Z+					;load byte from memory to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm mpr, Z+					;load byte from memory to mpr,post-increment
		st  Y+, mpr					;store post-increment into Y  
		; 
		ldi YL, low(SUB16_OP2)		;Load low byte address into Y 
		ldi	YH, high(SUB16_OP2)		;Load high byte address into Y
		;load address ofE into Z 
		ldi	ZL, low(COMPOUND_OP2)	;load low byte address into Z
		ldi ZH, high(COMPOUND_OP2)	;load lowe byte address into Z
		;copy E into  second operand
		lpm	mpr, Z+					;load byte from memort to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm	mpr, Z+					;load byte from memory to mpr,post-increment 
		st	Y+, mpr					;store byte into Y 
		
		rcall SUB16					;;call function
		
		; Setup the ADD16 function with SUB16 result and operand F
		; Perform addition next to calculate (D - E) + F
		ldi YL, low(ADD16_OP1)		;Load low byte address into Y 
		ldi	YH, high(ADD16_OP1)		;Load high byte address into Y
		;load address of SUB16_RESULT to Z
		ldi ZL, low(SUB16_RESULT)	;load low byte address into Z
		ldi ZH, high(SUB16_RESULT)	;loda low byte address into Z
		;copy SUB16_RESULT into first operand
		lpm mpr, Z+					;load byte from memory to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm mpr, Z+					;load byte from memory to mpr,post-increment
		st  Y+, mpr					;store post-increment into Y 
		  
		ldi YL, low(ADD16_OP2)		;Load low byte address into Y 
		ldi	YH, high(ADD16_OP2)		;Load high byte address into Y 
		;load address of F to Z
		ldi	ZL, low(COMPOUND_OP3)	;load low byte address into Z
		ldi ZH, high(COMPOUND_OP3)	;load lowe byte address into Z
		;copy F into second operand
		lpm	mpr, Z+					;load byte from memort to mpr, post-increment
		st	Y+, mpr					;store post-increment into Y
		lpm	mpr, Z+					;load byte from memory to mpr,post-increment  
		st	Y+, mpr					;store post-increment into Y  

		rcall ADD16					;call function
		; Setup the MUL24 function with ADD16 result as both operands
		; Perform multiplication to calculate ((D - E) + F)^2
		;load address of first operand to Y
		ldi YL, low(MUL24_OP1)		;Load low byte address into Y 
		ldi	YH, high(MUL24_OP1)		;Load high byte address into Y
		;load address of second operand to X
		ldi XL, low(MUL24_OP2)	;load low byte address into X
		ldi XH, high(MUL24_OP2)	;loda low byte address into X
		;load address of ADD16_RESULT to Z
		ldi ZL, low(ADD16_RESULT)	;load low byte address into Z
		ldi ZH, high(ADD16_RESULT)	;loda low byte address into Z
		ld mpr, Z+					;get byte 
		st	Y+, mpr					;store post-increment into Y
		st X+, mpr					;store post-increment into X
		ld mpr, Z					;get byte
		st  Y+, mpr					;store post-increment into Y  
		st X+, mpr					;store post-increment into X 
		ld mpr, Z+					;get byte
		st	Y+, mpr					;store post-increment into Y 
		st X+, mpr					;store post-increment into X

		rcall MUL24

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;			A - Operand A is gathered from address $0101:$0100
;			B - Operand B is gathered from address $0103:$0102
;			Res - Result is stored in address 
;					$0107:$0106:$0105:$0104
;		You will need to make sure that Res is cleared before
;		calling this function.
;-----------------------------------------------------------
MUL16:
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
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
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
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
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

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variable by pushing them to the stack

		; Execute the function here
		
		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

; ADD16 operands
OperandA:
	.DW 0xFCBA		;Addition operand A
OperandB:
	.DW	0xFFFF		;addition operand B
; SUB16 operands
OperandC:
	.DW	0xFCB9		;subtract operand C
OperandG:
	.DW 0xE420		;subtract operand G
; MUL24 operands
OperandH:
	.DD	0xFFFFFF		;multiplication operand H
OperandK:			
	.DD	0xFFFFFF		;multiplication operand K

; Compoud operands
OperandD:
	.DW	0xFCBA				; test value for operand D
OperandE:
	.DW	0x2019				; test value for operand E
OperandF:
	.DW	0x21BB				; test value for operand F

;***********************************************************
;*	Data Memory Allocation
;***********************************************************

.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.

.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16
.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result

.org	$0130
SUB16_OP1:
		.byte 2				; allocate two bytes for first operand of SUB16
SUB16_OP2:
		.byte 2				; allocate two bytes for second operand of SUB16
.org	$0140
SUB16_RESULT:
		.byte 2				;allocate two bytes for SUB16 result 

.org	$0150				;data memory allocation for operands
MUL24_OP1:
		.byte 3				;allocate three bytes for first operand of MUL24
MUL24_OP2:
		.byte 3				;allocate three bytes for second operand of MUL24
.org	$0160
MUL24_RESULT:
		.byte 6				;allocate six bytes for MUL24 result 

.org	$0170
COMPOUND_OP1:
		.byte 2				;allocate two bytes for first operand of COMPOUND
COMPOUND_OP2:
		.byte 2				;allocate two bytes for second operand of COMPOUND
COMPOUND_OP3:
		.byte 2				;allocate two bytes for third operand of COMPOUND


;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program


