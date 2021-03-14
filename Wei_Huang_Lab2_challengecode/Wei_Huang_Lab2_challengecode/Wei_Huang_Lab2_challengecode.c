/*
 * Wei_Huang_Lab2_challengecode.c
 *
 * Created: 10/13/2020 2:50:06 PM
 * Author : Huang Wei
 */ 


/*
This code will cause a TekBot connected to a mega128 board to 'dance' in a cool
pattern. No pins are used as input, and four Port B pins are used for output.

PORT MAP
Port B, Pin 4 -> Output -> Right Motor Enable
Port B, Pin 5 -> Output -> Right Motor Direction
Port B, Pin 7 -> Output -> Left Motor Enable
Port B, Pin 6 -> Output -> Left Motor Direction
Port D, pin 0 -> Input -> Righter Whisker
Port D, Pin 1 -> INput -> Letf Whisker	A
*/
#define F_CPU 16000000
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

int main(void)
{
	DDRB  = 0b11110000;      // configure Port B pins for input/output
	PORTB = 0b11110000;     // set initial value for Port B outputs
	// (initially, disable both motors)
	DDRD  = 0b00000000;      // configure Port D pins for input
	PORTD = 0b00000011;		// set initial value for Port D inputs
	while (1) { // loop forever
		PORTB = 0b01100000;     // make TekBot move forward
		if(PIND == 0b11111101){	//Left whisker  hit
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b00000000;     // move backward
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b00100000;     // turn left
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b01000000;     // turn right
			
		}
		else if(PIND == 0b11111110){	//right whisker
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b00000000;     // move backward
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b01000000;     // turn Right
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b00100000;     // turn Left
		}
		else if(PIND == 0B11111100){ //Both whiskers hit
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b00000000;     // move backward
			_delay_ms(1000);        // wait for 1 s
			PORTB = 0b01100000;     // turn both
		}

	}
}


