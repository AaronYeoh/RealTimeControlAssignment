/*
 * Lab2.asm
 *
 *  Created: 4/08/2015 2:43:56 p.m.
 *   Author: xwan572 & ayeo722
 */ 


.nolist						;Turn listfile generation Off
.include "m8def.inc"		;All defines(Ports, Interrupt Table, etc...) needed for the Atmega8
.list						;Turn listfile generation On

.dseg 						; Start data segment
.org 0x67 					; Set SRAM address to hex 67
CounterSchedule: .byte 1 ; Reserve a byte at SRAM for CounterSchedule
/*  ************ Instructions on using variables in program memory
.DSEG 
var1:  .BYTE 1 ; reserve 1 byte to var1 
table: .BYTE tab_size ; reserve tab_size bytes

.CSEG 
ldi r30,low(var1) ; Load Z register low 
ldi r31,high(var1) ; Load Z register high 
ld r1,Z ; Load VAR1 into register 1
*/
; Reserve a byte at SRAM


.cseg
.org $00000					;Setting Origin Address
		rjmp Main 			;Reset vector
.org INT0addr				;Setting Origin Address
		rjmp IntV0 			;INT vector
.org OVF0addr				;Setting Origin Address
		rjmp ClockTick 		;ClockTick vector

;.cseg
.org   0x0100               ;table address engine speed (RPM) and load
;Load/RPM('000)
RPMLoad_Lookup:       ;1/1	2/1	  3/1	4/1	   1/2  2/2  3/2    4/2	   1/3	2/3	  3/3	4/3	   1/4	  2/4	3/4	  4/4  
	        .db       0x01, 0x02, 0x03, 0x04, 0x02, 0x04, 0x06, 0x08, 0x03, 0x06, 0x09, 0x0C, 0x04, 0x08, 0x0C, 0x10

FactorA_Lookup:		;0  25  50  75
			.db		12, 11, 10, 9

FactorB_Lookup:		;1  2   3   4 //factor B
			.db		12, 11, 10, 9	

.org $00200					;Setting Origin Address
.include "MECHENG313A.inc"	;Functions needed for MECHENG313





;***************** Start of Main *****************
Main: 
		ldi r16,LOW(RAMEND)	    ;Initialise your stack pointer here
		out SPL,r16						
		ldi r16,HIGH(RAMEND)						
		out SPH,r16		
			

		;CBI DDRD, PD2		;I/O Setup
		sbi PORTD,PD2
		
		ldi r16,(1<<INT0); int mask 0 set +  (1<<INT1) 
		out GICR,r16
		ldi r16,(1<<ISC01)		; interrupt t0 on rising edge
		out MCUCR,r16		;Interrupt Setup
		

		;********* ADC ********
		; set MUX to channel 2, left adjust the result, AREF taken from AVCC
		ldi r16, (1<<MUX0) ; ADMUX channel 2, AREF from AVCC PORTC
		out ADMUX,r16
		; switch AD conversion on, start conversion, divider rate = 16
		ldi r16, (1<<ADEN)|(1<<ADSC)|(1<<ADPS2)|(1<<ADFR)
		out ADCSRA, r16


		cbi DDRC,PC1		

		;********* ClockTick 8-bit Timer/Counter 0 *******      
		ldi r16, (1<<CS01)      ; Start Counter 0      
      	out TCCR0, r16			; Timer Clock = Sys Clock (1MHz) / 8 (prescaler)
		ldi r16, (1<<TOIE0)     ; Enable interru       
		out TIMSK, r16			; Enable Timer Overflow interrupt

		ldi	r16, 68				; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT0, r16			; TCNT0Value = 255 - MaxValue	
		



		ldi r16 , 1
		sts CounterSchedule, r16

		;sei ; enable interrupts

		;********* Main infinite loop ********
forever:
		Start_Task UpTime
		rcall Task_3
		End_Task UpTime
		rjmp forever 
;*****************End of program *****************





;***************** Clock Tick Interrupt Service Routine *****************
ClockTick:
		Start_Task 	ClockTick_Task	;Turn output indicator pin On
		sei		;Enable interrupts!!!

		;********* Write ClockTick Code here ********
		ldi	r16, 68		; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT0, r16			; TCNT0Value = 255 - MaxValue
		

		; FuelInjectionTimingTask HARD
		; Every nth tick, run the timing subroutine


		; CarMonitorTask SOFT
		; Every tick, read ADCL and:
		; convert from fahrenheit to degrees C
		; convert from Fluid Ounces to Litres 

		End_Task	ClockTick_Task	;Turn output indicator pin Off
		RETI						;Return from Interurpt
;***************** End External Interrupt **********************

; ASYNC CODE
		; Collision DetectionTask HARD - DO NOT SEI 
		; Use ADCH and ADCL. if > 0011 (3): Turn on an LED.
		; Else turn off. Optimise: Only read ADCH. If any are 1. Turn on LED
		

		; CarDoorIndicatorTask SOFT - SEI ON
		;

		; LeftIndicatorTask FIRM

		; RightIndicatorTask FIRM

		; LeftToggleTask FIRM

		; RightToggleTask FIRM








;***************** Start of Task3 *****************
Task_3:	Start_Task 	3	;Turn output indicator pin On
		
		 push r16

		ldi ZH, high(RPMLoad_Lookup<<1)
		ldi ZL, low(RPMLoad_Lookup<<1)

		 ; Read 10-bit ADC conversion result ; optimisation possible
		 in r18, ADCL
		 in r19, ADCH

		 ;mov r18, r16 ; low
		 ;mov r19, r17 ; high
		 
		 ;RPM Level Lookup stored in r21
		 clr r20
		 sbrc r18, 7 ; Skip if bit 7 in ADCL is clear
		 sbr r20, $02;

		 sbrc r18, 6 ; Skip if bit 6 in ADCL is clear
		 sbr r20, $01;

		 ldi r21, 4; store 4 in r20, used below
		 mul r20, r21; Multiply r20 (the ADC result) by 4 

		 mov r21, r0 ; Move the result of the multiplication to r21

		 ;Load lookup stored in r20
		 clr r20
		 sbrc r18,  5; Skip if bit 5 in ADCL is clear
		 sbr r20, $02;

		 sbrc r18, 4 ; Skip if bit 4 in ADCL is clear
		 sbr r20, $01;

		 add r21, r20 ; Add the load to the previous RPM level

		 add ZL, r21

		 lpm r22, z




		 ldi ZH, high(FactorA_Lookup<<1)
		 ldi ZL, low(FactorA_Lookup<<1)


		 clr r20
		 sbrc r18,  3; Skip if bit 5 in ADCL is clear
		 sbr r20, $02;

		 sbrc r18, 2 ; Skip if bit 4 in ADCL is clear
		 sbr r20, $01;

		 add ZL, r20

		 lpm r23, z



		 pop r16
		 End_Task	3	;Turn output indicator pin Off
		RET
;***************** End Task3 **********************



;***************** Start of External Interrupt *****************
IntV0:
		push r16
		;Done
		ldi r16 , 1
;		sts FlagPD2, r16 ;Set the flag to 1. This is read in the ClockTick ISR
		pop r16
		reti			;Return from Interurpt
;***************** End External Interrupt **********************













;***************** Start of Task1 *****************
Task_1:	Start_Task 	1 	;Turn output indicator pin On
		push r16
		;********* Write Task 1 here ********
		;ldi r22, low(Mass); load low bit 
		;ldi r23, high(Mass); low high bit
		;ldi r21, Velocity; load velocity
		mul r21, r21; mul has to 
		mov r21, r1
		mov r20, r0
		rcall mul16x16_32; r19:r18:r17:r16 = r23:r22 * r21:r20
		mov r24, r18
		mov r23, r17
		mov r22, r16
		ldi r19, 2;
		ldi r21, 0
		ldi r20, 0
		clr r18
		clr r17
		clr r16
		rcall div24x24_24; r24:r23:r22 = r24:r23:r22 / r21:r20:r19
		;ldi r19, Distance
		;rcall div24x24_24;


		;************************************
		pop r16
		End_Task	1	;Turn output indicator pin Off
		RETI
;***************** End Task1 **********************
