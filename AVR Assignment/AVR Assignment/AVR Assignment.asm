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
PulseWidth: .byte 1
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
.org ADCCaddr
		rjmp ADCF0
.org OVF0addr				;Setting Origin Address
		rjmp ClockTick 		;ClockTick vector

;.cseg
.org   0x0100               ;table address engine speed (RPM) and load
;Load/RPM('000)
RPMLoad_Lookup:       ;1/1	2/1	  3/1	4/1	   1/2  2/2  3/2    4/2	   1/3	2/3	  3/3	4/3	   1/4	  2/4	3/4	  4/4  
	        .db       0x01, 0x02, 0x03, 0x04, 0x02, 0x04, 0x06, 0x08, 0x03, 0x06, 0x09, 0x0C, 0x04, 0x08, 0x0C, 0x10

FactorA_Lookup:		;0  25  50  75
			.db		12, 11, 10, 9

FactorB_Lookup:		;1 2  3  4 //factor B
			.db		4, 4, 4, 3	

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

		sbi DDRB, PB6
		sbi PORTB, PB2 ; Turns off Pin2 of PortB. Note the negative logic. For the collision detection
		
		ldi r16,(1<<INT0); int mask 0 set +  (1<<INT1) 
		out GICR,r16
		ldi r16,(1<<ISC01)		; interrupt t0 on rising edge
		out MCUCR,r16		;Interrupt Setup
		

		;********* ADC ********
		; set MUX to channel 2, left adjust the result, AREF taken from AVCC
		ldi r16, (1<<MUX0) ; ADMUX channel 2, AREF from AVCC PORTC
		out ADMUX,r16
		; switch AD conversion on, start conversion, divider rate = 16
		ldi r16, (1<<ADEN)|(1<<ADSC)|(1<<ADPS2)|(1<<ADFR)| (1<<ADIE)
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

		sei ; enable interrupts

		;********* Main infinite loop ********
forever:
		Start_Task UpTime
		rcall MonitorTask
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




MonitorTask:
		in r22, ADCL
		
		;Treat r18 (ADCL) as Fahrenheit and convert to celcius

		ldi r17, 32

		sub r22, r17

		ldi r19, 9

		clr r24
		clr r23
		clr r21
		clr r20
		;We divide by 9
		rcall div24x24_24 ;r24:r23:r22 = r24:r23:r22 / r21:r20:r19

		ldi r19, 5
		mul r22, r19

		mov r22, r0

		push r22

				

		//Treat r22? (ADCL) as Fluid Ounces water level input and convert the value to Litres
		//L =us fl oz / 33.814
		//L =uk fl oz / 35.195

		in r22, ADCL

		ldi r20, low(1000)
		ldi r21, high(1000)
		clr r23

		rcall mul16x16_32 ;r19:r18:r17:r16 = r23:r22 * r21:r20

		
		ldi r19, low(33814)
		ldi r20, high(33814)
		clr r21

		mov r24, r18
		mov r23, r17
		mov r22, r16

		rcall div24x24_24 ;r24:r23:r22 = r24:r23:r22 / r21:r20:r19

		mov r23, r22
		
		pop r22

		RET


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



		 //Factor A lookup
		 ldi ZH, high(FactorA_Lookup<<1)
		 ldi ZL, low(FactorA_Lookup<<1)


		 clr r20
		 sbrc r18,  3; Skip if bit 3 in ADCL is clear
		 sbr r20, $02;

		 sbrc r18, 2 ; Skip if bit 2 in ADCL is clear
		 sbr r20, $01;

		 add ZL, r20

		 lpm r23, z


		 //Factor B lookup
		 ldi ZH, high(FactorB_Lookup<<1)
		 ldi ZL, low(FactorB_Lookup<<1)


		 clr r20
		 sbrc r18,  1; Skip if bit 1 in ADCL is clear
		 sbr r20, $02;

		 sbrc r18, 0; Skip if bit 0 in ADCL is clear
		 sbr r20, $01;

		 add ZL, r20

		 lpm r24, z


		 //Calculate Pulse Width

		 
		 mul r23, r22

		 mov r23,r0

		 mul r24, r23 ; x FactorB

		 mov r23, r1
		 mov r22, r0

		 ldi r19, 40; divisor

		 clr r24;
		 clr r21;
		 clr r20;
		 ;Finally we divide by 40
		 rcall div24x24_24 ;r24:r23:r22 = r24:r23:r22 / r21:r20:r19
		 
		 sts PulseWidth, r22


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






;***************** Collision Detection*****************
ADCF0:	Start_Task 	2 	;Turn output indicator pin On
		
		;********* Write Task  here ********
		in r22, ADCL
		in r23, ADCH
		clr r20

		;obtain 2 LSB of ADCH, store in r20
		sbrc r23,  1; Skip if bit 1 in ADCH is clear
		sbr r20, $08;

		sbrc r23, 0; Skip if bit 0 in ADCH is clear
		sbr r20, $04;


		;obtain 2 MSB of ADCL, store in r20
		sbrc r22,  7; Skip if bit 7 in ADCL is clear
		sbr r20, $02;

		sbrc r22, 6; Skip if bit 6 in ADCL is clear
		sbr r20, $01;

		
		cpi r20, 4
		brsh collision
		sbi PORTB, PB6

		
		RETI


		collision:
		cbi PORTB, PB6 ;Collision has occurred. Turn on LED at PB6

		
		RETI
		
		;end of collision
		

		;************************************
		
		End_Task	2	;Turn output indicator pin Off
		RETI
;***************** End Task1 **********************






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
