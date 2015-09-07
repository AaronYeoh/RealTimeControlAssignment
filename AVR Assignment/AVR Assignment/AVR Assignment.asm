/*
 * Lab2.asm
 *
 *  Created: 4/08/2015 2:43:56 p.m.
 *   Author: xwan572
 */ 


.nolist						;Turn listfile generation Off
.include "m8def.inc"		;All defines(Ports, Interrupt Table, etc...) needed for the Atmega8
.list						;Turn listfile generation On

.equ	Mass  = 1200		;Mass 
.equ	Velocity = 28		;velocity
.equ	Distance =	1		;Distance

;Pulse width = (RMPLoad_lookup value) * FactorA / FactorB
.equ	FactorA = 1 		;Parameter related to Coolant Temperature
.equ	FactorB = 2			;Parameter related to Oxygen Level - int > 1 (actual equation uses *FactorB where FactorB<1)

.equ	Load = 3			;Load value for lookup table// RPMLoad

.dseg 						; Start data segment
.org 0x67 					; Set SRAM address to hex 67
FlagPD2: .byte 1 ; Reserve a byte at SRAM for FlagPD2
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
		

		ldi YH, high(RPMLoad_Lookup<<1)
		ldi YL, low(RPMLoad_Lookup<<1)

		ldi r16 , 0
		sts FlagPD2, r16

		ldi r16 , 1
		sts CounterSchedule, r16

		sei ; enable interrupts

		;********* Main infinite loop ********
forever:
		Start_Task UpTime

		End_Task UpTime
		rjmp forever 
;*****************End of program *****************


;***************** Start of Task1 *****************
Task_1:	Start_Task 	1 	;Turn output indicator pin On
		push r16
		;********* Write Task 1 here ********
		ldi r22, low(Mass); load low bit 
		ldi r23, high(Mass); low high bit
		ldi r21, Velocity; load velocity
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


;***************** Start of Task2 *****************
Task_2:	Start_Task 	2	;Turn output indicator pin On
		;********* Task 2 idle Code *********
		rcall System_Monitor
		;************************************
		End_Task	2	;Turn output indicator pin Off
		RET
;***************** End Task2 **********************


;***************** Start of Task3 *****************
Task_3:	Start_Task 	3	;Turn output indicator pin On
		
		 push r16


		 ; Read 10-bit ADC conversion result
		 in r17, ADCH
		 in r16, ADCL
		 
		 ;Store Z registers in Y registers
		 mov zl, yl
		 mov zh, yh

		 ;Store the ADC results in r18 so we can math it.
		 mov r18, r17; we need to work with the MSBs of the ADC input

		 ldi r20, 5; store 5 in r20, used below
		 mul r18, r20; Multiply r18 (the ADC result) by 5 

		 mov r18, r0


		 ldi r20, 1
		 ldi r22, Load;;Shift Z reg by the Load	
		 sub r22, r20 ;substract 1 from Load

		 add r18, r22
		 add zl, r18

		 lpm r18, z

		 /*
		 clr 23
		 mov r22, r18

		 clr r21
		 mov r20, FactorA

		 rcall mul16x16_32;  r19:r18:r17:r16 = r23:r22 * r21:r20 ////will erase r17, r16
		 mov r

		 */

		 ldi r20, FactorA
		 mul r18, r20

		 mov r21, r0
		 ldi r22, FactorB

		 rcall div8u





		 pop r16
		 End_Task	3	;Turn output indicator pin Off
		RET
;***************** End Task3 **********************



;***************** Start of External Interrupt *****************
IntV0:
		push r16
		;Done
		ldi r16 , 1
		sts FlagPD2, r16 ;Set the flag to 1. This is read in the ClockTick ISR
		pop r16
		reti			;Return from Interurpt
;***************** End External Interrupt **********************

;***************** Start of External Interrupt *****************
ClockTick:
		Start_Task 	ClockTick_Task	;Turn output indicator pin On
		sei		;Enable interrupts!!!

		;********* Write ClockTick Code here ********
		ldi	r16, 68		; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT0, r16			; TCNT0Value = 255 - MaxValue
		

		; **************** TASK 3 - ADC *************************
		lds r16,CounterSchedule ; Load CounterSchedule into register 16

		cpi r16, 3 
		brne SkipTask3
		; every 3rd tick, run task 3
		;if (Schedule counter == 3) run this and set schedulecounter = 1 else schedulecounter++
		rcall Task_3 ; Run the ADC
		clr r16

		SkipTask3:
		inc r16
		sts CounterSchedule, r16; Store r16 back into FlagSchedule

		; **************** TASK 1 - Force calculation ******************
		lds r16, FlagPD2
		cpi r16, 1
		brne SkipTask1
		; if PD2 was pressed, run task 1
		; branch if (pd2Flag == true) { run task 1, and set pd2flag = false} else {do nothing}
		rcall Task_1 ; Run the Force calculation
		clr r16
		sts FlagPD2, r16

		;***************** TASK 2 - System Monitor always runs ****************
		SkipTask1:
		rcall Task_2 ; Run system monitor. Lowest priority




		;************************************

		End_Task	ClockTick_Task	;Turn output indicator pin Off
		RETI						;Return from Interurpt
;***************** End External Interrupt **********************



