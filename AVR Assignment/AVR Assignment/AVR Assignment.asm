/*
 * Lab2.asm
 *	DOOR LED - PB2
 *	DOOR TOGGLE SWITCH - PD2 (Switch 1 on STK500)
 *  
 *	COLLISION LED - PB4
 *
 *  Created: 4/08/2015 2:43:56 p.m.
 *   Authors: xwan572 & ayeo722
 */ 

 ;************push and pop potential register used****************
.MACRO PopAll
	pop r16
	out SREG, r16 
	pop r0
	pop r1
	pop r16
	pop r17
	pop r18
	pop r19
	pop r20
	pop r21
	pop r22
	pop r23
	pop r24

.ENDMACRO



.MACRO PushAll
	push r24
	push r23
	push r22
	push r21
	push r20
	push r19
	push r18
	push r17
	push r16
	push r1
	push r0
	in r16, SREG
	push r16
.ENDMACRO
;************end push and pop****************


.nolist						;Turn listfile generation Off
.include "m8def.inc"		;All defines(Ports, Interrupt Table, etc...) needed for the Atmega8
.list						;Turn listfile generation On

.dseg 						; Start data segment
.org 0x67 					; Set SRAM address to hex 67
;CounterSchedule: .byte 1 ; Reserve a byte at SRAM for CounterSchedule
PulseWidth: .byte 1 

LeftBroken: .byte 1
TurnOnLeftNext: .byte 1
TurnOffLeftNext: .byte 1
RightBroken: .byte 1

TurnOnRightNext: .byte 1
TurnOffRightNext: .byte 1

FuelInjRunning: .byte 1 ; 0x6E
LeftRightInterrupted: .byte 1
CarDoorInterrupted: .byte 1
IndicatorRunning: .byte 1

PulseCounterSchedule: .byte 1 ; reserve 1 byte for counter of Task_3

LeftToggled: .byte 1
RightToggled: .byte 1
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
		rjmp IntV0 			;INT vector - for toggling the door state
;.org INT1addr				;Setting Origin Address
;		rjmp IntV1			;INT vector - for toggling the light state
.org ADCCaddr
		rjmp ADCF0
;.org OVF0addr				;Setting Origin Address
;		rjmp ClockTick 		;ClockTick vector
;.org OVF1addr				;Setting Origin Address
;		rjmp ClockTickLeftRight 		;ClockTick vector

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
			

		;SBI DDRD, PD4		;I/O Setup Left broken toggle
		;SBI DDRD, PD5		;I/O Setup Right Broken toggle
		;sbi PORTD,PD2		; Test code. Turns on an LED

		;Set the DDR for PORTB, allowing for us to write out

		ldi r16, (1<<PB0) | (1 << PB2) | (1 << PB4) | (1 << PB1)
		out DDRB, r16
		out PORTB, r16
		;sbi DDRB, PB0; left LED
		;sbi DDRB, PB2  ;door LED
		;sbi DDRB, PB4; collision LED
		;sbi DDRB, PB1; right LED

		;Set everything high in PORTD, set DDRD to be input only
		ldi r16, $FF;
		out PORTD, r16
		clr r16
		out DDRD, r16
		
		;sbi PORTB, PB2 ; Turns off Pin2 of PortB. Note the negative logic. For the collision detection
		;sbi PORTB, PB4	; Turns off Pin4 of PortB. For the door indicator. Door initialised as shut.
		;sbi PORTB, PB1	; Turns off Pin7 of PortB. LeftLED init as off
		;sbi PORTB, PB0	; Turns off Pin0 of PortB. RightLED init as off
		
		ldi r16,(1<<INT0) | (1<<INT1); int mask 0 set +  (1<<INT1) 
		out GICR,r16
		ldi r16,(1<<ISC01) | (1<<ISC11)		; interrupt t0 on falling edge
		out MCUCR,r16		;Interrupt Setup
		

		;********* ADC ********
		; set MUX to channel 2, left adjust the result, AREF taken from AVCC
		ldi r16, (1<<MUX0) ; ADMUX channel 2, AREF from AVCC PORTC
		out ADMUX,r16
		; switch AD conversion on, start conversion, divider rate = 16
		ldi r16, (1<<ADEN)|(1<<ADSC)|(1<<ADPS2)|(1<<ADFR)| (1<<ADIE)
		out ADCSRA, r16


		;cbi DDRC,PC1		; DELETE ?

		;********* ClockTick 8-bit Timer/Counter 0 *******      
		ldi r16, (1<<CS01)      ; Start Counter 0      
      	out TCCR0, r16			; Timer Clock = Sys Clock (1MHz) / 8 (prescaler)
		
		ldi	r16, 68				; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT0, r16			; TCNT0Value = 255 - MaxValue	
		

		;********* ClockTick 16-bit Timer/Counter 1 *******      
		ldi r16, (1<<CS11)      ; Start Counter 0      
      	out TCCR1B, r16			; Timer Clock = Sys Clock (1MHz) / 8 (prescaler)
		
		;to get 0.25ms per interrupt, TCNT1 = 34286 = $85EE
		ldi	r16, $EE			; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT1L, r16			; TCNT0Value = 2^16 - MaxValue	

		ldi	r16, $85			; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT1H, r16			; TCNT0Value = 255 - MaxValue	




		;********* Clock Interrupts
		ldi r16, (1<<TOIE0) | (1<<TOIE1)     ; Enable interrupts for Counter 0 and 1       
		out TIMSK, r16			; Enable Timer Overflow interrupt




		ldi r16 , 1
		sts PulseCounterSchedule, r16
		sts PulseWidth, r16


		ldi r16, 0
		sts CarDoorInterrupted, r16
		sts FuelInjRunning, r16
		sts LeftToggled, r16
		sts RightToggled, r16

		sei ; enable interrupts

		;********* Main infinite loop ********
forever:
		;Start_Task 1
		;rcall MonitorTask
		rcall Task_3
		;End_Task 1
		rjmp forever 
;*****************End of program *****************











;***************** Clock Tick Interrupt Service Routine *****************
ClockTick:
		;Start_Task 	ClockTick_Task	;Turn output indicator pin On
		PushAll
		sei		;Enable interrupts!!!

		;********* Write ClockTick Code here ********
		
		
		;rcall IntV1
		; FuelInjectionTimingTask HARD
		; Every nth tick, run the timing subroutine


		
		
		; Every tick, read ADCL and:
		; convert from fahrenheit to degrees C
		; convert from Fluid Ounces to Litres 


		lds r16, PulseCounterSchedule
		lds r17, PulseWidth
		
		cp r16, r17
		brne SkipTask
		rcall Task_3
		
		clr r16
		clr r17
		


		SkipTask:
		inc r16
		sts PulseCounterSchedule, r16


		rcall MonitorTask

		;End_Task	ClockTick_Task	;Turn output indicator pin Off
		PopAll
		RETI						;Return from Interurpt


;***************** End External Interrupt **********************

;***************** Clock Tick Interrupt Service Routine *****************
ClockTickLeftRight:
		PushAll
		;If the fuel injection code was running:
		lds r16, FuelInjRunning
		ldi r17, 1
		cpse r16, r17 ;Compare skip if equal
			rjmp RunLeftRight
		ldi r16, 1
		sts LeftRightInterrupted, r16

		pop r16
		reti

		;Allow collision to interrupt, disallow door code to interrupt
		

		RunLeftRight:
		
		;Not interrupted
		sei		;Enable interrupts!!!
		rcall IntV1 ; Check if Mike wants to toggle the stage of the switch
		ldi r16, 0
		sts LeftRightInterrupted, r16
		
		ldi r16, 1
		sts IndicatorRunning, r16
		
		
		
		;****************** Code for this interrupt below **********

		
				;to get 0.25ms per interrupt, TCNT1 = 34286 = $85EE
		ldi	r16, $EE			; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT1L, r16			; TCNT0Value = 2^16 - MaxValue	

		ldi	r16, $85			; MaxValue = TOVck (1.5ms or your Cal time) * Pck (1MHz) / 8 (prescaler)
		out TCNT1H, r16			; TCNT0Value = 255 - MaxValue	


		;********* Write ClockTick Code here ********
		;LEFT

		
		;When LeftLED is OFF 
		sbis PORTB, PB1 ;Skip if Left is off (PB1 == 1)
		rjmp LeftLEDON ; Left is actually ON

			;If Left is pressed
			sbic PIND, PD1 ;Check if LEFT button pressed (PD1 = 0), otherwise we RJMP to the right LED code
			rjmp RightLED ; Skipped if PD1 = 0

				;If either Broken or TurnOnLeftNext
				lds r16, LeftBroken
				lds r17, TurnOnLeftNext
				or r16, r17 ; if LeftBroken OR TurnOnLeftNext, turn on LEFT. R16 = 1
				sbrs r16, 0  ;If we want to turn on the 
				rjmp TurnOnLeftLater 
					;Turn Left ON
					cbi PORTB, PB1 ; Turn the LED ON by setting PB1 = 0
					ldi r16, 0
					sts TurnOnLeftNext, r16

					rjmp RightLED

				;Not Broken or TOLN
				TurnOnLeftLater:
					;TurnOnLeftNext = true
					ldi r16, 1
					sts TurnOnLeftNext, r16
					;Do nothing
				
			rjmp RightLED

		LeftLEDON:
		;If LeftLED is ON

			;If either Broken or TurnOffLeftNext
			lds r16, LeftBroken
			lds r17, TurnOffLeftNext
			or r16, r17 ; if LeftBroken OR TurnOnLeftNext, turn on LEFT. R16 = 1
			sbrs r16, 0  ;If we want to turn on the 
			rjmp TurnOffLeftLater 
				;Turn Left OFF
				sbi PORTB, PB1 ; Turn the LED OFF by setting PB1 = 1
				ldi r16, 0
				sts TurnOffLeftNext, r16	;TurnOffLeftNext = false

				rjmp RightLED
			
			;Not Broken or TOLN
			TurnOffLeftLater:
				;TurnOffLeftNext = true
				ldi r16, 1
				sts TurnOffLeftNext, r16
				;Do nothing
				
			
		
		
		
		RightLED:	

		
		;When RightLED is OFF 
		sbis PORTB, PB0 ;Skip if Right is off (PB0 == 1)
		rjmp RightLEDON ; Left is actually ON

			;If Right is pressed
			sbic PIND, PD0 ;Check if Right button pressed (PD0 = 0), otherwise return
			rjmp CarDoorCallback; Skipped if PD0 = 0

				;If either Broken or TurnOnRightNext
				lds r16, RightBroken
				lds r17, TurnOnRightNext
				or r16, r17 ; if RightBroken OR TurnOnRightNext, turn on RIGHT. R16 = 1
				sbrs r16, 0  ;If we want to turn on the 
				rjmp TurnOnRightLater 
					;Turn RIGHT ON
					cbi PORTB, PB0 ; Turn the LED ON by setting PB0 = 0
					ldi r16, 0
					sts TurnOnRightNext, r16

					rjmp CarDoorCallback 

				;Not Broken or TOLN
				TurnOnRightLater:
					;TurnOnRightNext = true
					ldi r16, 1
					sts TurnOnRightNext, r16
					;Do nothing
					
					rjmp CarDoorCallback
			

		RightLEDON:
		;If RightLED is ON

			;If either Broken or TurnOffRightNext
			lds r16, RightBroken
			lds r17, TurnOffRightNext
			or r16, r17 ; if RightBroken OR TurnOnRightNext, turn on Right. R16 = 1
			sbrs r16, 0  ;If we want to turn on the 
			rjmp TurnOffRightLater 
				;Turn Right OFF
				sbi PORTB, PB0 ; Turn the LED OFF by setting PB0 = 1
				ldi r16, 0
				sts TurnOffRightNext, r16	;TurnOffRightNext = false

				rjmp CarDoorCallback
			
			;Not Broken or TOLN
			TurnOffRightLater:
				;TurnOffRightNext = true
				ldi r16, 1
				sts TurnOffRightNext, r16
				;Do nothing


		CarDoorCallback:
		 ; Car door callback
		 lds r16, CarDoorInterrupted
		 sbrc r16, 0
		 rcall IntV0
		 
		 lds r16, 0
		 sts CarDoorInterrupted, r16 ; clear the flag
		 PopAll
		 reti


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
		PushAll
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

		PopAll
		RET


;***************** Start of Task3 *****************
Task_3:	;Start_Task 	3	;Turn output indicator pin On
		
		PushAll
		
		;Allow ONLY the Collision code to interrupt this
		ldi r16, 1
		sts FuelInjRunning, r16

		sei 

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
		 clr r16
		 clr r0
		 rcall div24x24_24 ;r24:r23:r22 = r24:r23:r22 / r21:r20:r19
		 	
		 ldi r19,2
				 
		 rcall div24x24_24 ;r24:r23:r22 = r24:r23:r22 / r21:r20:r19
		 
		 
		 sts PulseWidth, r22
		 

		 
		 ;error check if PulseWidth is 0, if true then branch to set it to 1, otherwise do nothing
		 cpi r22, 0
		 breq SetToOne
		 rjmp NoSetToOne

		 SetToOne:
		 ldi r22, 1
		 sts PulseWidth, r22
		 
		 NoSetToOne:
		 ;Clear the FuelInjRunning flag
		 ldi r16, 0
		 sts FuelInjRunning, r16 

		 ;********* Interrupt callback ***************
		 ;If the left right indicator was interrupted, run it
		 lds r16, LeftRightInterrupted
		 sbrc r16, 0
		 rcall ClockTickLeftRight
		 
		 lds r16, 0
		 sts LeftRightInterrupted, r16 ; clear the flag


		 ; Car door callback
		 lds r16, CarDoorInterrupted
		 sbrc r16, 0
		 rcall RunCarDoor
		 
		 lds r16, 0
		 sts CarDoorInterrupted, r16 ; clear the flag
		 



		 ;********* End Interrupt callback ***************

		 PopAll



		; End_Task	3	;Turn output indicator pin Off
		RET
;***************** End Task3 **********************



;***************** Start of External Interrupt *****************
; Car door status switcher ISR - Soft Real Time   ;Done!
; DOOR OPEN LIGHT LED PB4

IntV0:
		PushAll
		;If the fuel injection code was running:
		lds r16, FuelInjRunning
		lds r17, IndicatorRunning
		or r16, r17
		ldi r17, 1
		cpse r16, r17 ;Compare skip if equal
			rjmp RunCarDoor
		ldi r16, 1
		sts CarDoorInterrupted, r16

		PopAll
		reti

		;Allow collision to interrupt, disallow door code to interrupt
		

		RunCarDoor:
		PushAll
		;Not interrupted
		ldi r16, 0
		sts CarDoorInterrupted, r16
		sei		;Enable interrupts!!!
		; ***** Code below


		;Check the PB2 bit. If it is set, the door WAS shut (LED off) and it's now open. We want to turn ON the LED. 
		sbic PORTB, PB2
		rjmp door_shut

		;if door was open (PB4 == 0), it is shut now
		
		sbi PORTB, PB2  ;SET the door LED - LED is OFF
		PopAll
		RETI			;Return from Interurpt

		door_shut:
		;if door was shut, we set it as open
		cbi PORTB, PB2	;Clear the door LED - LED is ON 

		PopAll
		RETI			;Return from Interurpt
;***************** End External Interrupt **********************




;***************** Collision Detection*****************
ADCF0:	;Start_Task 	2 	;Turn output indicator pin On
		PushAll
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
		sbi PORTB, PB4 ;Collision has NOT occurred. Turn off LED at PB4 by setting the bit
			
		PopAll		
		RETI


		collision:
		cbi PORTB, PB4 ;Collision has occurred. Turn on LED at PB4 by clearing the bit
			
		PopAll	
		RETI
		
		;end of collision
		

		;************************************
		
		;End_Task	2	;Turn output indicator pin Off
;***************** End Task1 **********************


;To use, connect P
IntV1:
		PushAll

		
		
		

		lds r16, LeftToggled; check if left toggle was pressed already
		cpi r16, 1
		brne AllowToggleLeft
		; left Toggle not allowed
		sbis PIND, PD5 ; Left Broken toggle
			rjmp CheckRightToggle
		ldi r16, 0
		sts LeftToggled, r16
		rjmp CheckRightToggle

		AllowToggleLeft:
		sbic PIND, PD5 ; Left Broken toggle
			rjmp CheckRightToggle

		rcall LeftStatusToggle	;change the state of the left switch
		ldi r16, 1
		sts LeftToggled, r16
		

		CheckRightToggle:
		
		lds r16, RightToggled ; check if right toggle was pressed already
		cpi r16, 1
		brne AllowToggleRight
		;Right Toggle not allowed
		sbis PIND, PD4
			rjmp ReturnFromIntV1

		ldi r16, 0
		sts RightToggled, r16
		PopAll
		reti

		AllowToggleRight:
		sbic PIND, PD4; Right Broken toggle
			rjmp ReturnFromIntV1

		rcall RightStatusToggle ;change the state of the right switch
		ldi r16, 1
		sts RightToggled, r16
		
		

		ReturnFromIntV1:
			PopAll
			reti

;************ Toggle Normal / Broken state ************* 

;Left toggle
LeftStatusToggle:

		lds r16, LeftBroken

		cpi r16, 1
		breq SetLeftTo0
		
		ldi r16, 1
		sts LeftBroken, r16
		ret
		
		SetLeftTo0:

		ldi r16, 0
		sts LeftBroken, r16
		ret

;Right toggle
RightStatusToggle:

		lds r16, RightBroken

		cpi r16, 1
		breq SetRightTo0
		
		ldi r16, 1
		sts RightBroken, r16
		ret
		
		SetRightTo0:

		ldi r16, 0
		sts RightBroken, r16
		ret
;**************** end ******************


