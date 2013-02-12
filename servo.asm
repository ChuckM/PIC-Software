; vim: set syntax=pic :
; See the article on http://robotics.mcmanis.com/
;
	TITLE	"Electronic Speed Control SERVO SOFTWARE"
	LIST	P=16C54A, R=HEX, C=120
	include "p16C5X.inc"
	include "p16c5xd.inc"
	__FUSES _CP_OFF&_HS_OSC&_WDT_OFF
;
; ESC Servo Control Software
; Written 12/29/94 by Chuck McManis
;
;	Copyright (c) 1994-1995, Chuck McManis all rights reserved.
;
; Modification history:
;	* 12/19/94 - Initial release
;	* 1/12/95 - Modified to use PORT A for output.
;	* 1/18/95 - Timing constants assume a 20Mhz clock.
;
; Data space
;
    CBLOCK H'0008'
	COUNT_HI	; High Byte of the Count
	COUNT_LO	; Low Byte of the Count
	TEMP		; Temporary variable
	PWM_CMD		; PWM Constant
	PWM_TMP		; Temporary counter for PWM count down
	CMD_REG		; Motor control command
	CMD_TMP		; Temporary copy of CMD_REGS
	OUTER_CNT	; Outer loop count
	INNER_CNT	; Inner Loop count
	GLITCH		; Our glitch count
    ENDC
;
; Bridge Control definitions. The output bits are connected as follows:
;	Bit 3 -	Left High Side Switch
;	Bit 2 - Right High Side Switch
;	Bit 1 - Left Low Side Switch
;	Bit 0 - Right Low Side Switch

FORWARD_CMD	EQU	H'89'	; Move forward  (Left high + Right lower)
REVERSE_CMD	EQU	H'46'	; Move Backward (Right High + Left Lower)
COAST_CMD	EQU	H'00'	; Coast (all off)
BRAKE_CMD	EQU	H'33'	; Brakes on (Left Lower + Right Lower).
GLITCH_COUNT	EQU	D'172'	; 860uS (172 * 5 uS, 20 Mhz clock)
DEADBAND	EQU	D'6'	; Deadband range + 1

; The input pin
#define SERVO_IN	PORTB,0	

; The H-bridge ouput
#define BRIDGE_OUT	PORTA

;
; System initialization code
;
	ORG	H'1FF'	; Reset Vector (PIC16C54)
	GOTO	INIT
	ORG	0
INIT
	MOVLW	0		; All bits as outputs.
	TRIS	BRIDGE_OUT	; Output port is all outputs.
	CLRF	BRIDGE_OUT	; And all outputs OFF.
	MOVLW	GLITCH_COUNT	; Initialize vars
	MOVWF	GLITCH		; 
	MOVWF	COUNT_LO	; Initialize count to 0x1A0
	MOVLW	1		;
	MOVWF	COUNT_HI	;
;
; IDLE combines both the Idle and the Measure states. This is accomplished
; by the routine DO_MEASURE which can be called when measuring and when idle.
;
IDLE:
	CALL	DO_MEASURE	; Measure input pulse		       16
	MOVWF	TEMP		; Store result in TMP			1
	MOVF	TEMP,F		; Waste time, set Z bit			1
	BTFSS	ZERO_BIT	; If zero no new value.			2 1
	GOTO	PWM		; If got a value, then start.		- 2
	GOTO	$+1		; Waste time				2
	NOP			; ...					1
	GOTO	IDLE		; Go back to measuring			2
				;		Total Clocks in Loop   25
;
; PWM is the loop that generates the PWM output. We do this
; by turning on the H-Bridge and counting down PWM_CMD steps and
; then turning off the bridge. The loop count is effectively 100.
; This gives a direct percentage relationship between the value
; in PWM_CMD and the output. The value 1 is a 1% duty cycle square
; wave and the value 99 is a 99% duty cycle. (100 is 100%) So
; The range of legal inputs is 1 thru 100.
;
; The loop is structured to take 500 uS, the inner loop takes
; 5uS and the outer loop is designed to take 5uS as well.
; Thus 99 inner loop iterations and one outer loop pass == 100 * 5us
; which is 500uS. That yields a PWM frequency of 2000 Hz. 
; The outer loop has 40 iterations so that after 20mS, if no additional
; pulse has been received the loop exits and the H-bridge turns off.
;
; When we exit due to a timeout we must make sure that we keep calling
; DO_MEASURE on schedule because we may be in the middle of measuring a
; new input pulse. So we dovetail the outer loops return to idle with
; IDLE making the transistion from the PWM+Measuring state to the
; Idle+Measuring state seamless.
;
PWM:
	MOVLW	D'40'		; Constant for 20 mS
	MOVWF	OUTER_CNT	; Outer Count
	MOVF	PWM_CMD,W	; Set up the inner loop counters
	MOVWF	PWM_TMP
	MOVF	CMD_REG,W	; Get H-Bridge Command		
	MOVWF	CMD_TMP		; Put it in our temporary holder
	MOVWF	BRIDGE_OUT	; Turn on the motors			1
	MOVLW	D'99'		; 					1
	MOVWF	INNER_CNT	; 					1
	GOTO	PWM_LOOP	; Line up command			2 (4)
PWM_LOOP:
	CALL	DO_MEASURE	; Do a "measurement"			16
;
; If DO_MEASURE found a pulse, it will modify the control variables ;
;	PWM_TMP   - Set to FF to prevent swaping CMD_REG
;	INNER_CNT - Set to 1 so that this loop will exit immediately
;	OUTER_CND - Set to 41 so that the outer loop will run for 20mS
;	PWM_CMD   - Set to the new PWM constant
;	CMD_REG   - Set to the new H-Bridge Command
;

	DECF	PWM_TMP,F	; Decrement the "On" time		1\
	BTFSC	ZERO_BIT	; If not zero continue			2 1
	SWAPF	CMD_TMP,F	; Turn off the low side			- 1
	MOVF	CMD_TMP,W	; Get the Command Register		1/ 
	MOVWF	BRIDGE_OUT	; Send it To the H-Bridge.		1 
	MOVF	PWM_CMD,W	; Sort of a NOP				1
	DECFSZ	INNER_CNT,F	; Decrement inner loop count		1 2
	GOTO	PWM_LOOP	; Continue Looping			2 -
				;  Total Clocks	(inner loop)	25 (5 uS)
				;
;
; Outer loop count down, Note we've combined the MOVF PWM_CMD,W statement
; above with the MOVWF PWM_TMP statement below to accomplish reinitializing
; PWM_TMP in only 1 cycle. This works because the DECFSZ above doesn't
; change W. Without this trick there is no way for the outer loop to work
; in the required 5uS and we lose the ability to call do_measure on schedule.
;
	MOVWF	PWM_TMP		; Restore PWM command (loaded above)	1
	CALL	DO_MEASURE	; This is on a 5uS boundary.		16
	MOVLW	D'99'		; Reset inner loop count		1
	MOVWF	INNER_CNT	; Like so				1
	MOVF	CMD_REG,W	; Put command into W			1
	MOVWF	CMD_TMP		; Re-initialized CMD_TMP		1
	MOVWF	BRIDGE_OUT	; Send it to the motors			1
	DECF	OUTER_CNT	;					1
	BTFSS	ZERO_BIT	;					1 2
	GOTO	PWM_LOOP	;					2 -
;
; Getting here means we've timed out our 20 mS of looping. However we
; may be in the process of receiving a pulse so we make sure we call
; DO_MEASURE on schedule and essentially duplicate what the IDLE loop
; does and when we're done we loop to IDLE.
;
	CLRF	BRIDGE_OUT	; Turn off the motors			1
	CALL	DO_MEASURE	;					16
	MOVWF	TEMP		; Store result in TMP			1
	MOVF	TEMP,F		; Waste time, set Z bit			1
	BTFSS	ZERO_BIT	; If zero no new value.			2 1
	GOTO	PWM_LOOP	; If got a new value, then start.	- 2
	GOTO	$+1		; else Waste time			2
	NOP			; ...					1
	GOTO	IDLE		; Go back to measuring			2
;
; DO_MEASURE is the pulse measuring routine. It is called from either
; Idle or PWM. It implements a mini-state machine the monitors the
; input pin. The code is written so that it is called every 5uS.
; When called, the input will either be high or low. Depending on the
; input pins state, it either measures or doesn't.
; 	If the input pin is high, we start counting down our counter.
;	Each count represents 5uS. If we ever underflow our counter we
;	note that as a timeout (the input pulse was too wide.)
;
;	If the input pin is low, we process a potential pulse. If glitch
; 	is non-zero (input pulse < 860uS) we treat it as a glitch, reset
;	the counters and return. (Note this happens every time when the
;	input pin is low.)
;
;	If Glitch is zero we check for a counter underflow (timeout). If
;	the counter has timed out we reset the counters and return. (Note
;	this happens every time when the input pin stays high)
;
;	If we pass both of these tests (not a glitch, not a timeout) we
;	process it as a valid pulse, set all of the variables appropriately
; 	and return.
;
; Given the Design, this routine _MUST_ always execute in 14 clocks. The
; The clock counts in the right margin represent the time spent in
; various "paths" through this code.
;
DO_MEASURE:
	BTFSC	SERVO_IN	; Check for 'high' 		2 1
	GOTO	INP_HIGH	; Yup, count it.		- 2   (3)
	MOVF	GLITCH,F	; Check Glitch counter		1 -
	BTFSS	ZERO_BIT	; != 0 means Glitch.		2 - 1
	GOTO	NO_PULSE	; This was a glitch		- - 2 (6)
	BTFSS	COUNT_HI,7	; Check for overflow		2 - - 1
	GOTO	GOT_PULSE	; Process a good count		- - - 2	(8)
	MOVLW	GLITCH_COUNT	; Glitch length			1 - - *
	MOVWF	GLITCH		; Store it in GLITCH		1 - -
	MOVWF	COUNT_LO	; Just a glitch,		1 - - 
	MOVLW	1		; Load count with constant	1 - -
	MOVWF	COUNT_HI	; 				1 - - 
	RETLW	0		; And return			2 - -
						;              --  
						;  Total       14
						;  
;
; This is basically an abort, it resets the counters and state variables
; and returns.
;
NO_PULSE:			;				   (6)
	MOVLW	GLITCH_COUNT	; 				- - 1
	MOVWF	GLITCH		;				- - 1
	MOVWF	COUNT_LO	; Just a glitch,		- - 1 
	MOVLW	1		; Load count with 0x1A0		- - 1
	MOVWF	COUNT_HI	; 				- - 1
	NOP			; To get an even 14 clocks	- - 1 
	RETLW	0		; And return			- - 2
						;                  --
						;  Total           14
						;
;
; Process a "true" input.
;
INP_HIGH:			;				 (3)
	MOVF	GLITCH,F	; Check Glitch			- 1
	BTFSS	ZERO_BIT	; Is it already 0?		- 2 1
	DECF	GLITCH		; No, then decrement it		- - 1
	BTFSC	COUNT_HI,7	; Not a timeout already		- 2   1
	GOTO	INP_TIMEOUT	; Its a timeout			- -   2 (9)
	MOVF	COUNT_LO,F	; Test COUNT_LO for zero	- 1
	BTFSC	ZERO_BIT	; If its zero, this is it	- 2 1
	DECF	COUNT_HI,F	; Subtract one from COUNT_HI	- - 1
	DECF	COUNT_LO,F	; Subtract one from the count	- 1   -
	RETLW	0		;				- 2 
						;                -- 
						;  Total         14
						;
;
; During a timeout condition we basically hold the state variables
; where they are so that when the pin goes low it will be detected and
; the variables reset.
;
INP_TIMEOUT:			;				     (9)
	NOP			; Filler to get 14 clocks	- - - 1
	NOP			; Filler to get 14 clocks	- - - 1
	NOP			; Filler to get 14 clocks	- - - 1
	RETLW	0		;				- - - 2 
						;                    -- 
						;  Total             14
						;
;
; Given a valid measurement, calculate a Command and a PWM Constant.
;
;	Algorithim:
;	    Compute Delta = Count - midpoint;
;	    If (Delta < 0)
;		Command = Forward;
;	    else
;		Command = Reverse;
;	    Delta = ABS(Delta);
;	    if (Delta <= DEADBAND)
;		if (Delta == DEADBAND)
;			Command = Coast
;		else
;			Command = Brake;
;		PWM Constant = 100%.
;	    Else
;		PWM Constant = Min(MAX, delta - 8) * 100/MAX.
;		(for MAX == 100, this is simply (Min(MAX, delta - 8))
;	    Return.
;
;
GOT_PULSE:			;				(8)
	MOVLW	D'128'		; This is the midpoint.		1
	SUBWF	COUNT_LO,F	; 				1
	BTFSS	CARRY_BIT	; Carry is set (no Borrow)	2 1
	GOTO	DEAL_WITH_MINUS	; Figure out what to do now.	- 2 (13)
POSITIVE_RESULT:		;				    (+7)
	MOVLW	REVERSE_CMD	; Its probably reverse		1
	MOVWF	CMD_REG		; Store it in command register	1
NEW_PWM_VALUE:			;				        (21)
	MOVLW	DEADBAND	; Dead band is +/- 6 (50uS)	1
	SUBWF	COUNT_LO,F	; Subtract DEADBAND from count.	1
	BTFSS	CARRY_BIT	; If no borrow, continue	2 1
	GOTO 	DO_BRAKE	; In the dead zone do braking.	- 2 (19) (26)
	MOVLW	D'100'		; Check if its in the range	1
	SUBWF	COUNT_LO,W	; Subtract 99 (max range)	1
	BTFSC	CARRY_BIT	; Check if its Less than MAX	2  1
	GOTO	MAX_PWM		; Was >= 100 			-  2 (23) (30)
	MOVF	COUNT_LO,W	; Get count value.		1
DONE_PULSE:			;				  (26)(+12)(33)
	MOVWF	PWM_CMD		; This is the new PWM Constant	1  1
	INCF	PWM_CMD		; Adjust 0 - 99 -> 1 to 100%	1  1
	MOVLW	1		; Put 1 into INNER_CNT		1  1
	MOVWF	INNER_CNT	; 				1  1
	MOVWF	PWM_TMP		;				1  1
	MOVWF	COUNT_HI	; Reset counter			1  1
	MOVLW	GLITCH_COUNT	; for next time			1  1
	MOVWF	COUNT_LO	;				1  1
	MOVWF	GLITCH		; 				1  1
	MOVLW	D'41'		; Loop count + 1 		1  1
	MOVWF	OUTER_CNT	;				1  1
	RETLW	1		; Return true			2  2
;					Totals		       36  39
;					Totals	(max)	           46
;					Totals (reverse)       43  46
;					Totals (do_brake)      44  51
;
; Various exit times depending on the path. Since the spec says that
; another pulse won't start for at least 5mS the results are not
; critical but they are useful to know. They become the true spec for
; the minimum separation between pulses.
;
; Max time 51 * .2 = 10.20uS, Min time 36 * .2 = 7.2uS
;
MAX_PWM:
	MOVLW	D'100'		; Else max (does MIN(100, val))	1
	GOTO	DONE_PULSE	; Fix up loop values		2
;
; If the result was minus, it could be legitimately minus, or it could
; be that count was 0x100. We fix those by checking COUNT_HI.
;
DEAL_WITH_MINUS:
	BTFSC	COUNT_HI,0	; Is bit 0 a one? 		2  1
	GOTO 	ADJUST_RESULT	; Yes, so adjust		-  2
	MOVLW	FORWARD_CMD	; Its wider than the midpoint	1
	MOVWF	CMD_REG		; So this is the command.	1
	COMF	COUNT_LO,F	; Convert to a positive value	1
	INCF	COUNT_LO,F	; negate COUNT_LO		1
	GOTO	NEW_PWM_VALUE	; Now go compute PWM constant.	2
ADJUST_RESULT:
	MOVLW	D'128'		; 256 - 128  = 128		1
	MOVWF	COUNT_LO	; Fixup the low byte		1
	GOTO	POSITIVE_RESULT	; Now treat it normally.	2 
;
; Now if the value is -DEADBAND thru +DEADBAND we set the command to
; "BRAKE", if it was exactly -DEADBAND or +DEADBAND we set it to COAST.
; The reason for this is that given our sample rate/accuracy we find that
; we get 'jitter' in the input that results in a COUNT value that can move
; between two values on alternate samples. Given that variation, for input
; pulse widths that are 'near' DEADBAND-1 or -(DEADBAND-1) we could see
; alternating DEADBAND and DEADBAND-1 on the input. Without a one value
; buffer, this causes the output to alternate between a little on (1% PWM)
; and BRAKE. The motor then sits there and stutters. However by defining
; the value DEADBAND exactly to be '0' the output is either COAST/ON or
; COAST/BRAKE, both of which have a predictable and non-harmful output.
;
DO_BRAKE:
	MOVLW	DEADBAND	; Restore Count Value			1
	ADDWF	COUNT_LO,F	; Add it back to Count			1
	MOVLW	DEADBAND-1	; Check for boundary condition		1
	SUBWF	COUNT_LO,W	; Subtrack COUNT LO 			1
	BTFSC	ZERO_BIT	; Check to see if its zero		2 1
	GOTO	ON_THE_EDGE	; Its exactly DEADBAND - 1		- 2
	MOVLW	BRAKE_CMD	; This is the brake Command		1 -
ON_THE_EDGE:
	MOVWF	CMD_REG		; Store it				1
	MOVLW	D'100'		; Very large PWM Constant		1
	MOVWF	PWM_CMD		; So we get 100% braking action		1
	GOTO	DONE_PULSE	; Return indicating new value available.2 (12)
	END
