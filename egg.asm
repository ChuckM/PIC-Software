; vim: set syntax=pic :
; Emergency Egg Locator Beacon
; Written 02-Nov-2004
; Copyright (C) 2004, Charles McManis, All Rights Reserved
; 
; A non-exclusive, non-transferrable right to use, modify, or 
; distribute is granted to non-commercial purposes.
;
; This is a fairly simple hack that is fun and educational. 
; All the way back to the 1950's scientists have known that
; putting an AM radio near the computer allowed you to "pick up" 
; interference related to what was going on inside. Some creative
; individuals figured out they could make timing loops that actually
; generated tones such that music could be played.
;
; This program brings that hack into the 21st century (or at least
; late 20th century) using a PIC12F6xx chip. The interesting thing
; about this chip is that it has an internal 4Mhz oscillator, that
; you can route as a divide/4 value (1Mhz) to one of its output pins.
; 1Mhz happens to be smack dab in the middle of the AM radio band.
; The other feature of PIC chips that comes into play is that they 
; can drive 25mA or sink 25mA with their I/O pins. 
;
; So take the 1Mhz output, modulate it with another pin, and voila'
; instant CW AM transmitter. Given an inexpensive AM radio the signal
; generated can be picked up for 30 - 50' away. Probably more if you
; had a directional antenna on your reciever. 
;
; CHUCK MCMANIS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE
; SUITABILITY OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING
; BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT. CHUCK MCMANIS
; SHALL NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT
; OF USING, MODIFYING OR DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
;
;
	TITLE "Emergency Egg Locator Beacon v1.0"
	LIST P=PIC12F629, C=120, N=50, R=HEX
	include "P12F629.inc"
	__FUSES _CP_OFF&_WDT_ON&_MCLRE_OFF&_INTRC_OSC_CLKOUT&_BODEN_ON&_PWRTE_ON
	ERRORLEVEL 2

	CBLOCK H'20'
		BIT_COUNT:1		; One byte of BIT Count
		BYTE_COUNT:1	; Counter for bytes
		BIT_DELAY:1		; Delay for one bit.
		BOOLS:1			; Our flags register
		ISR_W			; Storage for W
		ISR_FSR			; Copy of the FSR
		ISR_STATUS		; Copy of the Status register
		EE_SRC			; EEPROM Source Pointer
		EE_DST			; EEPROM Destination Pointer
		EE_LEN			; EEPROM Byte count
		MSG_BUF			; Buffer byte for message
		MSG:16			; 16 Bytes (128 bits) for our message
	ENDC


BIT_WIDTH	EQU	D'100'	; 100 mS or .1 seconds
BIT_FLIP	EQU 0		; Bit 0 of our bools is 'time to flip'

T0_CONST_4MHZ   EQU     D'133'  ; Constant for 1mS ticks at 4Mhz

	ORG	H'2100'
;
; 16 Bytes of Message 
; in this case EELB ALPHA ONE
; 
;
EGG_MSG:
;		B'01234567'
;		  E   E       LLLLLLLLLLLLL   BBBBBBBBBBBBB
	DE	B'10001000',B'10111010',B'10001110',B'10101000'
;	         AAAAA       LLLLLLLLLLLLL   PPPPPPPPPPPPPPP
	DE	B'00010111',B'00010111',B'01010001',B'01110111'
;	      PP   HHHHHHHHHHH   AAAAAAAAA           OOOOOO      NNNNNNNNN   E
	DE	B'01000101',B'01010001',B'0111000', B'00010101',B'00011101',B'00010000'
	DE	B'00000000'		;
EGG_MSG_SIZE	EQU	$-EGG_MSG

;
; EEREAD macro
; Written 12-Apr-2002, Chuck McManis
;
; Uses the temporary variable EE_LEN, EE_SRC, and EE_DST
; During read, Source is EEPROM and Destination is RAM
;
; Read a block of EEPROM and store it in RAM
; Assumes RAM block is in BANK0.
;                                  
EEREAD  MACRO   SRC_ADDR, DST_ADDR, LEN
        LOCAL   RLOOP
        MOVLW   LEN             ; Number of bytes to copy
        MOVWF   EE_LEN          ; Store the byte count.
        MOVLW   SRC_ADDR        ; Get the source address in EEPROM
        MOVWF   EE_SRC          ; Store it here.
        MOVLW   DST_ADDR        ; Get the destination address
        MOVWF   EE_DST          ; Store it here.
RLOOP:        
        MOVF    EE_DST,W        ; Get byte destination
        MOVWF   FSR             ; Store it in the FSR
        BSF     STATUS, RP0     ; Switch to bank 1 
        MOVF    EE_SRC,W        ; Address to read
        MOVWF   EEADR           ; Start address
        BSF     EECON1, RD      ; Read the byte
        MOVF    EEDATA, W       ; Assume a single cycle read?
        BCF     STATUS, RP0     ; Back to bank 0
        MOVWF   INDF            ; Store in Destination
        INCF    EE_SRC,F        ; Next source address
        INCF    EE_DST,F        ; Next destination address
        DECFSZ  EE_LEN,F        ; Decrement the count
        GOTO    RLOOP           ; Loop if not done.
        ENDM    


	ORG	H'0000'
INIT:	

	GOTO	MAIN

;
; The only interrupt source in this program is timer0. I've empirically
; determined a 'reload' value such that the interrupts occur at approximately
; 1mS intervals. If we toggle a pin, the pin generates a squarewave with a
; frequency of approximately 500Hz (somewhere above middle A 440 in the scale)
; This pin is tied externally to a voltage divider to modulate the CLKOUT
; signal.
;
; Additionally there is a counter for determining one "bit" width. Each bit 
; is held for approximately .1s so three "one" bits in a row generates a
; .3s tone and a single one bit generates a .1s tone.
;
	ORG		H'0004'
ISR_ENTER:
	MOVWF	ISR_W		; Preserve W
	SWAPF	STATUS,W	; And Status register
	CLRF	STATUS		; Force Page 0
	MOVF	FSR,W		; Get the FSR
	MOVWF	ISR_FSR

	; Check to see if it is Timer 0 interrupting us (it should be!)
	BTFSS 	INTCON, T0IF
	GOTO	ISR_EXIT		; Else exit
	BCF		INTCON, T0IF
	MOVLW	T0_CONST_4MHZ	; 1Khz interrupt 500hz waveform
	MOVWF	TMR0


	BTFSC	BOOLS, BIT_FLIP	; If this is true we're waiting for next bit
	GOTO	ISR_EXIT		; next ...

	CLRWDT
	BTFSS	MSG_BUF,7		; Check high order bit of MSG buffer
	GOTO	NO_TONE			; If zero don't generate a tone


	; 
	; Generates a tone
	; Should generate 500 hz on GPIO.5 (0x20 is 00100000b)
	;
TONE:
	MOVF	GPIO,W		; Read the GPIO Register
	XORLW	H'20'		; Toggle GPIO,0
	MOVWF	GPIO		; Write it out 
	BSF		GPIO,GPIO2	; Turn on GPIO2
	GOTO	BIT_TIMER

NO_TONE:
	MOVF	GPIO,W		; Get GPIO pins
	IORLW	H'20'		; Set output high
	MOVWF	GPIO		; Write it out
	BCF		GPIO,GPIO2	; Turn off GPIO2

;
; run down the clock on the bit timer.
;
BIT_TIMER:
	DECFSZ	BIT_DELAY		; This is the beep timer
	GOTO	ISR_EXIT		; Not timed out.
	MOVLW	BIT_WIDTH
	MOVWF	BIT_DELAY
	BSF		BOOLS, BIT_FLIP	; Next bit please

;
; Worst case the ISR takes about 26 instructions or 26 uS out of 1mS
;
ISR_EXIT:
	MOVF    ISR_FSR,W       ; Get FSR register
	MOVWF   FSR             ; put it back into FSR
	SWAPF   ISR_STATUS,W    ; Pull Status back into W
	MOVWF   STATUS          ; Store it in status 
	SWAPF   ISR_W,F         ; Prepare W to be restored
	SWAPF   ISR_W,W         ; Return it, preserving Z bit in STATUS
	RETFIE

;
; Initialize Timer 0 to drive everything
;
	
MAIN:
	MOVLW	BIT_WIDTH		; Basic bit width
	MOVWF	BIT_DELAY		; Store it
	BSF		BOOLS, BIT_FLIP	; 
	CLRF	GPIO			; Clear GPIO Bits
	BSF		GPIO, GPIO5
	CLRF	MSG_BUF			; Make sure MSG byte 0 is clear
	MOVLW	07h				; Make GPIO bits digital
	MOVWF	CMCON			; Like so.
	BSF     STATUS, RP0     ; Set Bank 1
	CALL	H'03FF'			; Get calibration constant
	MOVWF	OSCCAL			; Store it
	MOVLW   B'0000010'      ; Set TMR0 prescaler to 8
	MOVWF   OPTION_REG      ; Store it in the OPTION register
	BSF     INTCON, T0IE    ; Enable Timer 0 to interrupt
	BCF     INTCON, T0IF    ; Reset flag that indicates interrupt
	MOVLW	0				; Make GPIO all outputs
	MOVWF	GPIO			; Using the TRISO register in bank 1
	BCF     STATUS, RP0     ; Now in bank 0.
	BSF		INTCON, GIE		; Enable interrupts
;
; The purpose of this code is to read out the data bits in
; EEPROM and then one by one put them out on the LED pin
; (GPIO5 in this case). To send morse, the convention of three
; one bits is a dash, one one bit is a dit, and three zero bits
; are a space. 
; 128 bits lets you send about 10 characters in morse.
;
; In C code, this is what is going on:
; 
; memcpy(eeprom, msg, 16); /* get message from EEPROM */
; for (i = 0; i < 16; i++) {
;	msg = data[i];
;	flip = 0; /* reset bit timer */
;	for (j = 0; j < 8; j++) {
;		while (!flip); /* wait time allotment */
;		msg = msg >> 1;	/* shift out a bit */
;	}
; }
; msg = 0;
; halt(); 
;
OUTER:
;
; Copy message from EEPROM to the MSG memory
;
	EEREAD	EGG_MSG, MSG, EGG_MSG_SIZE	; Read msg bytes out of the EEPROM
	MOVLW	EGG_MSG_SIZE
	MOVWF	BYTE_COUNT
	MOVLW	MSG
	MOVWF	FSR
MSG_LOOP:
	MOVF	INDF,W		; Read message byte
	MOVWF	MSG_BUF		; Store it in the message buffer byte
	MOVLW	D'8'		; Send 8 bits
	MOVWF	BIT_COUNT	; out.
BIT_LOOP:
	BCF		BOOLS, BIT_FLIP	; Send next bit out
	BTFSS	BOOLS, BIT_FLIP	; Wait for it to flip
	GOTO	$-1
	RLF		MSG_BUF,F		; Rotate next bit out
	DECFSZ	BIT_COUNT,F
	GOTO	BIT_LOOP		; 
	INCF	FSR,F		; Point to next byte of message
	DECFSZ	BYTE_COUNT,F	; Until all sent
	GOTO	MSG_LOOP
;
; Now ideally the chip should go to sleep here, and wake up again in 10
; seconds or so and re-transmit. But its hard to get a fix on it that way
; so for now we just loop back to the beginning and do it all again.
;
	GOTO OUTER

	END
       
