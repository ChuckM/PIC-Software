; LCD.ASM - LCD Utility Routines
; Written: 02-JAN-03
; Copyright (C) 2002, 2003 Charles McManis, All Rights Reserved
; $Id: lcd.asm,v 1.2 2003-01-02 18:01:01-08 chuck_mcmanis Exp chuck_mcmanis $
;
; This file defines three LCD Functions:
;       LCD_INIT:       This function will initialize the LCD to 4-bit
;                       mode, turn on the display, and clear it.
;       LCD_CMD:        This function will send a "command" to the LCD
;                       which is used to download characters etc.
;       LCD_CHAR:       This function will send a character to the LCD
;                       which is then displayed in the current cursor
;                       position.
;       LCD_DELAY:      This function is a 'spin loop' delay utility to
;                       get the timing right to the LCD.
;
; The LCD is attached to the Servo Gizmo board thusly:
;
;       RA0 - DB4       RB4 - RS
;       RA1 - DB5       RB5 - E
;       RA2 - DB6 
;       RA3 - DB7

LCD_E   EQU     5
LCD_RS  EQU     4

;
; The LCD needs to be initialized into 4 bit mode and then
; we can write letters to it. Note that the LCD is ALWAYS 
; in write mode, no reading allowed so you just have to 
; hope it isn't busy. (Usually it isn't :-)
;
; According to the Hitachi data sheet, you first have to
; wait 15 mS to insure the display is "stable" after Vcc is
; applied. Then the following sequence puts it into "4 bit"
; mode (reset by instruction)
;               0x03            ' wait > 4.1 mS
;               0x03            ' wait > .1 mS
;               0x03            ' wait > .1 mS
;               0x02            ' wait > .1 mS
;
; Display is now in 4-bit mode so initialize it with:
;               0x02 0x08  ' Set 4-bit mode, 2 line display
;               0x00 0x08  ' Display "ON"
;               0x00 0x01  ' Clear Display
;               0x00 0x06  ' Entry mode, auto increment cursor
;
LCD_INIT:
        MOVLW   D'200'          ; Wait for LCD to settle (20mS)
        CALL    LCD_DELAY
        MOVLW   H'03'           ; Set LCD for 4 bits        
        MOVWF   PORTA           ; Data Lines
        BCF     PORTB,LCD_RS    ; Clear RS line
        BSF     PORTB,LCD_E     ; Toggle E
        BCF     PORTB,LCD_E     ; 
        MOVLW   H'50'           ; Wait 5 mS
        CALL    LCD_DELAY
        
        BSF     PORTB, LCD_E    ; Send command again
        BCF     PORTB, LCD_E
        MOVLW   H'2'      
        CALL    LCD_DELAY
                   
        BSF     PORTB, LCD_E    ; Third time's the charm
        BCF     PORTB, LCD_E
        MOVLW   H'2'      
        CALL    LCD_DELAY
        
        MOVLW   H'02'           ; Set for 4 bits
        MOVWF   PORTA           ; like so                          
        BSF     PORTB, LCD_E    ; 
        BCF     PORTB, LCD_E
        MOVLW   H'2'            ; Wait .2 mS
        CALL    LCD_DELAY
        ; 
        ; Now at this point its in 4-bit mode so send setup
        ; commands through the 4-bit interface.
        ;        
        MOVLW   H'2A'           ; Set 2 line display, 4 bit I/O
        CALL    LCD_CMD         ; 
        MOVLW   H'08'           ; Turn off the Display
        CALL    LCD_CMD
        MOVLW   H'01'           ; Clear the contents of the display
        CALL    LCD_CMD
        MOVLW   H'06'           ; Set the Entry mode
        CALL    LCD_CMD
        MOVLW   H'0C'           ; Turn it on again
        CALL    LCD_CMD
        RETURN                  ; Ready to rock and roll
        
;
; LCD_CMD
;
; Generic routine to send a command to the LCD. Since some
; commands take longer to run this routine waits 1mS after it
; sending the command to insure the LCD is done with it.
;
LCD_CMD:
        MOVWF   LCD_TMP         ; Store command
        MOVLW   H'1'            ; This is the generic delay
        MOVWF   CMD_DELAY       ; Used by default
        MOVLW   H'FC'           ; This is how we check for clear/home
        ANDWF   LCD_TMP,W       ; If any bit other than 0 or 1 is set
        BTFSS   STATUS,Z        ; 
        GOTO    OK_DELAY        ; If non-zero leave delay alone
        MOVLW   D'20'           ; Else store 2mS delay value.
        MOVWF   CMD_DELAY
OK_DELAY: 
        SWAPF   LCD_TMP,W       ; Read it, put upper nibble down
        ANDLW   H'0f'       
        BCF     PORTB,LCD_RS    ; Turn OFF the R/S bit
        MOVWF   PORTA           ; Out it goes
        BSF     PORTB,LCD_E     ; Clock it out
        BCF     PORTB,LCD_E     ; Like so
        MOVF    LCD_TMP,W       ; Get lower nybble
        ANDLW   H'0F'           ; Turn off R/S
        MOVWF   PORTA           ; Put it on PortA
        BSF     PORTB,LCD_E     ; Clock it out
        BCF     PORTB,LCD_E     ;
        MOVF    CMD_DELAY,W     ; Wait for it to complete
        CALL    LCD_DELAY
        RETURN                  ;

;
; LCD_CHAR
;
; Generic routine to send a command to the LCD. In this
; version it just sends it, a "smarter" version would watch
; for <CR> or <LF> and do something appropriate to the display.
;
LCD_CHAR:
        MOVWF   LCD_TMP         ; Store it in LCD_TMP
        SWAPF   LCD_TMP,W       ; Upper Nybble
        ANDLW   H'0F'           ; Clear upper bits
        BSF     PORTB,LCD_RS    ; Turn On R/S bit
        MOVWF   PORTA           ; Put it out to PortA
        BSF     PORTB,LCD_E     ; Clock it out
        BCF     PORTB,LCD_E     ;
        MOVF    LCD_TMP,W       ; Get the lower nybble
        ANDLW   H'0F'           ; Clear upper bits
        BSF     PORTB,LCD_RS    ; Turn On R/S bit
        MOVWF   PORTA           ; Out to PORTA
        BSF     PORTB, LCD_E    ; Clock it out
        BCF     PORTB, LCD_E    ; 
        MOVLW   H'2'            ; Wait a bit
        CALL    LCD_DELAY
        RETURN
;
; Wait Macro
; 
; This macro comes from the "Advanced" PIC programming seminar
; and it will build a delay of 'n' machine cycles which are 
; 1/4 the clock frequency. So for a 4Mhz crystal a delay of
; 1 cycle is 4 ticks, or 1 instruction cycle which is 1uS. 
;
LCD_WAIT MACRO   CYCLES
        nolist
X SET (CYCLES) % 4
        IF (X == 1) || (X == 3)
            NOP
        ENDIF
        
        IF (X == 2) || (X == 3)
            GOTO $+1
        ENDIF
        
X SET (CYCLES) / 4
        IF (X)
            IF (X == 256)
X SET 0
            ENDIF
            
            MOVLW   X
            ADDLW   -1
            SKPZ
            GOTO    $-2
        ENDIF
        list
        ENDM
        
;
; Delay Loop
; 
; This function is designed to delay for 100uS times the number
; of times through the loop
; Once through :
;               1+91+2+1+2+1+2 = 100
;                   
; Twice through
;               1+91+2+1+1+2+96+1+2+1+2 = 200
;                            ******
; Thrice through
;               1+91+2+1+1+2+96+1+1+2+96+1+2+1+2 = 300
;                            ******** ******
; "N" times through (n * 100) cycles.
;
; NOTE: This will have to be changed when you change the 
; frequency from 4Mhz.
;
LCD_DELAY:
        MOVWF   DLY_CNT         ; Store delay count        (1)
        LCD_WAIT 91             ; Delay 95
        GOTO    D2_LOOP         ; Check to see if it was 1 (2)
D1_LOOP:
        LCD_WAIT 96             ; (20)
D2_LOOP:        
        DECF    DLY_CNT,F       ; Subtract 1            (1)
        INCFSZ  DLY_CNT,W       ; Check for underflow   (2)
        GOTO    D1_LOOP         ; Not zero so loop      (2)
        NOP
        RETURN
                                                 
