; QUAD.ASM      Digital Encoder Reader Module
; Written by    Chuck McManis (http://www.mcmanis.com/chuck)
; This Version  11-FEB-02
; Copyright (c) 2001 Charles McManis, All Rights Reserved
;
; Change Log:
;       11-FEB-02       Initial Design
;
; NOTICE: THIS CODE COMES WITHOUT WARRANTY OF ANY KIND EITHER
;         EXPRESSED OR IMPLIED. USE THIS CODE AT YOUR OWN RISK!
;         I WILL NOT BE HELD RESPONSIBLE FOR ANY DAMAGES, DIRECT 
;         OR CONSEQUENTIAL THAT YOU MAY EXPERIENCE BY USING IT.
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;
        TITLE "QUAD - Digital Encoder Reader"
        LIST P=PIC16F628, C=120, N=50, R=HEX
        include "P16F628.inc"
        include "16bits.inc"
       	__FUSES _CP_OFF&_XT_OSC&_WDT_OFF&_LVP_OFF
        ERRORLEVEL 2                

;
; Quadrature inputs from the digital encoder.
;
QUAD_A  EQU     1
QUAD_B  EQU     2        

BUT_S1  EQU     7
        
;
; Local variables ...
;
        CBLOCK  H'40'
           COUNT:2
           MSG_NDX
           Q_NOW
           Q_1
           QUAD_ACT
           TMP_B
        ENDC
        
        CBLOCK H'70'
            TMP_W     
            TMP_STATUS            
            EE_TMP:2            ; This needs to exist in both banks
        ENDC
        ORG H'0000'
        GOTO    INIT            ; Let's get this puppy rolling
;
; Code Section
;
; The code section of the ISR lives at 0x0004. Its possible to put a 
; jump here however doing so adds two clocks of latency to servicing 
; any interrupt.
;       
        ORG     H'0004'          ; Interrupt service routine     
        GOTO    INTR_PRE
;
; Return the value of what we should do to the value to adjust it.
;        
QUAD_ACTION:        
        ;
        ; Computed jump based on Quadrature pin state.
        ;
        CLRF    PCLATH  ; Must be in page 0!!!
        ADDWF   PCL,F   ; Indirect jump
        RETLW   H'00'   ; 00 -> 00
        RETLW   H'FF'   ; 00 -> 01 -1
        RETLW   H'01'   ; 00 -> 10 +1
        RETLW   H'00'   ; 00 -> 11
        RETLW   H'01'   ; 01 -> 00 +1
        RETLW   H'00'   ; 01 -> 01
        RETLW   H'00'   ; 01 -> 10 
        RETLW   H'FF'   ; 01 -> 11 -1
        RETLW   H'FF'   ; 10 -> 00 -1
        RETLW   H'00'   ; 10 -> 01
        RETLW   H'00'   ; 10 -> 10
        RETLW   H'01'   ; 10 -> 11 +1
        RETLW   H'00'   ; 11 -> 00
        RETLW   H'01'   ; 11 -> 01 +1
        RETLW   H'FF'   ; 11 -> 10 -1
        RETLW   H'00'   ; 11 -> 11
                
;        
;        
; PRESSED -- Jump to 'label' if the button is pressed.
;
PRESSED MACRO   label        
        BTFSS   PORTB, BUTTON
        GOTO    label
        ENDM
        
;        
; NOT_PRESSED -- Jump to 'label' if the button is not pressed.
;
NOT_PRESSED MACRO   label        
        BTFSC   PORTB, BUTTON
        GOTO    label
        ENDM
        
        
;
; QUAD State
;
; A quadrature encoder traverse a couple of states when
; it is rotating these are:
;       00      |  Counter
;       10      |  Clockwise
;       11      |     ^
;       01      V     |
;       00  Clockwise |
;
;
QUAD_STATE:
        BCF     STATUS,C        ; Force Carry to be zero
        MOVF    PORTB,W         ; Read the Quadrature encoder
        ANDLW   H'6'            ; And it with 0110
        MOVWF   Q_1             ; Store it
        RRF     Q_1,F           ; And rotate it right. 
        
        RLF     Q_NOW,F         ; Rotate Q_NOW Left
        RLF     Q_NOW,W         ; by two (this was last time)
        IORWF   Q_1,W           ; Or in the current value
        MOVWF   QUAD_ACT           ; Store at as the new action
        MOVF    Q_1,W           ; Get last time
        MOVWF   Q_NOW           ; And store it.
        MOVF    QUAD_ACT,W      ; Get the action
        CALL    QUAD_ACTION     ; Get thing to do
        ADDWF   COUNT,F         ; Add it to the count
        INCF    COUNT,W         ; Was it 0xFF?
        BTFSC   STATUS,Z        ; Skip if not
        CLRF    COUNT           ; Else reset it to zero
        SUBLW   D'202'          ; Was the incremented version 202?
        BTFSS   STATUS,Z        ; Skip this if it was
        RETURN
        MOVLW   D'200'          ; Count goes to max
        MOVWF   COUNT
        RETURN
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;    U T I L I T Y    I / O   R O U T I N E S
;
        #include util.asm
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;
        PAGE
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;       I N T E R R U P T   S E R V I C E   R O U T I N E S 
;
; The interrupt service routines for the various on-chip peripherals.
; INTR_PRE is the "pre-amble" that stores W and STATUS safely away
; INTR_POST is the "post-amble" that restores W and STATUS and returns
; to the previous point of execution.
;
; INTR_PRE takes 4 Tcy
; INTR_POST takes 6 Tcy
; Thus there is a built in 10 cycle overhead for interrupt services.
; Async interrupts can take 3 Tcy to dispatch and sync interrupts
; only take 2 Tcy to dispatch, so there is a 6 or 7 Tcy to first
; instruction of the ISR and a total of 13 Tcy (MAX) in overhead.
; 
; Interrupts are prioritized by the order in which they are tested
; for (polled). The general format is
;
;    ISR_n:
;       BTFSS   Reg, Flag
;       GOTO    ISR_n+1
;        ...
;    ISR_n+1:
;
; Thus there will be 3Tcy of latency for each ISR in the string.
; I'm using TMR0 as ISR_0 which means an interrupt faster than 
; about once every 100 clocks or so would be too fast (100 Tcy at
; 4 Mhz is 100 uS.)
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
INTR_PRE:        
        MOVWF   TMP_W           ; Copy W to a temporary register
        SWAPF   STATUS,W        ; Swap Status Nibbles and move to W 
        MOVWF   TMP_STATUS      ; Copy STATUS to a temporary register
        CLRF    STATUS          ; Force Bank 0
;
; State is saved, and we've expended 3 Tcy plus the 3 Tcy (4 worst 
; case) of interrupt latency for a total of 6(7) Tcy.
; 
; Now loop through until we've satisfied all the pending interrupts.
;
ISR_0:
        ; ... test bit to see if it is set
        BTFSS   INTCON,T0IF     ; Did Timeer0 Overflow?
        GOTO    ISR_1           ; No it didn't, so check next thing.
        ;
        ; Process Timer 0 Overflow Interrupt
        ;
        BCF     INTCON, T0IF    ; Clear Timer 0 interrupt
        MOVLW   D'133'          ; Reset counter to get 1 Khz interrupt
        MOVWF   TMR0            ; Store it.
        CALL    QUAD_STATE      ; Check the Quadrature Encoders.
        GOTO    ISR_1           ; Nope, keep counting
ISR_1:  
;
; Exit the interrupt service routine. 
; This involves recovering W and STATUS and then
; returning. Note that putting STATUS back automatically pops the bank
; back as well.
;               This takes 6 Tcy for a total overhead of 12 Tcy for sync
;               interrupts and 13 Tcy for async interrupts.
; 
INTR_POST:
        SWAPF   TMP_STATUS,W    ; Pull Status back into W
        MOVWF   STATUS          ; Store it in status 
        SWAPF   TMP_W,F         ; Prepare W to be restored
        SWAPF   TMP_W,W         ; Return it, preserving Z bit in STATUS
        RETFIE
                
        PAGE
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;     I N I T I A L I Z A T I O N   S E C T I O N
;
; Initialization, this is the start address for initializing the PIC.
; All peripherals are initialized and any memory locations that need
; to be pre-initialized (to values or to zero) is done here.
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
INIT:
        CLRF    STATUS          ; Set Bank 0
        CLRF    PORTA           ; Clear PortA
        CLRF    PORTB           ; and clear PortB
        MOVLW   H'07'           ; Make PortA Digital I/O
        MOVWF   CMCON           ; By setting CMCON<0:3>
        ; * * * * * *
        ; * BANK 1 Operations
        ; * * * * * *
        BSF     STATUS,RP0      ; Set Bank 1
        MOVLW   B'0000010'      ; Set TMR0 prescaler to 8
        MOVWF   OPTION_REG      ; Store it in the OPTION register
        CLRF    TRISA           ; Now A is all outputs
        CLRF    TRISB           ; B all outputs
        BSF     TRISB,BUT_S1    ; Button S1
        BSF     TRISB,QUAD_A
        BSF     TRISB,QUAD_B
        MOVLW   D'199'
        MOVWF   PR2             ; Set PWM Register
        ; * * * * * * * * * * *
        ; * BANK 0 Operations *
        ; * * * * * * * * * * *
BANK_0:        
        CLRF    STATUS          ; Back to BANK 0
        BSF     PORTB,LED1      ; Turn On LED1
        MOVLW   D'100'
        MOVWF   CCPR1L
        MOVLW   H'4'            ; TMR2 Enable 1:1 Prescale
        MOVWF   T2CON           ; Go
        MOVLW   b'1100'
        MOVWF   CCP1CON
        
        MOVLW   D'200'
        
        CALL    DELAY
        CALL    LCD_INIT
        CLR16    COUNT
        MOVLW   d'100'
        MOVWF   COUNT   
;
; The last thing we do is enable interrupts 
;        
        BSF     INTCON, T0IE    ; Enable Timer 0 to interrupt
        BCF     INTCON, T0IF    ; Reset flag that indicates interrupt
        BSF     INTCON, GIE     ; Enable interrupts
        BCF     PORTB, LED1
        
        PAGE
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;     M A I N   P R O G R A M
;
; This program prints out on the LCD the following text:
;
;         Pulse Width: 00000uS
;         Sample Num : 00000
;
; When interrupts on the CCP1 line occur the interrupt service routine
; measures the pulse width and stores it into the 16 bit CAPTURE value
; then increments the number of samples that it has seen and sets the
; boolean flag "GOT_ONE" in the FLAGS byte.
;
; This routine monitors the state of the GOT_ONE flag and when it
; becomes "true" (logic 1) it loops through its cycle redisplaying
; the message but now with updated pulse width and sample count
; numbers.
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;
; Print out a simple message
;
MAIN:
        PUTMSG MSG1, MSG_NDX
        MOVF    PORTB,W         ; Read portB
        MOVWF   TMP_B           ; Store it here for now
        RRF     TMP_B,F         ; Rotate it right one
        MOVF    TMP_B,W         ; Put it into W
        ANDLW   H'1'            ; Mask off the bit
        ADDLW   '0'
        CALL    LCD_CHAR
        RRF     TMP_B,F         ; Rotate it Right
        MOVF    TMP_B,W
        ANDLW   H'1'
        ADDLW   '0'
        CALL    LCD_CHAR
        MOVLW   H'C4'           ; Position on Next Line
        CALL    LCD_CMD
        PUTNUM  COUNT
        MOVF    COUNT,W
        MOVWF   CCPR1L
        GOTO    MAIN
                
        ORG H'0400'                                 
;
; * * * * * * * * * * * * * * * * * * *
; *   M E S S A G E   S E C T I O N   *
; * * * * * * * * * * * * * * * * * * *
; 
; Messages that are sent to the LCD are not required in the 
; production version (no LCD attached)
;
 
MSG1:   MOVLW   high MSG1
        MOVWF   PCLATH
        MOVF    MSG_NDX,W
        ADDWF   PCL,F     
        DT      H'80', "Current Count: ", 0
        END        

