; WSM2.ASM      Weapons Systems Control Module
; vi: set syntax=pic :
; Written by    Chuck McManis (http://www.mcmanis.com/chuck)
; This Version  25-JUL-02
; Copyright (c) 2002 Charles McManis, All Rights Reserved
; RCS Version
; $Id: wsm2.asm,v 1.0 2003-12-16 21:33:53-08 cmcmanis Exp cmcmanis $
;
; Change Log:
;       28-NOV-02       Updated with Timer2 clock ticks.
;       25-JUL-02       Created from Version 1, new board/design
;       05-JAN-02       Created from the pulse measurement source
;       07-APR-02       Modified from SERVO_PRE for Rev C ESC.
;       25-Apr-02       Created from the PIC1 code.
;
; NOTICE: THIS CODE COMES WITHOUT WARRANTY OF ANY KIND EITHER
;         EXPRESSED OR IMPLIED. USE THIS CODE AT YOUR OWN RISK!
;         I WILL NOT BE HELD RESPONSIBLE FOR ANY DAMAGES, DIRECT 
;         OR CONSEQUENTIAL THAT YOU MAY EXPERIENCE BY USING IT.
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;
        TITLE "WSM 2.0 - Weapons System Module"
        LIST P=PIC16F628, C=120, N=50, R=HEX
        include "P16F628.inc"
        include "inc\16bits.inc"
        include "inc\util.inc"
        include "inc\sg.inc"
       	__FUSES _CP_OFF&_XT_OSC&_WDT_OFF&_LVP_OFF
        
        ERRORLEVEL 2     
;
; These are the "factory" values for minimum, mid, and 
; maximum servo inputs. Followed by the scaling factor
; to convert a value between 0 and 500 to one between 0-200. 
;
        ORG     H'2100'
FACT_CAL:        
        DE      low D'1072', high D'1072'
        DE      low D'1522', high D'1522'
        DE      low D'1922', high D'1922' 
        DE      B'10010000'                     ; 2.25 in fixed point
        DE      B'10010000'
;
; These are the User calibration values. They get over written
; during the calibration process.
;        
USER_CAL:
        DE      low D'1072', high D'1072'
        DE      low D'1522', high D'1522'
        DE      low D'1922', high D'1922' 
        DE      B'10010000'                     ; 2.25 in fixed point
        DE      B'10010000'
        
        CBLOCK H'20'
            ; 
            ; Used by the Pulse Capture ISR
            ;
            LEADING:2
            CAPTURE:2
            ;
            ; Used in the Main loop for scaling and creating
            ; messages.
            ;
            SCALE_TMP
            CMD_TMP
            TEMP_VALUE:2
            R_MIN:2             ; Computed minimum and Maximum
            R_MAX:2
            ;
            ; Message passing mechanism between ISR's and MAIN
            BOOLS               ; Boolean values
            ;
            ; Used by the Timer 0 ISR to compute time outs
            ; Note: it is Tee-Zero-underscore-COUNT
            ;
            T0_COUNT
            RELAY_TO:2          ; Relay timeout in Ms
            ;
            ; EEPROM/Calibration data Keep together!!!
            ;
            DATA_START:0
            CAP_MIN:2           ; Minimum value captured
            CAP_MID:2           ; Neutral value (midpoint) captured
            CAP_MAX:2           ; Maximum value captured
            SCALE_FWD           ; Forward Scaling factor
            SCALE_REV           ; Reverse Scaling factor
            DATA_END:0
        ENDC
;
; Compute the size in bytes of the data we keep in the EEPROM
;        
DATA_SZ EQU  ( DATA_END - DATA_START )  ; Size of the data block            
        
;
; These commands sent to PORTA will cause either the FWD relay to
; energize or the REV relay to energize.
;
FWD_RELAY EQU           H'01'
REV_RELAY EQU           H'04'
OFF_RELAY EQU           H'0'

GUARD_BAND EQU     10      ; 5 uS on either side of neutral
;
; Flag definitions for our flags values.
;
GOT_ONE EQU     0               ; Boolean "Got One"    
UI_MODE EQU     1               ; Calibrating    
VALID   EQU     2               ; Valid pulses received
UPDATE  EQU     3               ; Update of EEPROM required.
TIMER   EQU     4               ; Timer expired.
ACTIVE  EQU     5               ; Command is active

CMD_TIMEOUT     EQU     D'500' ; one second activation time

I_CNT   EQU     D'20'           ; Reset output if you get nothing
                                ; for 60 mS.
        ORG H'0000'
        GOTO    INIT            ; Let's get this puppy rolling
;
; Code Section
;
; The code section of the ISR lives at 0x0004. Its possible to put a 
; jump here however doing so adds two clocks of latency to servicing 
; any interrupt.
;       
        ORG     H'0004'         ; Interrupt service routine     
        ISR_ENTER               ; Enter the Interrupt Service routine                
;                   
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;       I N T E R R U P T   S E R V I C E   R O U T I N E S 
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;
;
        BTFSS   PIR1, TMR2IF    ; Timer 2 is the real time (1ms) Clock
        GOTO    ISR_1           ; If it hasn't fired, check the servo
        BCF     PIR1, TMR2IF    ; If we saw a timer 2 tick note it.
        CALL    BUTT_STATE      ; Manage the button UI
        MOVF    RELAY_TO,W      ; Is this value zero?
        IORWF   RELAY_TO+1,W    ; 
        BTFSC   STATUS, Z       ; Nope so decrement it.
        GOTO    ISR_0A          ; Otherwise ignore it.
        ;
        ; 
        DEC16   RELAY_TO
        BTFSS   STATUS, Z       ; Did it just hit zero?
        GOTO    ISR_0A          ; Nope so continue
        BSF     BOOLS, TIMER    ; Then signal a timeout
ISR_0A: DECFSZ  T0_COUNT,F      ; Decrement interrupt counter
        GOTO    ISR_1           ; Nope, keep counting
        ;
        ; Count underflows when we've hit this interrupt "n" times,
        ; where n is the number in COUNT.
        ;
        MOVLW   I_CNT           ; Restore counter value
        MOVWF   T0_COUNT
        MOV16   CAP_MID, CAPTURE  ; Neutral
        BSF     BOOLS, GOT_ONE
        BCF     BOOLS, VALID
        BCF     PORTB, SIG_LED      ; LED management, turn off Signal LED
        ;
        ; Process interrupts from the Input Capture/Compare pin
        ; (CCP1 on the 16F628)
        ;
ISR_1:  
        BTFSS   PIR1, CCP1IF    ; Check to see that CCP1 interrupted
        GOTO    ISR_2           ; If not continue
        BCF     PIR1, CCP1IF    ; Re-enable it
        BTFSS   CCP1CON, CCP1M0 ; Check for falling edge watch
        GOTO    FALL_EDGE       ; Go pick up the falling edge
        MOVF    CCPR1L,W        ; else store leading edge value
        MOVWF   LEADING         ; into 16 bit work LEADING
        MOVF    CCPR1H,W
        MOVWF   LEADING+1
        BCF     CCP1CON, CCP1M0 ; Now capture the trailing edge
        GOTO    ISR_2           ; Exit the interrupt service routine
        
FALL_EDGE:
        BSF     CCP1CON, CCP1M0 ; Re-set for trailing edge capture
        MOVF    CCPR1L,W        ; Store the captured value into
        MOVWF   CAPTURE         ; CAPT_LO and ...
        MOVF    CCPR1H,W
        MOVWF   CAPTURE+1       ;             ... CAPT_HI
        ;
        ; 16 bit subtract 
        ;     CAPTURE = CAPTURE - LEAD
        ;
        SUB16   CAPTURE, LEADING
        
        BSF     BOOLS, GOT_ONE  ; Indicate we have a new sample.
        BSF     BOOLS, VALID    ; Indicate we're getting valid cmds
        BSF     PORTB, SIG_LED  ; LED Management, got a valid signal
        MOVLW   I_CNT           ; Reset timeout count
        MOVWF   T0_COUNT
ISR_2:                          ; Process the next interrupt
                                ; All done so exit.
        ISR_EXIT
                
        PAGE
        include "button.asm"
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
        CLRF    B_STATE
        CLRF    BOOLS
        CLRF    PORTA           ; Clear PortA
        CLRF    PORTB           ; and clear PortB
        MOVLW   H'07'           ; Make PortA Digital I/O
        MOVWF   CMCON           ; By setting CMCON<0:3>
        
        ; * * * * * *
        ; * BANK 1 Operations
        ; * * * * * *
        BSF     STATUS,RP0      ; Set Bank 1
        CLRF    TRISA           ; Now A is all outputs
        CLRF    TRISB           ; B all outputs
        BSF     TRISB,BUTTON    ; Button S1 input
        BSF     TRISB,SERVO     ; CCP1 is an input.
        BSF     TRISA,W1        ; W1 input
        BSF     TRISA,W2        ; W2 input
        BSF     PIE1, CCP1IE    ; Enable interrupts from CCP1
        BSF     PIE1, TMR2IE    ; Enable interrupts from Timer 2
        MOVLW   D'250'          ; At 4Mhz this gives us 1mS interrupts.
        MOVWF   PR2             ; Store that in PR2
        
        ; * * * * * * * * * * *
        ; * BANK 0 Operations *
        ; * * * * * * * * * * *
        CLRF    STATUS          ; Back to BANK 0
        MOVLW   B'00000001'     ; Enable Timer 1 1:1 Prescale
        MOVWF   T1CON
        MOVLW   B'00000101'     ; Capture mode rising edge
        MOVWF   CCP1CON
        MOVLW   B'00000101'     ; Timer 2 on 4:1 prescaler, 1:1 Postscaler
        MOVWF   T2CON

        ; * * * * * * * * * * * * * *
        ; * Non-Chip initialization *        
        ; * * * * * * * * * * * * * *
        
        BSF     PORTB,CAL_LED   ; Turn On CAL
        CALL    IDLE_0          ; Initialize state machine.
        CLR16   RELAY_TO        ; Clear timeout on relays
        CLRF    BOOLS           ; Clear flags
        CLR16   CAPTURE
                                                           
        ;
        ; Load calibration data
        ;
        EEREAD  USER_CAL, DATA_START, DATA_SZ
;
; The last thing we do is enable interrupts 
;        
        BSF     INTCON, PEIE    ; Enable Peripheral Interrupts
        BSF     INTCON, GIE     ; Enable interrupts
        BCF     PORTB, CAL_LED
        ;
        ; Enable the Relays? XXX Should use these to change state?
        ;
        BSF     PORTB, ENA_1N2
        BSF     PORTB, ENA_3N4
        
        BTFSC   PORTA, W2       ; Are we doing a Factory Reset?
        GOTO    MAIN            ; No we're not
        EEREAD  FACT_CAL, DATA_START, DATA_SZ
        BSF     BOOLS, UPDATE   ; Tell the MAIN loop to reset us
        PAGE

; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;     M A I N   P R O G R A M
;
; This loop creates a simple priority scheme, when we start running
; at main, if we're in "UI_MODE", which means the user is calibrating
; then we just loop around to main. If not, then the following
; priority is enforced:
;       - Highest Priority : update the flash. If we have an update
;               ready to go we update it.
;       - Second Highest : new servo command has arrived. If we've
;               received a pulse we send that command
;       - Third Priority : we run a temperature scan and send an
;               update if it exceeds a threshold.
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

MAIN:
        BTFSC   BOOLS, UI_MODE  ; Are we doing re-cal?
        GOTO    MAIN            ; Yes so just loop here...
        BTFSC   BOOLS, TIMER    ; Check for timer ticks
        GOTO    TIMER_TICK      ; Process a timer tick
        BTFSC   BOOLS, UPDATE   ; Do we need to update EEPROM?
        GOTO    DO_UPDATE       ; Yes, so update EEPROM ...
        BTFSC   BOOLS, GOT_ONE  ; Did we get a Servo Input
        GOTO    DO_CMD          ; Yes, so process that...
        GOTO    MAIN
        ;
        ; On timed activation mode, the relay code will set
        ; RELAY_TO to be the relay time out in mS, if it goes from
        ; non-zero to zero the interrupt routine will set the timer
        ; flag.
        ;
TIMER_TICK:
        BCF     BOOLS, TIMER
        MOVLW   OFF_RELAY       ; Turn off the relays
        MOVWF   PORTA           ; Like so.
        BCF     BOOLS, ACTIVE
        GOTO    MAIN
        ;
        ; If updated capture data has been computed 
        ; then store new values in the EEPROM.
        ;
DO_UPDATE:        
        BCF     BOOLS, UPDATE
        EEWRITE DATA_START, USER_CAL, DATA_SZ
        GOTO    MAIN
        
        ;
        ; XXX need to not execute on first command...
        ;
        ; Process a servo Command
        ;       Adjust it for guardband
        ;       Scale it between 0 and 200
        ;       Extract the command (fwd, rev, brake)
        ;       Put it into the "CMD WANT" register.
        ;
        ; scale it to 0 to 200 
        ;
DO_CMD:        
        BCF     BOOLS, GOT_ONE
        ;
        ; First Clip the value to CAP_MIN <= x <= CAP_MAX
        ;
        CMP16   CAPTURE, CAP_MAX        ; Is it greater than MAX?
        SKPC
        GOTO    NOT_MAX
        MOV16   CAP_MAX, CAPTURE        ; Use MAX instead
NOT_MAX:        
        CMP16   CAPTURE, CAP_MIN
        SKPNC
        GOTO    NOT_MIN
        MOV16   CAP_MIN, CAPTURE
NOT_MIN:
        MOV16   CAPTURE, TEMP_VALUE     ; May have been modified.
        SUB16   TEMP_VALUE, CAP_MID     ; Get magnitude from Neutral
        SKPC                            ; If carry set result was +
        GOTO    NEG_MAGNITUDE           ; Yes, so process it.
        MOVLW   FWD_RELAY               ; Potentially activating #1 solenoid
        MOVWF   CMD_TMP                 ; Store in the packet
        MOVF    SCALE_FWD,W             ; Get forward scaling factor
        MOVWF   SCALE_TMP               ; and put it here for now.
        GOTO    FINISH_UP               ; 
NEG_MAGNITUDE:
        MOVF    SCALE_REV,W             ; Get scaling factor
        MOVWF   SCALE_TMP               ; Store it here for now
        MOVLW   REV_RELAY
        MOVWF   CMD_TMP
        NEG16   TEMP_VALUE              ; Negate value 
FINISH_UP:
        CMPI16  TEMP_VALUE, GUARD_BAND  ; Guard band around 0
        SKPC                            ; Greater than guard band.
        GOTO    STOP_COMMAND            ; Yup, change it to a stop
        ;
        ; Otherwise we scale it to a value between 0 and 200
        ; using the cached scaling constant.
        ;
        SUBI16   TEMP_VALUE, GUARD_BAND ; Subtract it from result.
        MOV16   TEMP_VALUE, DIVIDEND    ; Put in the dividend
        MOVF    SCALE_TMP,W             ; Get scaling factor
        MOVWF   DIVISOR                 ; Store in low byte of divisor
        CLRF    DIVISOR+1               ; Clear upper byte
        CALL    FP_DIVIDE               ; Do the divide.
        GOTO    DO_COMMAND              ; Now result has 0 - 200 in it.
STOP_COMMAND:
        MOVLW   OFF_RELAY
        MOVWF   CMD_TMP   
DO_COMMAND:             
        ;
        ; Now decide if we need to activate the relays
        ;
        BTFSC   BOOLS, ACTIVE   ; Is the last command still active?
        GOTO    MAIN            ; Yes, so just loop
        MOVF    CMD_TMP,W       ; Check to see if its an "OFF" command
        SUBLW   OFF_RELAY       ; Using a subtract
        BTFSS   STATUS,Z        ; Skip on zero
        GOTO    NOT_OFF         ; Else it isn't an off command
        MOVLW   OFF_RELAY       ; Get OFF command
        MOVWF   PORTA           ; Store it on PORTA
        GOTO    MAIN            ; And Loop
        ;
        ; Not an Off command if we get here.
NOT_OFF:        
        MOVF    RESULT,W        ; Else see if there is one to do...
        SUBLW   D'150'          ; Compare it against 150
        BTFSS   STATUS,C        ; Cy != 0, 150 > intensity
        GOTO    EXEC_CMD        ; Execute the command > 75%
        MOVF    RESULT,W        ; Get the intensity
        SUBLW   D'132'          ; Now compare it to 132 (66%)
        BTFSC   STATUS,C        ; Cy = 0 132 < intensity
        GOTO    MAIN            ; if its > 132 then don't change it
        MOVLW   OFF_RELAY       ; Turn off the relay
        MOVWF   PORTA           ; Out it goes.
        GOTO    MAIN            ; Back to work. (don't drop through)
        ;
        ; Execute the command in the buffer, set delay if necessary
        ;
EXEC_CMD:        
        MOVF    CMD_TMP,W       ; Get relay command
        MOVWF   PORTA           ; and send it out
        ;
        ; Set the timer to hold this commadn for CMD_TIMEOUT mS
        ;
        BTFSC   PORTA, W1       ; Are we running in delayed mode?
        GOTO    MAIN            ; Nope so ignore this part...
        ;
        ; Else set a timeout for activation
        ;
        INIT16  RELAY_TO, CMD_TIMEOUT
        BCF     BOOLS, TIMER    ; Clear this flag
        BSF     BOOLS, ACTIVE   ; Notify loop we're active.
        GOTO    MAIN            ; Loop...
        
        include "divider.asm"
        END                      
