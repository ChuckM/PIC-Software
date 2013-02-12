; BUTTON.ASM
; vi: set syntax=pic :
; Copyright (C) 2002, Charles McManis All Rights Reserved
; $Id: button.asm,v 1.0 2003-12-16 21:33:52-08 cmcmanis Exp cmcmanis $
;
; This code implements a button state machine, it should be called at 1mS
; intervals.
;
; The code is not standalone, it requires the capture variables be defined
; and the division variables (RESULT, DIVIDEND, etc) I just put it here to 
; keep changes to it tracked separately from changes to other parts of the
; code.
;
        NOLIST        
        PAGE
        CBLOCK                    ;
            ; Variables used in the Button State machine
            ;
            B_TIME:2            ; Keep track of time button is pressed
            B_STATE             ; Current state (0, 1, or 2)
            BUTT_COUNT:2        ; Blink counter (could be a byte)
            B_TMP:2
        ENDC

; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;               The Button State Machine
;
; The button state machine implements the user interface. It uses
; a single button that when pressed and held goes into calibration
; mode. During calibration mode the user is expected to wiggle their
; R/C stick to its minimum and maximum extension and the we will
; record the minimum and maximum pulse widths that are seen. Once
; the user is done they press the button again with the R/C lever
; in the "neutral" position and the neutral pulse width is recorded.
; During calibration, the outputs are disabled and the Calibrate
; LED blinks at approximately 4 Hz.
;
; Once calibrated, new values are stored in EEPROM for future use.
;
; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

;
; Equates and Macros
;                    
; UI_TIME - press and hold time to get into UI mode
; BLINK_TIME rate of LED blink (4 hz in this case)
UI_TIME         EQU     D'750'
BLINK_TIME      EQU     D'250'
        
;
; The state machine dispatcher. 
;            
BUTT_STATE:
        MOVLW   high BUTT_STATE
        MOVWF   PCLATH
        MOVF    B_STATE,W       ; Get button state
        ADDWF   PCL,F           ; Indirect jump
        GOTO    STATE_0         ; IDLE State
        GOTO    STATE_1         ; SAMPLE/MEASURE State
        GOTO    STATE_2         ; MONITOR State
           
;
; The Idle State. 
;       In this state we are waiting for the button press. When
; it occurs we reset the UI mode flag and pop into state 1 by
; returning. This insures that 1mS will pass before we look at
; the button again (this is implicit debounce).
; 
; It it isn't pressed we just return.
;
STATE_0:
        PRESSED IDLE_1          ; When pressed measure time
        RETURN                  ; Nope so we're fine.
IDLE_0:
        CLRF    B_STATE         ; This entry point used to get back        
        BCF     PORTB, CAL_LED  ; To IDLE state and to initialize the
        INIT16  B_TIME, UI_TIME ; State machine.
        INIT16  BUTT_COUNT, BLINK_TIME
        BCF     BOOLS, UI_MODE  ; Clear flag
        RETURN                  ; Back to whomever called us.
IDLE_1:
        MOVLW   D'1'            ; Switch to state 1 (mS debounce)
        MOVWF   B_STATE         ; Store it
        RETURN
        
;
; The Measuring State
;       Now someone has pushed the button so we start measuring
; how long they hold it down. When they let it up we will either
; go into the UI state or back to idle.
;
STATE_1:        
        NOT_PRESSED     MEASURE_1
        BSF     PORTB, CAL_LED  ; Turn on CAL (ack the button press)
        BTFSC   BOOLS, UI_MODE  ; Check to see if we're in UI mode
        RETURN                  ; Ok waiting for release
        DEC16   B_TIME          ; Else count down the mS
        SKPNZ                   ; If not zero we're not done
        BSF     BOOLS, UI_MODE  ; If it is zero we're good to go
        RETURN                  ; Wait for next clock tick
MEASURE_1:
        BTFSS   BOOLS, UI_MODE  ; Was it held long enough?
        GOTO    IDLE_0          ; Nope then reset to idle_0
MEASURE_2:
        MOVLW   D'2'            ; Yup, so lets capture data
        MOVWF   B_STATE         ; Set new state, initialize capture
        INIT16  CAP_MIN, D'1522'  ; variables to median values
        INIT16  CAP_MAX, D'1522'  ; that will be over ridden
        RETURN                  ; and return.

;
; The Monitoring State
;       Now we monitor inputs from the capture ISR and
; look for the minimum and maximum values.
;
STATE_2:
        PRESSED FIN             ; Next stop the IDLE state ...
        DEC16   BUTT_COUNT      ; While in the state we blink
        SKPZ                    ; CAL by toggling it ever BLINK mS
        GOTO    MON_0           ;
        INIT16  BUTT_COUNT, BLINK_TIME
        MOVF    PORTB,W         ; Get PORT B contents
        XORLW   CAL_B           ; Toggle CAL
        MOVWF   PORTB           ; Put PORT B contents
;
; wait to see a valid capture
;        
MON_0:  BTFSS   BOOLS, GOT_ONE  ; Wait for it ...
        RETURN                  ; Nope so return to main.
        BCF     BOOLS, GOT_ONE  ; Note that we saw it
        CMP16   CAPTURE, CAP_MIN
        SKPNC                   ; If carry is set its new MIN  
        GOTO    MON_2           ; Else check for MAX
        MOV16   CAPTURE, CAP_MIN ; Store new minimum value
MON_2:  CMP16   CAPTURE, CAP_MAX ; Is it more than CAP_MAX?
        SKPC                    ; If carry set it is MAX
        GOTO    MON_3           ; Else update MID (always)
        MOV16   CAPTURE, CAP_MAX ; Store new maximum value
MON_3:  MOV16   CAPTURE, CAP_MID ; Always store, in the end this will
        RETURN                  ; be correct. Now return.

;
; Now finish up, compute the scaling factor values and store those
;    
FIN:               
        MOV16   CAP_MAX, R_MAX  ; Copy the value
        SUBI16  R_MAX, GUARD_BAND ; Subtract off the guard band
        SUB16   R_MAX, CAP_MID  ; Subtract Neutral
        
        MOV16   CAP_MID, R_MIN  ; Start from the mid point
        SUBI16  R_MIN,GUARD_BAND   ; Subtract off the guard band
        SUB16   R_MIN, CAP_MIN  ; Subtract the minimum
        
        MOV16   R_MAX, DIVIDEND ; Compute MAX/200.0
        MOVLW   D'200'
        MOVWF   DIVISOR
        CLRF    DIVISOR+1
        CALL    FP_DIVIDE
        MOVF    RESULT,W
        MOVWF   SCALE_FWD       ; That's new forward scaling.
        
        MOV16   R_MIN, DIVIDEND ; Next compute MIN/200.0
        MOVLW   D'200'
        MOVWF   DIVISOR
        CLRF    DIVISOR+1
        CALL    FP_DIVIDE
        MOVF    RESULT,W
        MOVWF   SCALE_REV       ; This is new reverse.
        BSF     BOOLS, UPDATE   ; Signal main to save in EEPROM
        GOTO    IDLE_0
        LIST
        
        
