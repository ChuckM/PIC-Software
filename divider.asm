; DIVIDER.ASM	Fixed Point Division
; vim: set syntax=pic :
; Written by 	Chuck McManis (http://www.mcmanis.com/chuck)
; This version	28-NOV-02
; Copyright (c) 2002 Charles McManis, All Rights Reserved.
; RCS Version
; $Id: divider.asm,v 1.0 2003-12-16 21:33:52-08 cmcmanis Exp cmcmanis $
;
; Description:
;     	This file contains the code for some specific fixed point
;     	division functions. They divide a 16 bit value with a
;       2.6 bit fixed point divisor. They are used to compute
;       a scaling factor between 0 - 500 uSeconds to 0 - 200 
;     	"intensity" units.
;
;   NOTE: It depends on the 16bits.inc code also in the repo to provide
;         16 bit operations such as LSL16, ADD16 etc.
;
        NOLIST
        CBLOCK
            ;
            ; Variables used by this routine.
            ;
            RESULT:2
            DIVISOR:2           ; Used in the interrupt
            DIVIDEND:2          ; Routine when calibrating
            DIV_COUNT
        ENDC
;
; FP_DIVIDE
;
; This does a special "fixed point" divide. It takes the
; value in DIVIDEND, shifts it six decimal places to the left
; and then divides by divisor. Thus the result is a floating point
; number with six significant fraction bits.
;
FP_DIVIDE:
        MOVLW   6               ; Shift by six bits
        MOVWF   DIV_COUNT       ; Store in count
FPD_0:  LSL16   DIVIDEND
        DECFSZ  DIV_COUNT,F
        GOTO    FPD_0
        ;
        ; ... now fall through to the 16 bit divide function
        ;                

;
; 16-bit x 16-bit unsigned divide. 
;
;       Divide two 16 bit numbers returning a 16 bit result
;       and a 16 bit remainder.
;
; DIVISOR (top part) - 16 bit variable
; DIVIDEND (bottom part) - 16 bit variable
; 
; Result is the a 16 bit result, the low byte is generally the
; interesting one.
;
; Don't divide by zero, its bad.
;
; Requires 7 bytes of RAM
;       RESULT:2
;       DIVISOR:2
;       DIVIDEND:2
;       DIV_COUNT
; DIV_COUNT is temporary for us...
; 
DIV16X16:
        CLR16   RESULT          ; Clear the result
        MOVF    DIVISOR,W       ; Check for zero
        IORWF   DIVISOR+1,W     ; 
        BTFSC   STATUS,Z        ; Check for zero
        RETLW   H'FF'           ; return 0xFF if illegal
        MOVLW   1               ; Start count at 1
        MOVWF   DIV_COUNT       ; Clear Count
D1:     BTFSC   DIVISOR+1,7     ; High bit set ?
        GOTO    D2              ; Yes then continue
        INCF    DIV_COUNT,F     ; Increment count
        LSL16   DIVISOR         ; Shift it left
        GOTO    D1
D2:     LSL16   RESULT          ; Shift result left
        SUB16   DIVIDEND, DIVISOR ; Reduce Divisor
        BTFSC   STATUS, C       ; Did it reduce?        
        GOTO    D3              ; No, so it was less than
        ADD16   DIVIDEND, DIVISOR ; Reverse subtraction
        GOTO    D4              ; Continue the process
D3:     BSF     RESULT,0        ; Yes it did, this gets a 1 bit
D4:     DECF    DIV_COUNT,F     ; Decrement N_COUNT
        BTFSC   STATUS,Z        ; If its not zero then continue
        GOTO    D5              ; Now we round the last bit.        
        LSR16   DIVISOR         ; Adjust divisor
        GOTO    D2              ; Next bit.
D5:     LSL16   DIVIDEND        ; Shift the dividend left by 1
        SUB16   DIVIDEND, DIVISOR ; See if that makes it larger than divisor
        BTFSS   STATUS,C         ;
        RETURN
        INC16   RESULT          ; Round up.        
        RETLW   0               ; Else return
        LIST
