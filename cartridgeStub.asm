; MIT License

; Copyright (c) 2017 James Lean

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

; define where in the VIC memory the cartridge will load
cartridgeBaseAddress    equ $a000

    processor 6502

    IFCONST CRT
        IFNCONST atLastOneCompilationPassComplete
            echo "Compiling .crt file"
        ENDIF
        org cartridgeBaseAddress - 2;
        dc.b    <cartridgeBaseAddress
        dc.b    >cartridgeBaseAddress
    ELSE
        IFNCONST atLastOneCompilationPassComplete
            echo "Compiling .bin cartridge image"
        ENDIF
	    org	cartridgeBaseAddress
    ENDIF

; constants
destinationAddress  equ     $1200
executionAddress    equ     $1204
CHROUT              equ     $ffd2 ; ROM routine

; zero page variables
readAddressLow      equ     253
readAddressHigh     equ     254
writeAddressLow     equ     204
writeAddressHigh    equ     205
cursor              equ     253
temp                equ     204

	; Cartridge header
	dc.b 	<initialisation
	dc.b    >initialisation
	
	dc.b    0
	dc.b    0
	
	dc.b    $41,$30,$C3,$C2,$CD	    ; labels this as a valid cartridge image

initialisation
    sei
	cld
	ldx	#$ff
	txs
	
    ; kernal setup
    jsr	$fd8d
    jsr	$fd52
    jsr	$fdf9
    jsr	$e518
    
    jsr displayCopyProtection
    jsr displayIntroMessage
    
copyGame subroutine
    lda #<gameCode
    sta readAddressLow
    lda #>gameCode
    sta readAddressHigh
    
    lda #<destinationAddress
    sta writeAddressLow
    lda #>destinationAddress
    sta writeAddressHigh
    
    ldy #0
.loop
    inc $900f
    lda readAddressLow
    cmp #<endOfGame
    bne .notEnd
    lda readAddressHigh
    cmp #>endOfGame
    beq .endReached    
.notEnd
    lda (readAddressLow),y
    sta (writeAddressLow),y
    
    inc writeAddressLow
    bne .writeIncremented
    inc writeAddressHigh  

.writeIncremented
    inc readAddressLow
    bne .readIncremented
    inc readAddressHigh

.readIncremented
    jmp .loop
        
.endReached
    ; data copied. run it
    jmp executionAddress

copyProtectionMessage
    dc.b    147, 13
    dc.b    "ALIGN THE PLASTIC",13
    dc.b    "PRISM WITH THE CODE",13
    dc.b    "CARD AT POSITION ",0
copyProtectionMessage2
    dc.b    ".",13
    dc.b    13
    dc.b    "ENTER THE THREE DIGITS"
    dc.b    "YOU CAN SEE.",13
    dc.b    13,0
    
resultMessage
    dc.b    13,13
    dc.b    "CLOSE ENOUGH",0
    
displayCopyProtection
    lda #<copyProtectionMessage
    sta cursor
    lda #>copyProtectionMessage
    sta cursor + 1
    jsr printLine
    
    lda #"D"
    jsr CHROUT
    lda #"3"
    jsr CHROUT
    
    lda #<copyProtectionMessage2
    sta cursor
    lda #>copyProtectionMessage2
    sta cursor + 1
    jsr printLine
    
    jsr waitForAnyKey
    lda #"*"
    jsr CHROUT
    
    jsr waitForAnyKey
    lda #"*"
    jsr CHROUT
    
    jsr waitForAnyKey
    lda #"*"
    jsr CHROUT
    
    jsr delay
    lda #<resultMessage
    sta cursor
    lda #>resultMessage
    sta cursor + 1
    jsr printLine
    jsr delay    
    rts

delay subroutine
    lda #2
    ldx #255
    ldy #0
.loop
    dey
    bne .loop
    dex
    cpx #0
    bne .loop
    sec
    sbc #1
    cmp #0
    bne .loop
    rts
    
introMessage
    dc.b    147, 13
    dc.b    5, "  SPACESHIP MINUS ONE",13
    dc.b    "  --------- ----- ---",13
    dc.b    13
    dc.b    31, "   YOU ARE THE PROUD",13
    dc.b    "   OWNER OF THE MOST",13
    dc.b    "ADVANCED AND  YET MOST",13
    dc.b    "  MINIMAL SPACE CRAFT",13
    dc.b    "   EVER CONSTRUCTED.",13
    dc.b    13
    dc.b    "  THERE IS ONLY ONE",13
    dc.b    "   CONTROL: THRUST.",13
    dc.b    13
    dc.b    "IT IS TIME TO TAKE IT",13
    dc.b    "    OUT FOR A SPIN.",13,0
introMessage2
    dc.b    13
    dc.b    "  BE CAREFUL NOT TO",13
    dc.b    "SCRATCH THE PAINTWORK!",13
    dc.b    13
    dc.b    5, "      PRESS SPACE",0
        
displayIntroMessage
    lda #59
    sta $900f

    lda #<introMessage
    sta cursor
    lda #>introMessage
    sta cursor + 1
    jsr printLine
    
    lda #<introMessage2
    sta cursor
    lda #>introMessage2
    sta cursor + 1
    jsr printLine
    
    jsr waitForStartKey
    rts
    
printLine subroutine
    ldy #0
.loop
    lda (cursor),y
    cmp #0
    beq .done
    jsr CHROUT
    iny
    cpy #0
    bne .loop
    inc cursor
    bne .loop
    inc cursor + 1
    jmp .loop
.done 
    rts

getKeyState     subroutine
    lda #255    ; set off with no key press recorded
    sta temp
    ldx #0
    ldy #30
.loop
    lda #0
    sta $9120 ; reset keyboard state
    lda $9121 ; get current value
    and temp
    sta temp
    inx
    bne .loop
    dey
    bne .loop
    lda temp
    rts
    
waitForAnyKey   subroutine
    ; first wait for no key to be pressed, then wait for something to be pressed
    jsr getKeyState                                
    cmp #255 ; no key pressed
    beq .waitForKeyDown
    jmp waitForAnyKey
.waitForKeyDown
    lda #0
    sta $9120 ; reset keyboard state
    lda $9121
                                
    cmp #255 ; no key pressed
    beq .waitForKeyDown
.done
    rts
    
waitForStartKey subroutine
    jsr getKeyState                                
    cmp #254 ; space key
    beq .done
    jmp waitForStartKey
.done
    rts
    
gameCode
    incbin temp/game.bin
endOfGame
    dc.b    0 ; end
    
    ; pad to 8K
    org cartridgeBaseAddress + 8192 - 1
    dc.b    0 ; final byte
    
atLastOneCompilationPassComplete    SET 1
