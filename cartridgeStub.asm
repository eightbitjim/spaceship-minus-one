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

; zero page variables
readAddressLow      equ     253
readAddressHigh     equ     254
writeAddressLow     equ     204
writeAddressHigh    equ     205
			
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

gameCode
    incbin temp/game.bin
endOfGame
    dc.b    0 ; end
    
    ; pad to 8K
    org cartridgeBaseAddress + 8192 - 1
    dc.b    0 ; final byte
    
atLastOneCompilationPassComplete    SET 1
