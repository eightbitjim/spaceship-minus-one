    processor 6502
	ORG	$a000

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