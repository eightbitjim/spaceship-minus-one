				processor 6502
				org $1400 ; should be free

start   		ldx  #$0 ; current character
loop  			jsr changeborder
				inx
				txa
				jsr fillscreen
				tax        
        		jmp loop	; just carry on forever
        
changeborder	inc $900f
				rts

cursor EQU		251

fillscreen		subroutine
				ldy #$1e
				sty cursor + 1
				ldy #$0
				ldx #$0
				sty cursor
.1				sta (cursor),y
				cpy #$fa
				bne .2
				cpx #$1
				beq finished
.2				iny
				bne .1
				inc cursor + 1
				inx
				jmp .1
finished		rts
				