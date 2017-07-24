				processor 6502
				org $1400 ; should be free

scrolldemo		subroutine
				jsr init
				jmp .scrollNow
				
welcome		dc.b	147,18,31," FUEL 255       ",144,"000000",146,0
startMessage	dc.b	" PRESS S TO START    ",0
welcometerminator 	dc 0

scrollCounter	dc 		0

rasterline		equ 	$9004
borderPaper		equ		$900f

CHROUT			equ 	$ffd2

screenstart 	equ	$1e00
screenstarthigh	equ $1e
colorstarthigh	equ	$96

cursor 			equ		251
colorcursor		equ		243

screenwidth		equ		22
screenheight	equ		23

shipx			dc		10
shipMinorY		dc 		0
shipy			dc 		10

fuel			dc 		0
fuelIncreaseLeft	dc 	0
fuelIncreaseAmount equ 10

directionUp		equ		$ff
directionDown	equ		$01

shipdy			dc		$00
shipDirection	dc		directionDown

shipimpulse		equ		80
gravity			equ		3	

jetSound		dc		0

keypress		equ		197

keyspace		equ		32
nokey			equ		64

.outOfFuel
				rts

.collectFuelLeft
				lda #32
				sta character
				inc cursor
				jsr storechar
				jmp .increaseFuel		
.collectFuelRight
				lda #32
				sta character
				dec cursor
				jsr storechar				
				jmp .increaseFuel		
.increaseFuel
				lda #255
				sta fuelSoundCount
				
				lda #fuelIncreaseAmount
				sta fuelIncreaseLeft
				jmp .doneCollision
.collision
				; what have we collided with?
				cmp #fuelLeft
				beq .collectFuelLeft
				cmp #fuelRight
				beq .collectFuelRight
				
				; End of game
				jsr explode
				jsr stopSound
				rts
.scrolled
				lda #8
				sta scrollCounter
.1				
				jsr drawship
				lda charReplaced
				cmp #32
				bne .collision
.doneCollision
				jsr control					
				jsr delay
				jsr clearship
				jsr physics
				jsr updateSound
				jsr smoothScroll
				dec scrollCounter
				bne .1
.scrollNow		
				jsr scroll
				jsr drawscreen
				jsr updatePeriodic
				beq .outOfFuel
				jmp .scrolled

smoothScroll	subroutine
				lda scrollCounter
				cmp #1
				beq .resetScroll
				ldx #8
.loop
				dex
				clc
				rol towerRightEdge,x
				rol towerLeftEdge,x
				
				clc
				rol fuelRightEdge,x
				rol fuelLeftEdge,x
				
				lda bottomBlockPosition,x
				asl
				bcc .doneRotate
				ora #1	; set bottom bit if it fell off the top
.doneRotate	
				sta bottomBlockPosition,x

				cpx #0 
				bne .loop
.finished		
				rts
.resetScroll
				ldx #5 * 8	; 5 characters to reset
.loop2
				dex
				lda towerLeftOriginal,x
				sta	towerLeftEdge,x
				cpx #0
				bne	.loop2
				rts
				
updatePeriodic	subroutine ; returns with zero flag set if fuel exhausted
				; draw fuel on screen
				ldx #8 ; digit number 3, plus "SCORE" text
				lda #0
				cmp fuelIncreaseLeft
				beq .decreaseFuel

				; otherwise increase fuel
				lda #255
				cmp fuel
				beq .doneIncreaseAndReadyToReturn		
				inc fuel
				lda #58  + 128; '9' + 1
.increaseFuel
				inc screenstart,x
				cmp screenstart,x
				bne .doneIncrease
				lda #48 + 128 ; '0'
				sta screenstart,x
				lda #58  + 128; '9' + 1
				dex
				cpx #5
				bne .increaseFuel				
.doneIncrease		
.doneIncreaseAndReadyToReturn
				dec fuelIncreaseLeft
				lda #1 ; clear zero flag
				rts

.decreaseFuel
				lda #48  + 128 - 1; '0' - 1
.digitloop
				dec screenstart,x
				cmp screenstart,x
				bne .done
				lda #57 + 128 ; '9'
				sta screenstart,x
				lda #48 + 128 - 1 ; '0'
				dex
				cpx #5
				bne .digitloop				
.done				
				dec fuel
				rts
								
init			subroutine
				jsr setUpSound
				
				lda #10		; set ship start position
				sta shipx
				lda #10
				sta shipy
				
				lda #80
				sta $291	; disable case change
								
				; clear screen and display score
				lda #<welcome
				sta cursor;
				lda #>welcome
				sta cursor + 1
				jsr printline
				
				jsr defineCharacters ;prepare UDGs
				jsr prepareColors ; prepare color map
				jsr createBottomOfScreen
				
				lda #0
				sta shipdy
				sta towerheight
				
				lda #255
				sta fuel
				
				lda #4
				sta fuelColumn
				
				lda #16
				sta towercolumnsleft
				rts

defaultBackground	equ 3
lowerBackground		equ 5

ycoord		dc 0
xcoord		dc 0

color0		dc 0 	; written for a 0 bit in the background map
color1		dc 0	; written for a 1 bit in the background map
bgOrfg		dc 0	; bg = 0; fg = 1

; background map format is:
; starts in backbround mode (i.e. space)
; [instruction][instruction][instruction], etc
;
; where:
;	instruction = 253: continue to end of screen
; 	instruction	< 253: number of characters in run before inverting (i.e. background to foreground)
;	instruction = 254, x: change background color to following byte
;	instruction = 255, x: change foreground color to following byte

changeNormalColor		equ	255
changeBackgroundColor	equ	254

backgroundMap	
				dc	255, 4, 254, 0, 22 ; title
				dc	254, 3, 255, 1 ; change colours to clouds
				dc	70,1,20,2,1,2,16,7,7,1,1,2,4,7,6,6,16,6,115
				dc 	255, 6 ; change colour to buildings
				dc	1,3,1,15,1,1,1,3,1,15,3,1,1,1,1,6,1,7,4,1,5,3,2,7,11,2,3,5
				dc 	255, 5 ; change colour to grass
				dc	110
				
				dc 253 ; end
			
prepareColors	subroutine
				lda #colorstarthigh ; use colorcursor to point to screen position in colour map
				sta colorcursor + 1
				lda #0
				sta colorcursor
				
				lda #<backgroundMap	; use cursor to point to backgroundMap position
				sta cursor
				lda #>backgroundMap
				sta cursor + 1

				ldx #1
				stx bgOrfg
.switchbgfg
				ldx bgOrfg
				cpx #0
				bne .notBg
				ldx #1
				stx bgOrfg
				jmp .readloop
.notBg
				ldx #0
				stx bgOrfg
.readloop
				jsr .nextInstruction
				cmp #255
				bne .not1
				jsr .nextInstruction
				sta	color0					; store as background color
				jmp .readloop							
.not1
				cmp #254
				bne .not2
				jsr .nextInstruction
				sta	color1					; store as foreground color
				jmp .readloop
.not2
				cmp #253						; end marker
				beq .finished

				tax
				lda #0
				cmp bgOrfg
				bne .fg
				lda color0
				jmp .outloop			
.fg
				lda color1
.outloop
				cpx #0
				beq .switchbgfg ; swap background and foreground, and get next instruction
								
				sta (colorcursor),y ; y = 0

				; colorcursor+=1
				inc colorcursor
				bne .doneIncColorCursor
				inc colorcursor + 1
.doneIncColorCursor
				dex
				jmp .outloop			
.finished:
				rts			
.nextInstruction
				lda (cursor),y
				; cursor+=1
				inc cursor
				bne .doneIncCursor
				inc cursor + 1	
.doneIncCursor	
				rts
				
createBottomOfScreen	subroutine
				ldx #22
				ldy #22
				lda #bottomscreencharacter
				sta character
				jsr drawchar ; draw the first one
				ldy #22 ; number left to go
				lda #1 ; number of spaces to subtract
				sta diff
.loop
				dey
				jsr subcursor
				jsr storechar
				cpy #0
				bne .loop
				rts
				
printline		subroutine
				ldy #0
.loop
				lda (cursor),y
				cmp #0
				beq .done
				jsr CHROUT
				iny
				jmp .loop
.done 
				rts

scroll			subroutine
				clc
				ldx #screenstarthigh	; put start of screen hi in cursor position
				ldy #22 				; screen scroll start lo
				stx cursor + 1
				sty cursor
				ldy #0			; current *X* position
				ldx #21			; number of lines left to scroll
.1				
				iny 			; (2) cycles. Gain 4
				lda (cursor),y 	; (5+) get existing character
				dey				; (2)
				sta (cursor),y	; (6) store in previous location
				iny 			; (2) 
				cpy #21
				bne .1 			; (2) continue within line
				ldy #0 			; (?) reset back to start of line counter
				dex 			; (2) next line
				beq .finished
				lda cursor
				clc
				adc #22
				sta cursor
				bcc	.1 			; continue with next line
				inc cursor + 1
				jmp .1 			; continue with next line						
.finished
				rts				; done

;;;; Draw a character at the given x and y coordinates
; x coord		x
; y coord		y
character		dc 0
charReplaced	dc 0

drawchar		subroutine
				;; initialise values
				lda	#screenstarthigh
				sta cursor + 1
				lda #colorstarthigh
				sta colorcursor + 1
				clc
				txa				; x coord to screen low byte

.1				bcs .2			; overflowed page?
.1ret			cpy #$00		; no further y offset?
				beq .3
				adc #screenwidth - 1 ; not sure why I need this -1
				dey
				jmp .1			; 
				
.2				clc	
				inc cursor + 1	; next screen page
				inc colorcursor + 1 ; and color cursor
				jmp .1ret
				
.3				sta cursor		; got final low byte value
				sta colorcursor	; and the color cursor
				
				ldx #0
				lda (cursor),x	; get character about to be replaced
				sta	charReplaced
				
storechar		ldx #0			; offset to allow indirect addressing -- should really use zero page
				lda	character	; char to print
				sta (cursor),x				
				rts				

addcursor		subroutine
				clc
				adc cursor	
				sta cursor
				bcc .1
				inc cursor + 1
.1				rts

diff			dc 0
subcursor		subroutine
				sec
				lda cursor
				sbc diff	
				sta cursor
				bcs .1
				dec cursor + 1
.1				rts

;;;; Draw a tower at the right hand side of the screen
towerheight		dc 0
gapwidth		dc 8
fuelColumn		dc 128 + 4 ; set MSB to indicate will not be drawn. Reset bit when drawing next tower
fuelRow			dc 10 ; height above the ground or tower
fuelChar		dc 3

; color and colorcharacter already set
; a is character to draw
tempVar			dc.b	0

drawtower		subroutine
				;; If the tower is zero height, draw a space at the bottom and then draw a gap full height
				;; if the screen
				ldy towerheight
				cpy #0
				bne .draw
				lda #spacecharacter
.draw
				sta character
				sta tempVar

				;; draw first block at bottom right, then build up from there
				ldx #screenwidth - 1
				ldy #screenheight - 2
				jsr drawchar
				
				lda #screenwidth
				sta diff
				ldy towerheight
				cpy #0
				beq .2
				dey			; subtract one from tower height as bottom character is never drawn
.1				beq .2				
				jsr subcursor
				jsr storechar				
				dey
				jmp .1
								
.2				; now draw the gap
				lda #spacecharacter
				sta character
				ldy gapwidth
				lda towerheight		; if tower height is zero, don't draw the top
				cmp #0
				bne .3
				ldy #screenheight - 2
				
.3				beq .31
				jsr subcursor
				jsr storechar
				lda towercolumnsleft
				cmp fuelColumn
				bne .305

				; is this the row to print the fuel at?
				cpy fuelRow
				bne .305 ; not yet right height to print fuel
				
				; print fuel
				lda fuelChar; this is variable, as we may be drawing the first or second character
				sta character
				jsr storechar
				lda #spacecharacter ; back to printing spaces
				sta character
				
				; do we need to print the second edge?
				lda fuelChar
				cmp #fuelLeft
				beq .switchToRight
								
				; switch to printing the left edge again and choose the position for
				; the next fuel character
				lda #fuelLeft
				sta fuelChar
				lda #6
;;				jsr random ; need to choose 1 to 6
				and #$7 ; random number 0 to 7
				ora #$80 ; net MSB to delay drawing until after next tower
				cmp #$87
				bne .not7
				lda #3 ; replace a 0 with a 3 to put the fuel between towers not just before a tower
.not7
				sta fuelColumn
				
				; now the row
				jsr random 
				and #$0f
				clc
				adc #$3
				sta fuelRow
.305
				dey
				jmp .3
.switchToRight
				lda #fuelRight
				sta fuelChar
				dec fuelColumn ; set to print the right hand edge next column
				jmp .305
				
				; now the top bit
.31				lda tempVar
				sta character
.4				lda cursor ; reached top line of screen?
				cmp #21
				beq .5
				
				jsr storechar
				jsr subcursor			
				jmp .4
.5				
				rts
			
towercolumnsleft		dc	16
defaulttowerwidth		equ	4
towercharacters			dc.b	3,0,0,2 ; front edge, middle block, middle block, back edge
spacecharacter			equ		32		
bottomscreencharacter	equ	4
fuelcharacter			equ 6

;;;; Draw next right hand line of screen
drawline		subroutine
				lda #0
				cmp towercolumnsleft
				bne .drawit
								
				; work out next tower height
				lda towerheight
				cmp #0
				bne .gap
				jsr random
				and #$0f
				sta towerheight
				cmp #$0f
				bne .notdec
				dec towerheight
.notdec 
				lda #defaulttowerwidth
				sta towercolumnsleft
.drawit	
				dec towercolumnsleft
				lda towerheight
				cmp #0
				beq .drawit1
				ldx	towercolumnsleft
				lda towercharacters,x
.drawit1		jsr drawtower
				
				rts
.gap			lda #8
				sta towercolumnsleft
				lda #0
				sta towerheight
				
				; allow fuel to be drawn from now on
				lda fuelColumn
				and #$7f
				sta fuelColumn
				
				jmp .drawit			

;;;; Random number generator
randseed		dc 234, 17

random			subroutine
				lda randseed
				ror
				ror
				ror
				eor randseed
				sbc randseed + 1
				rol
				rol
				rol
				sta randseed + 1
				rol
				eor randseed
				adc randseed + 1
				inc randseed
				sta randseed
				rts
				
delaycount		dc 0	
delay			ldx #$30
				stx delaycount
				lda #0
				sta borderPaper
.1				dex
				bne .1
				ldx delaycount
				dex
				stx delaycount
				bne .1
				
				; wait for raster to enter border
.rasterloop
				lda rasterline
				cmp #124 ; 180
				bmi .rasterloop
		;		lda #240
		;		sta borderPaper
				rts

shipchar		dc 0

drawship		lda #1		; ship picture
				sta character
drawshipchar	ldx shipx
				ldy shipy
				;lda #3
				;sta color
				jsr drawchar
				rts
				
clearship		lda #32		; space
				sta character
				jmp drawshipchar
				
drawscreen		subroutine
				jsr drawline
				rts
				
; carry flag set if space just pressed, clear otherwise
lastkey			dc	0
	
control			subroutine
				lda keypress
				ldx lastkey
				sta lastkey
				cpx #keyspace
				beq .notpress 
				cmp #keyspace
				bne .notpress
				; space just pressed
				; apply an impulse
				lda #255
				sta jetSound
				lda #shipimpulse
				sta shipdy
				lda #directionUp
				sta shipDirection	
.notpress
				
				rts
			
physics			subroutine
				; update ship position. First the minor position
				lda shipMinorY
				clc
				adc shipdy
				sta shipMinorY
				bcc .doneShipPosition

				; need to update major position, depending on the direction
				lda shipy
				clc
				adc shipDirection
				sta shipy
.doneShipPosition			
	
				; update ship velocity
				; are we currently going up or down?
				lda shipDirection
				cmp #directionUp
				beq .goingUp
.goingDown
				lda shipdy
				cmp #255 - #gravity
				bmi .maxVelocity
				
				; increase velocity
				clc
				adc #gravity
				sta shipdy
.maxVelocity
				rts
.goingUp
				lda jetSound
				cmp #0
				beq .notjet
				dec jetSound	
.notjet
				lda shipdy
				; decrease velocity
				sec				
				sbc #gravity
				bcc .switchToDown
				sta shipdy
				rts					
.switchToDown
				lda #0
				sta shipdy
				lda #directionDown
				sta shipDirection
				rts		
				
							
;;;; Graphics routines
startOfChars	equ	7168
towerLeftEdge	equ startOfChars + 16
towerRightEdge	equ startOfChars + 24
bottomBlockPosition	equ startOfChars + 32
fuelLeftEdge	equ startOfChars + 5 * 8
fuelRightEdge	equ startOfChars + 6 * 8
filledChar		equ 0

fuelLeft		equ 5
fuelRight		equ 6

charDefinitionPointer	equ 36869

chars			dc.b	255,255,255,255,255,255,255,255
charShip		dc.b	128+64,128+64+32+16,128+64+32+16+8+4,255, 255, 128+64+32+16+8+4, 128+64+32+16, 128+64
charEmpty		dc.b	0,0,0,0,0,0,0,0
charBlock		dc.b	255,255,255,255,255,255,255,255
bottomBlock		dc.b	128+64, 32+16, 8+4, 2+1, 2+1, 8+4, 32+16, 128+64
fuel1			dc.b	0,0,0,0,0,0,0,0
fuel2			dc.b	0,0,255,255,255,255,0,0

towerLeftOriginal	equ chars + 16
towerRightOriginal	equ chars + 24
				
numBytes		equ		56 ; 7 * 8

defineCharacters	subroutine
				;;; Prepare character definitions
				lda #<chars
				sta cursor
				lda #>chars
				sta cursor + 1
				
				lda #<startOfChars
				sta	colorcursor
				lda #>startOfChars
				sta colorcursor + 1
				
				ldy #numBytes

.copyLoop		dey
				lda (cursor),y
				sta	(colorcursor),y
				cpy #0
				bne .copyLoop
						
				;;; Switch character definitions to RAM
				lda #$ff
				sta $9005
				rts
				
soundVolume		equ		36878
voice0			equ		36874
voice1			equ		36875
voice2			equ		36876
voice3			equ		36877

fuelSoundCount	dc 0

setUpSound
				lda #15
				sta soundVolume	; volume = 15
				lda #0
				sta voice0	; voice 0
				sta voice1	; voice 1
				sta voice2	; voice 2
				sta voice3	; voice 3 : noise
				rts
stopSound
				lda #0
				sta voice0
				sta voice1
				sta voice2
				sta voice3
				sta soundVolume
				rts
				
updateSound subroutine
				lda jetSound
				sta voice3
				
				lda fuelSoundCount
				sta voice2
				rol
				sta voice0
				lda fuelSoundCount
				cmp #0
				beq .donefuelsound
				dec fuelSoundCount
.donefuelsound
				rts

explodeCountLo	dc 0
explosionSize	dc 0
explosionLeftEdge	dc 0
explosionRightEdge	dc 0
explosionTopEdge	dc 0
explosionBottomEdge	dc 0

explosionX		dc 0
explosionY		dc 0
explosionColor	equ 17

screenWidth		equ 23
screenHeight	equ 24

explode subroutine
				lda #$ff
				sta explodeCountLo	
				lda #$1
				sta explosionSize	
.explodeLoop
				; explode count
				lda explodeCountLo
				sta voice3
				sta voice2
				sta voice1
				sta voice0
		
				; draw explosion effect
				; work out left edge and right edge of current frame

				; RIGHT EDGE
				lda shipx
				clc
				adc explosionSize
				cmp #screenWidth
				bmi .notOffRightEdge
				lda #screenWidth - 1
.notOffRightEdge
				sta explosionRightEdge
				
				; LEFT EDGE
				lda shipx
				sec
				sbc explosionSize
				bmi .offLeftEdge
				jmp .doneLeftEdge
.offLeftEdge
				lda #0
.doneLeftEdge
				sta explosionLeftEdge
				
				; BOTTOM EDGE
				lda shipy
				clc
				adc explosionSize
				cmp #screenHeight
				bmi .notOffBottomEdge
				lda #screenHeight - 1
.notOffBottomEdge
				sta explosionBottomEdge
				
				; TOP EDGE
				lda shipy
				sec
				sbc explosionSize
				sbc #2
				bmi .offTopEdge
				jmp .doneTopEdge
.offTopEdge
				lda #1
.doneTopEdge
				sta explosionTopEdge								
					
				; now draw the explosion box
				lda #filledChar
				sta character

				lda explosionTopEdge
				sta explosionY
				
.lineLoop
				lda explosionLeftEdge
				sta explosionX

				ldx explosionX
				ldy explosionY
				jsr drawchar
				ldx #0
.columnLoop
				jsr storechar
				lda #explosionColor
				sta (colorcursor),x
				lda #1
				jsr addcursor
				inx				
				inc explosionX
				lda explosionX
				cmp explosionRightEdge
				bne .columnLoop
				
				inc explosionY
				lda explosionY
				cmp explosionBottomEdge
				bne .lineLoop
		
				; increment counters
				jsr delay

				dec explodeCountLo
				dec explodeCountLo
				dec explodeCountLo
				dec explodeCountLo
				inc explosionSize
				lda explosionSize
				cmp #23 ; max explosion size
				beq .done
				jmp .explodeLoop			
.done			
				; finished
				rts
				