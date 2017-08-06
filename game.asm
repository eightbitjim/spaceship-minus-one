				processor 6502
				org $1300 ; 1400 originally. should be free
				
start   		subroutine
				jsr onceOnlyInit
				jsr init
restart
				jsr startScreen
				jsr init
				jmp scrollNow
				
welcome			dc.b	147,18,31," FUEL 128       ",144,"000000",146,0
startMessage	dc.b	147,17,17,17,17,17,17,18, 5,29, 29, " VICCY SPACESHIP! ", 13
				dc.b	17,17,17,17, 159, 18, " PRESS SPACE TO START ",0

welcometerminator 	dc 0

directionUp			equ		$ff
directionDown		equ		$01
rasterline			equ 	$9004
borderPaper			equ		$900f
screenMemoryPage	equ		648 ; screen memory page for operating system
CHROUT				equ 	$ffd2 ; ROM routine
screenstart 		equ		$1000 ; $1e00 for unexpanded VIC, $1000 for expanded
screenstarthigh		equ 	$10 ; $1e for unexpanded VIC, $10 for expanded
colorstarthigh		equ		$94 ; $94 for expanded VIC, $96 for unexpanded VIC
screenwidth			equ		22
screenheight		equ		23
fuelIncreaseAmount 	equ 	10
shipimpulse			equ		80
gravity				equ		3	
keypress			equ		197 ; zero page location
keyspace			equ		32
nokey				equ		64
charDefinitionPointer	equ 36869								

; zero page variables
shipy			equ		254
shipMinorY		equ 	253
cursor 			equ		251 ; also 252
colorcursor		equ		243
shipdy			equ		207
jetSound		equ		205
scrollCounter	equ		204
fuelIncreaseLeft equ 	179
fuel			equ 	178
scoreHi			equ 	177
scoreLo			equ		176
shipx			equ		171
shipDirection	equ		170
fuelSoundCount	equ     169
diff			equ 	166 
charReplaced    equ 	165
character		equ		164 ; put in zero page ; Every 1 and 8 frames during refresh * speed up. Maybe even self modifying code.
charReplaced2	equ		151 
towerTopCharacter	equ 150 
towerMiddleCharacter equ	147
distanceBetweenTowers	equ	146
gapWidth		equ		143
physicsCountdown	equ	142
physicsCountdownInitialValue equ 141
progressCounterLo	equ 140
progressCounterHi	equ 139

; non zero page variables
levelNumber			dc.b 0 ; infrequent
randseed		dc 234, 17 ; Occasionally
flags			dc 0 ; bit 0: decrease fuel if set
fuelActiveFlag	equ #1

.outOfFuel
				jmp restart
;				rts

collision subroutine
				; if end of game, return with zero flag not set
				; what have we collided with?

				cmp #fuelLeftPrintable
				beq .collectFuelLeft
				cmp #fuelRightPrintable
				beq .collectFuelRight
				rts ; zero flag not set, indicates fatal
				
.collectFuelLeft
				lda #32
				sta character
				inc.z cursor
				jsr storechar
				jmp .increaseFuel		
.collectFuelRight
				lda #32
				sta character
				dec.z cursor
				jsr storechar				
				jmp .increaseFuel		
.increaseFuel
				lda #128
				sta fuelSoundCount
				lda #fuelIncreaseAmount
				sta fuelIncreaseLeft
.finishedNotFatal
				lda #0 ; set zero flag to indicate non-fatal
				rts
				
endGame		
				; End of game
				lda #7 ; yellow
				sta explosionColor
				jsr explode
				jsr stopSound
				jmp restart

scrolled subroutine
				lda #8
				sta scrollCounter
.smoothScrollLoop			
				jsr drawship
				lda charReplaced
				cmp #space
				beq .doneCollision1
				jsr collision ; deal with the collision. Returns with zero flag not set if fatal
				bne endGame
.doneCollision1
				lda charReplaced2
				cmp #space
				beq .doneCollision2
				; move cursor up one line so collected object is cleared
				lda #22 ; screen width
				jsr subcursor
				lda charReplaced2
				jsr collision ; deal with the collision. Returns with zero flag set if fatal
				bne endGame
.doneCollision2
				jsr control					
				jsr delay
				jsr updateSound
				jsr rasterdelay
				jsr clearship
				jsr physics				
				jsr smoothScroll
				dec scrollCounter				
				bne .smoothScrollLoop
				
scrollNow		subroutine
				jsr scroll
				jsr drawscreen
				jsr updatePeriodic
				jmp scrolled

smoothScroll	subroutine
				
				; first scroll the double character scollables
				ldx #8 * numberOfScrollableCharacters
.loop
				dex
				clc
				rol rightEdges,x
				rol leftEdges,x
				cpx #0 
				bne .loop
				
				; next scroll the single character scrollables
				ldx #8 * numberOfSingleScrollableChars
.singleLoop
				dex
				lda singleScrollable,x
				asl
				bcc .noWrapAround
				ora #1	; set bottom bit if it fell off the top
.noWrapAround	
				sta singleScrollable,x
				cpx #0
				bne .singleLoop

.finished		
				lda scrollCounter
				cmp #1
				beq .resetScroll

				rts
.resetScroll
				ldx #8 * numberOfScrollableCharacters ; 8 bytes per character
.loop2
				dex
				; copy back from left to right and clear the left
				lda leftEdges,x
				sta	rightEdges,x
				lda #0
				sta leftEdges,x
				cpx #0
				bne	.loop2
				rts
				
updatePeriodic	subroutine ; returns with zero flag set if fuel exhausted
				jsr increaseScoreAndProgress
				
				; if flag is set, decrease or increase fuel
				lda flags
				and #fuelActiveFlag
				beq .return
				
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
.return
				lda #1 ; clear zero flag
				rts

.decreaseFuel
				lda fuel
				cmp #0
				bne .notEmpty
				rts		
.notEmpty
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
					
; Increase score by one and update the onscreen counter
increaseScoreAndProgress	subroutine
				inc scoreLo
				bne .doneIncrease
				inc scoreHi	
.doneIncrease
				; increase the digits on screen
				lda #58  + 128; '9' + 1
				ldx #20 ; position of score digits from the start of screen memory
.increaseDigits
				inc screenstart,x
				cmp screenstart,x
				bne .doneIncreaseDigits
				lda #48 + 128 ; '0'
				sta screenstart,x
				lda #58  + 128; '9' + 1
				dex
				cpx #20 - 5 ; reached the last digit?
				bne .increaseDigits				
.doneIncreaseDigits			
				; now decrease progress counter. If reached zero, move to next level before returning
				ldx progressCounterLo
				dex
				stx progressCounterLo
				cpx #35 ; near end of level?
				bne .notNearEnd
				lda progressCounterHi
				cmp #1
				bne .notNearEnd
				; near end of level. Set gap length between towers
				lda #41 ; more than a screen
				sta distanceBetweenTowers
.notNearEnd
				cpx #0
				bne .doneProgress
				dec progressCounterHi
				bne .doneProgress
				jsr increaseLevel				
.doneProgress
				rts
			
increaseLevel	subroutine
				ldx levelNumber
				inx
				cpx #maxLevel
				bne .doneIncrease
				ldx #0 ; increase level
				; Increase speed (TODO)
.doneIncrease
				stx levelNumber
				jsr setUpLevel
				rts

onceOnlyInit	subroutine
				sei ; don't need maskable interrupts
				
				lda #$7f
			  	;sta $912e     ; disable interrupts
  				;sta $912d
  				
  				sta $911e     ; disable non maskable interrupts
				lda #8
				sta scrollCounter
				rts
				
init			subroutine
				jsr resetScroll;
								
				; make sure the screen memory is in the right place
				lda #22 ; 22 for expanded VIC, 150 for unexpanded
				sta 36866
				
				lda #192+1+2+4+8 ; 192+1+2+4+8 for expanded VIC, 240 normally for unexpanded, 255 for unexpanded with chars at 7168
				sta 36869
				
				lda #$10 ; default screen page. dec 30 for unexpanded vic
				sta screenMemoryPage ; tell the kernel where the screen is. Must match the above.
				
				jsr setUpSound
				
				lda #6		; set ship start position
				sta shipx
				lda #10
				sta shipy
				
				lda #80
				sta $291	; disable case change
								
				; clear screen and display score
				lda #<welcome
				sta.z cursor;
				lda #>welcome
				sta.z cursor + 1
				jsr printline
				
				jsr defineCharacters ;prepare UDGs
				jsr createBottomOfScreen
				
				lda #0
				sta shipdy
				sta towerheight
				sta scoreLo
				sta scoreHi
				sta levelNumber
				
				lda #128 ; start with half full fuel
				sta fuel
				
				lda #8
				sta scrollCounter
				
				lda #4
				sta fuelColumn
				
				lda #16 ; give some space at the start of the level
				sta towercolumnsleft
				
				lda #fuelLeftPrintable
				sta fuelChar
								
				; Set up current level
setUpLevel
				ldx levelNumber
				jsr setupTowerCharacters ; also sets up constants
				ldx levelNumber	
				jsr prepareColors ; prepare color map		
				
				lda physicsCountdownInitialValue
				sta physicsCountdown	
				rts

defaultBackground		equ 	3
lowerBackground			equ 	5
changeNormalColor		equ		255
changeBackgroundColor	equ		254

; Local variables to backgroundMap, and don't need to be in zero page as they are
; only used infrequently
ycoord				dc 		0
xcoord				dc 		0

color0				dc 		0 ; written for a 0 bit in the background map
color1				dc 		0 ; written for a 1 bit in the background map
bgOrfg				dc 		0 ; bg = 0; fg = 1

; background map format is:
; starts in backbround mode (i.e. space)
; [instruction][instruction][instruction], etc
;
; where:
;	instruction = 253: continue to end of screen
; 	instruction	< 253: number of characters in run before inverting (i.e. background to foreground)
;	instruction = 254, x: change background color to following byte
;	instruction = 255, x: change foreground color to following byte

backgroundMap	
; map 0
				dc	255, 4, 254, 0, 22 ; title
				dc	254, 3, 255, 1 ; change colours to clouds
				dc	70,1,20,2,1,2,16,7,7,1,1,2,4,7,6,6,16,6,115
				dc 	255, 6 ; change colour to buildings
				dc	1,3,1,15,1,1,1,3,1,15,3,1,1,1,1,6,1,7,4,1,5,3,2,7,11,2,3,5
				dc 	255, 5 ; change colour to grass
				dc	110
				dc 253 ; end
; map 1
				dc	255, 4, 254, 0, 22 ; title
				dc	254, 1, 255, 2, 22, 22,
				dc	254, 7, 255, 1, 22, 22,
				dc	254, 4, 255, 7, 22, 22,
				dc	254, 2, 255, 1, 22, 22,
				dc	254, 7, 255, 4, 22, 22,
				dc	254, 1, 255, 1, 22, 22,
				dc	254, 4, 255, 2, 22, 22,
				dc	254, 1, 255, 4, 22, 22,
				dc	254, 7, 255, 1, 22, 22,
				dc	254, 2, 255, 7, 22, 22,

				dc 253 ; end
; map 2


; sets up background colours
; start by loading x with the index of the colour map, starting at zero

prepareColors	subroutine
				lda #<backgroundMap	; use cursor to point to backgroundMap position
				sta.z cursor
				lda #>backgroundMap
				sta.z cursor + 1
				ldy #0 ; start at the beginning of the maps
.findLoop
				; seek to the right map
				cpx #0
				beq .foundMap
				jsr .nextInstruction
				cmp #253 ; end of map?
				bne .findLoop
				dex ; found end of current map
				jmp .findLoop				
.foundMap
				lda #colorstarthigh ; use colorcursor to point to screen position in colour map
				sta.z colorcursor + 1
				lda #0
				sta.z colorcursor
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
				inc.z colorcursor
				bne .doneIncColorCursor
				inc.z colorcursor + 1
.doneIncColorCursor
				dex
				jmp .outloop			
.finished:
				rts			
.nextInstruction
				lda (cursor),y
				; cursor+=1
				inc.z cursor
				bne .doneIncCursor
				inc.z cursor + 1	
.doneIncCursor	
				rts
				
createBottomOfScreen	subroutine
				ldx #22
				ldy #22
				lda #bottomBlockPrintable
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
				stx.z cursor + 1
				sty.z cursor
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
				lda.z cursor
				clc
				adc #22
				sta.z cursor
				bcc	.1 			; continue with next line
				inc.z cursor + 1
				jmp .1 			; continue with next line						
.finished
				rts				; done

;;;; Draw a character at the given x and y coordinates
; x coord		x
; y coord		y


drawchar		subroutine
				;; initialise values
				lda	#screenstarthigh
				sta.z cursor + 1
				lda #colorstarthigh
				sta.z colorcursor + 1
				clc
				txa				; x coord to screen low byte

.1				bcs .2			; overflowed page?
.1ret			cpy #$00		; no further y offset?
				beq .3
				adc #screenwidth - 1 ; not sure why I need this -1
				dey
				jmp .1			; 
				
.2				clc	
				inc.z cursor + 1	; next screen page
				inc.z colorcursor + 1 ; and color cursor
				jmp .1ret
				
.3				sta.z cursor		; got final low byte value
				sta.z colorcursor	; and the color cursor
				
storeCharSaveReplaced
				ldx #0
				lda (cursor),x	; get character about to be replaced
				sta	charReplaced
				
storechar		ldx #0			; offset to allow indirect addressing -- should really use zero page
				lda	character	; char to print
				sta (cursor),x				
				rts				

addcursor		subroutine
				clc
				adc.z cursor	
				sta.z cursor
				bcc .1
				inc.z cursor + 1
.1				rts

subcursor		subroutine
				sec
				lda.z cursor
				sbc diff	
				sta.z cursor
				bcs .1
				dec.z cursor + 1
.1				rts

;;;; Draw a tower at the right hand side of the screen
towerheight			dc 	0

fuelColumn			dc 	128 + 4 ; set MSB to indicate will not be drawn. Reset bit when drawing next tower
fuelRow				dc 	10 ; height above the ground or tower
fuelChar			dc 	3
towercolumnsleft	dc	16

; color and colorcharacter already set
; towerMiddleCharacter already set for the non-top/bottom character, and also in a
; towerTopCharacter already set

drawtowerScope  subroutine
.shortTower
				cpy #0
				bne .not0
				lda #spacecharacter
				sta towerMiddleCharacter	
				jmp .draw
.not0
				lda towerTopCharacter
				jmp .draw
drawtower		
				;; If the tower is zero height, draw a space at the bottom and then draw a gap full height
				;; if the screen
				ldy towerheight
				cpy #2 ; One high, so print the top character instead of the middle
				bmi .shortTower
.draw
				sta character

				;; draw first block at bottom right, then build up from there
				ldx #screenwidth - 1
				ldy #screenheight - 2
				jsr drawchar
				
				lda #screenwidth
				sta diff
				
				ldy towerheight
				cpy #0
				beq .drawnBottom
				dey			; subtract one from tower height as bottom character is never drawn
				
.1				beq .drawnBottom ; finished drawing bottom of tower?

				;;;; inlining the subcursor function to save 12 cycles
				;jsr subcursor ; move up a line				
				sec
				lda cursor
				sbc diff
				sta cursor
				bcs .subfinished
				dec cursor + 1
				;;;;; end subcursor inlining			
.subfinished
				;;;; inlining storechar to save 12 cycles
				;jsr storechar				
				ldx #0			; offset to allow indirect addressing -- should really use zero page
				lda	character	; char to print
				sta (cursor),x
.storecharcomplete1
				cpy #2
				beq .switchToTopCharacter
				dey
				jmp .1
.switchToTopCharacter
				lda towerTopCharacter ; use this next
				sta character
				dey
				jmp .1										
.drawnBottom	; now draw the gap
				lda #spacecharacter
				sta character
				ldy gapWidth
				lda towerheight		; if tower height is zero, don't draw the top
				cmp #0
				bne .3
				ldy #screenheight - 2
				
.3				beq .drawnGap
				
				;;;; inlining the subcursor function to save 12 cycles
				;jsr subcursor ; move up a line				
				sec
				lda cursor
				sbc diff
				sta cursor
				bcs .subfinished2
				dec cursor + 1
				;;;;; end subcursor inlining		
.subfinished2
				;;;; inlining storechar to save 12 cycles
				;jsr storechar				
				ldx #0			; offset to allow indirect addressing -- should really use zero page
				lda	character	; char to print
				sta (cursor),x
.storecharcomplete2
				lda towercolumnsleft
				cmp fuelColumn
				bne .305

				; is this the row to print the fuel at?
				cpy fuelRow
				bne .305 ; not yet right height to print fuel
				
				; print fuel
				lda fuelChar; this is variable, as we may be drawing the first or second character
				sta character
				;;;; inlining storechar to save 12 cycles
				;jsr storechar				
				ldx #0			; offset to allow indirect addressing -- should really use zero page
				lda	character	; char to print
				sta (cursor),x
.storecharcomplete3
				lda #spacecharacter ; back to printing spaces
				sta character
				
				; do we need to print the second edge?
				lda fuelChar
				cmp #fuelLeftPrintable
				beq .switchToRight
								
				; switch to printing the left edge again and choose the position for
				; the next fuel character
				lda #fuelLeftPrintable
				sta fuelChar
				lda #6
;;				jsr random ; need to choose 1 to 6
				and #$7 ; random number 0 to 7
				ora #$80 ; set MSB to delay drawing until after next tower
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
				lda #fuelRightPrintable
				sta fuelChar
				dec fuelColumn ; set to print the right hand edge next column
				jmp .305

.drawnGap
				; now draw the the top part
				ldy #1 ; draw 1 top character then switch to middle				
				lda towerTopCharacter
				sta character
.4				lda.z cursor ; reached top line of screen?
				cmp #21
				beq .5				
				;;;; inlining storechar to save 12 cycles
				;jsr storechar				
				ldx #0			; offset to allow indirect addressing -- should really use zero page
				lda	character	; char to print
				sta (cursor),x
.storecharcomplete4
				;;;; inlining the subcursor function to save 12 cycles
				;jsr subcursor ; move up a line				
				sec
				lda cursor
				sbc diff
				sta cursor
				bcs .subfinished3
				dec cursor + 1
				;;;;; end subcursor inlining		
.subfinished3		
				dey
				bne .4
				lda towerMiddleCharacter
				sta character
				jmp .4
.5				
				rts
			
defaulttowerwidth		equ		4

towercharacters			dc.b	3,0,0,2;
towerTopCharacters 		dc.b	0, 32, 0, 32; to be filled in with real tower characters. Must immediately follow towercharacters

spacecharacter			equ		32		
bottomscreencharacter	equ		4
fuelcharacter			equ 	6

;;;; Draw next right hand line of screen
drawline		subroutine
				lda #0
				cmp towercolumnsleft
				bne .drawit
								
				; work out next tower height
				lda towerheight
				cmp #0
				bne .gap
				
				; if we are nearly at the end of the level, draw no more towers, so just
				; have a big gap
				lda progressCounterHi
				cmp #0
				bne .doneCheckEndLevel
				lda progressCounterLo
				cmp #50 ; one screen worth left?
				bmi .doneCheckEndLevel
				lda #0 ; tower height zero, i.e. no tower as at end of level
				jmp .chosenTowerHeight
.doneCheckEndLevel
				jsr random				
				and #$0f
.chosenTowerHeight
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
				lda towerTopCharacters,x ; character to draw at top and bottom edge of towers (around the gap)
				sta towerTopCharacter
				lda towercharacters,x ;
				sta towerMiddleCharacter 
.drawit1		jsr drawtower
				rts
				
.gap			lda distanceBetweenTowers
				sta towercolumnsleft
				lda #0
				sta towerheight
				
				; allow fuel to be drawn from now on
				lda fuelColumn
				and #$7f
				sta fuelColumn
				
				jmp .drawit			

;;;; Random number generator
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
				
delaycount		dc 0 ; doesn't need to be in zero page, as used to cause a delay anyway
delayAmount		dc $30

delay			ldx delayAmount
				stx delaycount
.1				dex
				bne .1
				ldx delaycount
				dex
				stx delaycount
				bne .1
				rts
				
				; wait for raster to enter border
rasterdelay
.rasterloop
				lda rasterline
				cmp #121 ; 124
				bmi .rasterloop
		;		lda #240
		;		sta borderPaper
				rts

drawship		; work out ship offset in pixels from 0 to 7. Take top 3 bits of minor Y value
				lda shipMinorY
				lsr
				lsr
				lsr
				lsr
				lsr
				clc
				
				ldx shipDirection
				cpx #directionUp
				beq .goingUp
				adc #shipTopPrintable
				sta character
				jmp drawshipchar
.goingUp
				sta character
				lda #7
				sec
				sbc character
				clc
				adc #shipTopPrintable
				sta character
				
drawshipchar	ldx shipx
				ldy shipy
				jsr drawchar
				lda charReplaced
				sta charReplaced2 ; store this for collision detection later
				lda character
				cmp #32
				beq .keepSpace
				clc
				adc #8 ; bottom set are 8 bytes further on
				sta character
.keepSpace
				lda #22
				jsr addcursor
				jsr storeCharSaveReplaced
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
				lda fuel	; if fuel is exhausted, no control is possible
				;cmp #0
				bne .notEmpty
				rts
.notEmpty
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
				jsr swapMinorY
.notpress		
				rts
			
physics			subroutine
				; first, is it actually time to update physics
				dec physicsCountdown
				beq .timeToUpdate
				rts
.timeToUpdate
				lda physicsCountdownInitialValue ; reset countdown timer
				sta physicsCountdown
				
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
				jmp swapMinorY

swapMinorY
				; swap the minor y value around
				lda #255
				sec
				sbc shipMinorY
				sta shipMinorY
				rts		
							
;;;; Graphics routines
copyROMCharacters	subroutine
				;;; First copy original character definitions in
				lda #<32768 ; start of ROM character set
				sta.z cursor
				lda #>32768 
				sta.z cursor + 1
				
				lda #<7168 
				sta.z	colorcursor
				lda #>7168
				sta.z colorcursor + 1
				
				ldy #0
				ldx #1
.copyLoop
				lda (cursor),y
				sta (colorcursor),y
				iny
				cpy #0
				bne .copyLoop
				inc.z cursor + 1
				inc.z colorcursor + 1
				dex
				cpx #0 
				bne .copyLoop
				rts
								
defineCharacters	subroutine
			;	jsr copyROMCharacters
				rts
				
soundVolume		equ		36878
voice0			equ		36874
voice1			equ		36875
voice2			equ		36876
voice3			equ		36877

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
				inc fuelSoundCount
				inc fuelSoundCount
				inc fuelSoundCount
				inc fuelSoundCount
.donefuelsound
				rts

; don't need to be in zero page as only used during explosion, which isn't time critical
explodeCountLo		dc 0
explosionSize		dc 0
explosionLeftEdge	dc 0
explosionRightEdge	dc 0
explosionTopEdge	dc 0
explosionBottomEdge	dc 0

explosionX			dc 0
explosionY			dc 0
explosionColor		dc 165

screenWidth			equ 23
screenHeight		equ 24

explode subroutine
				lda #$ff
				sta explodeCountLo	
				lda #$1
				sta explosionSize	
				
				lda shipy ; if ship is at bottom of screen, move up a bit so the explosion is visible
				cpy #21
				bmi .explodeLoop
				lda #21
				sta shipy
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
				lda #screenHeight
.notOffBottomEdge
				sta explosionBottomEdge
				
				; TOP EDGE
				lda shipy
				sec
				sbc explosionSize
				bmi .offTopEdge
				jmp .doneTopEdge
.offTopEdge
				lda #0
.doneTopEdge
				sta explosionTopEdge								
					
				; now draw the explosion box
				lda #32
				sta character

				lda explosionTopEdge
				sta explosionY
				
.lineLoop
				lda explosionLeftEdge
				sta explosionX

				ldx explosionX
				ldy explosionY
				jsr drawchar
				ldy #0
.columnLoop
				jsr storechar
				lda explosionColor
				sta (colorcursor),y
				lda #1
				jsr addcursor
				iny
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

startScreen     subroutine
				lda #6 ; blue
				sta explosionColor
				jsr explode
				lda #1
				sta explosionColor
				jsr explode
				jsr stopSound
				
				; clear screen and display score
				lda #<startMessage
				sta.z cursor;
				lda #>startMessage
				sta.z cursor + 1
				jsr printline
				jsr waitForStartKey
				rts
				
waitForStartKey subroutine
				jsr random
				lda keypress
				cmp #32 ; space
				bne waitForStartKey
				rts

; level format:
; Bytes 0 - 3: front, middle, middle, back edge characters for mid-way through tower
;       4 - 7: front, middle, middle, back edge characters for top and bottom edges of tower
;       8    : horizontal gap between towers
;       9    : vertical gap between towers
;       10   : border and paper colour
;       11-12: number of moves before switching to next level (lo,hi)
;       13   : physics countdown timer initial value
;       14   : delay default value
;		15   : flags

space	equ 32

towerChars1				dc.b	space,towerRightPrintable,towerLeftPrintable,space ,solidRightPrintable,solidPrintable,solidPrintable,solidLeftPrintable ,8,8,0, 50,2, 1,48, 1; front edge, middle block, middle block, back edge, then tower top chars
towerChars2				dc.b	space,space,space,space, space,starRightPrintable,starLeftPrintable,space, 1,21,8, 0,3 ,2, 24, 0  ; black border, black paper
towerChars3				dc.b	3,2,32,32 ,32,32,32,32 ,8,8,0, 0,2, 1, 48, 1; stars
maxLevel				equ 	3

;;;; Set up characters to use when drawing towers. X should have tower set number, 0 being the first
setupTowerCharacters	subroutine				
				lda #<towerChars1
				sta cursor
				lda #>towerChars1
				sta cursor + 1
				lda #16 ; number of positions to skip over to get next set of characters
.xloop
				cpx #0 ; use this character set?
				beq .useThis
				jsr addcursor
				dex
				jmp .xloop
.useThis
				ldy #0
				lda #<towercharacters
				sta colorcursor
				lda #>towercharacters
				sta colorcursor + 1
.yloop
				lda (cursor),y
				sta (colorcursor),y
				iny
				cpy #8
				bne .yloop
.done
				lda (cursor),y
				sta distanceBetweenTowers
				iny
				lda (cursor),y
				sta gapWidth
				iny
				lda (cursor),y
				sta borderPaper
				iny
				lda (cursor),y
				sta progressCounterLo
				iny
				lda (cursor),y
				sta progressCounterHi
				iny
				lda (cursor),y
				sta physicsCountdownInitialValue
				iny
				lda (cursor),y
				sta delayAmount
				iny
				lda (cursor),y
				sta flags
				rts

resetScroll		subroutine
				; reset smooth scrolling back to the start
				lda scrollCounter
.scrollLoop
				beq .scrollDone
				jsr smoothScroll
				dec scrollCounter
				jmp .scrollLoop
.scrollDone
				lda #8
				sta scrollCounter
				rts
				
programEnd
				dc.b 0
				
; now for the graphics
				org 7168
				
startOfChars
scrollable		
				; first the scrollable characters. All the left hand edges, then right hand
				; striped block, scrollable
leftEdges

solidLeftChar	dc.b	0,0,0,0,0,0,0,0
towerLeftChar	dc.b	0,0,0,0,0,0,0,0
fuelLeftChar	dc.b	0,0,0,0,0,0,0,0
starLeftChar	dc.b	0,0,0,0,0,0,0,0

rightEdges

solidRightChar	dc.b	255,255,0,255,255,0,255,255
towerRightChar	dc.b	145,137,197,163,145,137,197,163
fuelRightChar	dc.b	0,0,255,255,255,255,0,0
starRightChar	dc.b	16,16,56,254,56,16,16,16

numberOfScrollableCharacters equ (rightEdges - leftEdges) / 8

fuelLeftPrintable equ (fuelLeftChar - startOfChars) / 8
fuelRightPrintable equ (fuelRightChar - startOfChars) / 8
towerLeftPrintable equ (towerLeftChar - startOfChars) / 8
towerRightPrintable equ (towerRightChar - startOfChars) / 8
solidLeftPrintable equ (solidLeftChar - startOfChars) / 8
solidRightPrintable equ (solidRightChar - startOfChars) / 8
starLeftPrintable equ (starLeftChar - startOfChars) / 8
starRightPrintable equ (starRightChar - startOfChars) / 8

nonscrollable				
				; now the non scrollble characters
				; striped block, not scrollable
solidChar		dc.b	255,255,0,255,255,0,255,255				
solidPrintable equ (solidChar - startOfChars) / 8			

singleScrollable
numberOfSingleScrollableChars	equ (endOfScenery - singleScrollable) / 8

				; now the single character rotational scrolling blocks
				; wavy block at bottom of screen
bottomBlockChar	dc.b	128+64, 32+16, 8+4, 2+1, 2+1, 8+4, 32+16, 128+64
bottomBlockPrintable equ (bottomBlockChar - startOfChars) / 8

endOfScenery

				; now for the pre-computed space ship characters, first the top half, then the bottom half
shipBottomPrintable	equ	(shipBottom - startOfChars / 8)
shipTopPrintable	equ (shipTop - startOfChars) / 8
				
shipTop
				dc.b	128+64,128+64+32+16,128+64+32+16+8+4,255, 255, 128+64+32+16+8+4, 128+64+32+16, 128+64
				dc.b	0, 128+64,128+64+32+16,128+64+32+16+8+4,255, 255, 128+64+32+16+8+4, 128+64+32+16
				dc.b	0, 0, 128+64,128+64+32+16,128+64+32+16+8+4,255, 255, 128+64+32+16+8+4
				dc.b	0, 0, 0, 128+64,128+64+32+16,128+64+32+16+8+4,255, 255
				
				dc.b	0, 0, 0, 0, 128+64,128+64+32+16,128+64+32+16+8+4,255
				dc.b	0, 0, 0, 0, 0, 128+64,128+64+32+16,128+64+32+16+8+4
				dc.b	0, 0, 0, 0, 0, 0, 128+64,128+64+32+16
				dc.b	0, 0, 0, 0, 0, 0, 0, 128+64
shipBottom
				dc.b	0, 0, 0, 0, 0, 0, 0, 0
				dc.b	128+64, 0, 0, 0, 0, 0, 0, 0
				dc.b	128+64+32+16, 128+64, 0, 0, 0, 0, 0, 0
				dc.b	128+64+32+16+8+4, 128+64+32+16, 128+64, 0, 0, 0, 0, 0
				
				dc.b	255, 128+64+32+16+8+4, 128+64+32+16, 128+64, 0, 0, 0, 0
				dc.b	255, 255, 128+64+32+16+8+4, 128+64+32+16, 128+64, 0, 0, 0
				dc.b	128+64+32+16+8+4, 255, 255, 128+64+32+16+8+4, 128+64+32+16, 128+64, 0, 0
				dc.b	128+64+32+16,128+64+32+16+8+4,255, 255, 128+64+32+16+8+4, 128+64+32+16, 128+64, 0
enfOfChars
				dc.b	1,2,4,8,16,32,64,128
dataEnd
				dc.b	0

IFNCONST	printedStatus
				printedStatus equ 1
				echo "To run: SYS ", start
				echo "Total length ", dataEnd - start
				echo "Space left ", 7168 - programEnd
ENDIF


					