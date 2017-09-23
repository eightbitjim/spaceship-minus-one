				processor 6502
				org $1280 ; 1400 originally. should be free
				
				; keyboard scan routine at eb1e, fills in $cb and f5. Scans $9120 and $9121. quite long
start   		subroutine
				jsr onceOnlyInit
				jsr init
restart
				jsr startScreen
				jsr init
				jsr thrust ; give the ship a short thrust to start off
				jsr drawline
				jsr scrollNow
				jmp smoothScrollLoop
				
welcome			dc.b	147,18,31," FUEL 128       ",144,"000000",146,0
startMessage	dc.b	19,17,17,17,17,17,17,18, 5,29, 29, 29, " SPACE SHIP '83 ", 13
				dc.b	17,17,17,17, 159, 29, 29, 29, 18, " SPACE TO START ",13,0
continueMessage	dc.b	17, 29, 29,  29, 18,             "B  TO START FROM" , 13, 29, 29, 29, 18, "   BEGINNING    ",0

welcometerminator 	dc 0

joystickDDR1		equ		$9113
joystickDDR2		equ		$9122
joystickIn1			equ		$9111
joystickIn2			equ		$9120

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
spacePrintable		equ 	32
charDefinitionPointer	equ 36869	
nextLineLength		equ		22 ; 22 positions in all
							

; zero page variables
shipy			equ		254
shipMinorY		equ 	253
cursor 			equ		251 ; also 252
colorcursor		equ		243
shipdy			equ		207
jetSound		equ		205
scrollCounter	equ		204
lastkey			equ		179
fuel			equ 	178
shipdx			equ		177
shipMinorX		equ		176
;scoreHi			equ 	177
;scoreLo			equ		176
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
topTowerEdgeCharacter	equ 138
topTowerMiddleCharacter	equ	137
shipToBeDrawnAt1	equ 23 ; +1
shipToBeDrawnAt2	equ 25 ; +1

nextLine			equ 	0 ; put at start of zero page

towercharacters		equ 27;	 4 bytes
towerTopCharacters 	equ 31; 4 bytes
topTowerCharacters	equ 35; 4 bytes 
topTowerEdgeCharacters	equ 39; 4 bytes 
;framesSinceLastScroll	equ 43
lastFrameWasScroll	equ 43

; non zero page variables
levelNumber			dc.b 0 ; infrequent
randseed		dc 234, 17 ; Occasionally
flags			dc 0 ; bit 0: decrease fuel if set
delayReduction	dc 0	; amount to reduce delay by. Increased by level wrap-around
minDelayAmount	equ	5
delayReductionPerWraparound	equ 10 ; amount to reduce delay by each time all levels are completed


.outOfFuel
				jmp restart

collision subroutine
				; if end of game, return with zero flag not set
				; what have we collided with?
				cmp #fuelLeftPrintable
				beq .collectFuelLeft
				cmp #fuelRightPrintable
				beq .collectFuelRight
				rts ; zero flag not set, indicates fatal
.collectFuelLeft
				lda #spacePrintable
				ldy #1
				sta (shipToBeDrawnAt1),y
				sta (shipToBeDrawnAt2),y
				jmp .increaseFuelNow	
.collectFuelRight
				lda #spacePrintable
				ldy #0
				dec shipToBeDrawnAt1
				dec shipToBeDrawnAt2
				sta (shipToBeDrawnAt1),y
				sta (shipToBeDrawnAt2),y
				inc shipToBeDrawnAt1
				inc shipToBeDrawnAt2
				jmp .increaseFuelNow	
.increaseFuelNow
				jsr increaseScoreBy100
				lda #128
				sta fuelSoundCount
				lda #fuelIncreaseAmount
				jsr increaseFuel
				jsr increaseFuel
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

smoothScrollLoop			
				lda lastFrameWasScroll
				cmp #0  
				beq .normal
				lda #0
				sta lastFrameWasScroll
				jmp dontdrawship
.normal
				jsr workOutShipPosition
				jsr drawship
dontdrawship
				lda charReplaced
				cmp #spacePrintable
				beq .doneCollision1
				jsr collision ; deal with the collision. Returns with zero flag not set if fatal
				bne endGame
.doneCollision1
				lda charReplaced2
				cmp #spacePrintable
				beq .doneCollision2
				; move cursor up one line so collected object is cleared
				lda #22 ; screen width
				jsr subcursor
				lda charReplaced2
				jsr collision ; deal with the collision. Returns with zero flag set if fatal
				bne endGame
.doneCollision2
				jsr control		
				jsr updateSound
				jsr rasterdelay
				jsr clearship
				jsr physics		

				jmp smoothScrollLoop
				
scrollNow
				jsr workOutShipPosition
				jsr scroll
				jsr updatePeriodic

				lda #8
				sta scrollCounter
				sta lastFrameWasScroll
				rts
prepareLine
				jsr drawline
				jmp donePrepareLine

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
				bne .finishedSmoothScroll
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

.finishedSmoothScroll
				rts
				
handleFullScroll
				lda #5
				cmp scrollCounter
				beq prepareLine
donePrepareLine
				dec scrollCounter				
				beq	scrollNow 
				rts
				
updatePeriodic	subroutine ; returns with zero flag set if fuel exhausted
				jsr increaseScoreAndProgress
				lda #1 ; clear zero flag
				rts
								
increaseFuel	subroutine		
				; draw fuel on screen
				ldx #8 ; digit number 3, plus "SCORE" text

				lda #255
				cmp fuel
				beq .doneIncrease	
				inc fuel
				lda #58  + 128; '9' + 1	
.digitLoop	
				inc screenstart,x
				cmp screenstart,x
				bne .doneIncrease
				lda #48 + 128 ; '0'
				sta screenstart,x
				lda #58  + 128; '9' + 1
				dex
				cpx #5
				bne .digitLoop				
.doneIncrease		
				rts

decreaseFuel 	subroutine
				; draw fuel on screen
				ldx #8 ; digit number 3, plus "SCORE" text
				
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
increaseScore
				; now increase score
				
				; increase the digits on screen
				lda #58  + 128; '9' + 1
				ldx #20 ; position of score digits from the start of screen memory
increaseDigits
				inc screenstart,x
				cmp screenstart,x
				bne .doneIncreaseDigits
				lda #48 + 128 ; '0'
				sta screenstart,x
				lda #58  + 128; '9' + 1
				dex
				cpx #20 - 5 ; reached the last digit?
				bne increaseDigits				
.doneIncreaseDigits			
				rts
			
increaseScoreBy10
				lda #58 + 128
				ldx #19
				jmp increaseDigits

increaseScoreBy100
				lda #58 + 128
				ldx #18
				jmp increaseDigits
				
increaseLevel	subroutine
				jsr powerUp
				ldx levelNumber
				inx
				cpx #maxLevel
				bne .doneIncrease
				ldx #0 ; back to first level, but increase speed
				; Increase speed
				lda delayReduction
				clc
				adc #delayReductionPerWraparound
				sta delayReduction
.doneIncrease
				stx levelNumber
				jsr setUpLevel
				rts
				
onceOnlyInit	subroutine
				sei ; don't need maskable interrupts
				
				lda #$7f
			  	sta $912e     ; disable interrupts
  				sta $912d  				
  				sta $911e     ; disable non maskable interrupts from restore key
  				
  				lda $912e
  				lda $912d
  				
				lda #8
				sta scrollCounter
				sta lastFrameWasScroll
				
				; make sure the screen memory is in the right place
				lda #22 ; 22 for expanded VIC, 150 for unexpanded
				sta 36866
				
				lda #192+1+2+4+8 ; 192+1+2+4+8 for expanded VIC, 240 normally for unexpanded, 255 for unexpanded with chars at 7168
				sta 36869
				
				lda #$10 ; default screen page. dec 30 for unexpanded vic
				sta screenMemoryPage ; tell the kernel where the screen is. Must match the above.
				
				lda #80
				sta $291	; disable case change
				
				jsr defineCharacters ;prepare UDGs
				
				lda #0
				sta levelNumber
				sta joystickDDR1	; prepare for joystick input
				
				lda #10
				sta delayReduction
				
				jsr prepareTowerPositions
				rts

init			subroutine
				jsr resetScroll;
				jsr setUpSound
				
				lda #6		; set ship start position
				sta shipx
				lda #10
				sta shipy
								
				; clear screen and display score
				lda #<welcome
				sta.z cursor;
				lda #>welcome
				sta.z cursor + 1
				jsr printline
				
				jsr createBottomOfScreen
				
				lda #0
				sta shipdy
				sta towerheight
				
				lda #128 ; start with half full fuel
				sta fuel
				
				lda #8
				sta scrollCounter
				sta lastFrameWasScroll
				
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
					
				lda physicsCountdownInitialValue
				sta physicsCountdown	
				
				lda #spacePrintable ; ship hasn't collided with anything yet
				sta charReplaced
				
				lda #0
				sta shipdx
				sta shipMinorX
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
				; not yet drawn ship. Indicate this by setting the replacedChar with 255 (invalid value it will never encounter)
				lda #255
				sta charReplaced
				
				clc
				ldx #screenstarthigh	; put start of screen hi in cursor position
				ldy #22 				; screen scroll start lo
				stx.z cursor + 1
				sty.z cursor
				ldy #0			; current *X* position
				ldx #0			; number of current line
.lineLoop				
				iny 			; (2) cycles. Gain 4
				lda (cursor),y 	; (5+) get existing character. Absolute addressing would be (4)
				dey				; (2)
				sta (cursor),y	; (6) store in previous location. Absolute addressing would be (4)
				iny 			; (2) 
				cpy #21			; (2)
.2				; .1 to .2 : 19+ originally
				bne .lineLoop 			; (2) continue within line
				
				; copy in the next line from the buffer
				lda nextLine,x
				sta (cursor),y
				
				; is it time to draw the ship?
				cpx shipy
				bmi .notDrawShip
				lda charReplaced
				cmp #255
				bne .notDrawShip ; already drawn it
				txa
				pha
				tya
				pha
				jsr drawship
				pla
				tay
				pla
				tax
.notDrawShip

				; move to next line
				ldy #0 			; (2) reset back to start of line counter
				inx				; (2) next line
				cpx #screenHeight - 3
				beq .finished	; (3) or (4) if page boundary crossed
				

				lda.z cursor	; (3)
				clc				; (2)
				adc #22			; (2)
				sta.z cursor	; (3)
				bcc	.lineLoop			; (3) or (4) continue with next line
				inc.z cursor + 1 ; (5)
				jmp .lineLoop 			;  (3) continue with next line						
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
				; if it's 255, don't draw
				cmp #255
				beq .done
				sta (cursor),x				
.done
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

prepareTowerPositions	subroutine
				ldy #nextLineLength - 1
				lda #spacePrintable
.loop
				sta nextLine,y
				cpy #0
				beq .done
				dey
				jmp .loop
.done
				rts
				
				
; color and colorcharacter already set
; towerMiddleCharacter already set for the non-top/bottom character, and also in a
; towerTopCharacter already set

; prepares nextLine with a line to copy in during the next scroll
drawtowerScope  subroutine
.shortTower
				cpy #0
				bne .not0
				lda #spacePrintable
				sta towerMiddleCharacter	
				jmp .draw
.not0
				lda towerTopCharacter
				jmp .draw
.switchToTopCharacter
				lda towerTopCharacter ; use this next
				sta character
				dey
				bne .1
				jmp .drawnBottom										
drawtower		
				;; If the tower is zero height, draw a space at the bottom and then draw a gap full height
				;; if the screen
				sta character
				ldy towerheight
				cpy #2 ; One high, so print the top character instead of the middle
				bmi .shortTower
.draw
				;; draw first block at bottom right, then build up from there
				ldx #nextLineLength - 1			
				ldy towerheight
				cpy #0
				beq .drawnBottom
				dey			; subtract one from tower height as bottom character is never drawn				
				beq .drawnBottom ; finished drawing bottom of tower?
.1				dex
.subfinished
				sta nextLine,x
.storecharcomplete1
				cpy #2	;[2]
				beq .switchToTopCharacter ;[2,3]
				dey ;[2]
				bne .1 ;[3,2] 
				
.drawnBottom	; now draw the gap
				ldy gapWidth
				lda towerheight		; if tower height is zero, don't draw the top
				cmp #0
				bne .readyToDrawGap
				ldy #screenheight - 2
.readyToDrawGap
				lda #spacePrintable
				sta character
				cpy #0
.3				beq .drawnGap
				dex ; one line up					
.subfinished2
				lda character ; TODO, see if we can remove the need to do this
				sta nextLine,x
				cpx #0
				beq .doneTower
				
.storecharcomplete2
				lda towercolumnsleft
				cmp fuelColumn
				bne .305

				; is this the row to print the fuel at?
				cpy fuelRow
				bne .305 ; not yet right height to print fuel
				
				; print fuel
				lda fuelChar; this is variable, as we may be drawing the first or second character
				sta nextLine,x
.storecharcomplete3
				lda #spacePrintable ; back to printing spaces
				
				; do we need to print the second edge?
				lda fuelChar
				cmp #fuelLeftPrintable
				beq .switchToRight
								
				; switch to printing the left edge again and choose the position for
				; the next fuel character
				lda #fuelLeftPrintable
				sta fuelChar
				lda #6
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
				lda topTowerEdgeCharacter
.4				
				sta	nextLine,x
				cpx #0
				beq .5	
				dex
				dey
				bne .4
				lda topTowerMiddleCharacter
				jmp .4
.5			
.doneTower	
				rts
			
defaulttowerwidth		equ		4

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
				lda towerTopCharacters,x ; character to draw at top edge of towers (around the gap)
				sta towerTopCharacter
				lda towercharacters,x ;
				sta towerMiddleCharacter 
				lda topTowerEdgeCharacters,x ; character to draw at bottom edge of towers (around the gap)
				sta topTowerEdgeCharacter
				lda topTowerCharacters,x ;
				sta topTowerMiddleCharacter 
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
		;		lda #3
		;		sta borderPaper
.rasterloop
				lda rasterline
				cmp #131 ; TODO: different value for NTSC, probably lower
				bpl .rasterloop
				
		;		lda #0
		;		sta borderPaper
				
.rasterLowerLoop

				lda rasterline
				cmp #130 ;130; TODO: different value for NTSC, probably lower
				bmi .rasterloop

		;		lda #1
		;		sta borderPaper

				rts

workOutShipPosition
				ldx shipx
				ldy shipy
				lda #255 ; don't draw
				sta character
				jsr drawchar
				
				lda cursor
				sta shipToBeDrawnAt1
				lda cursor + 1
				sta shipToBeDrawnAt1 + 1
				
				lda #screenwidth
				sta diff
				jsr addcursor
				
				lda cursor
				sta shipToBeDrawnAt2
				lda cursor + 1
				sta shipToBeDrawnAt2 + 1
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
				
drawshipchar	
				ldx #0
				lda (shipToBeDrawnAt1),x
				sta charReplaced2 ; store this for collision detection later
				lda character
				sta (shipToBeDrawnAt1),x

				cmp #spacePrintable
				beq .keepSpace
				clc
				adc #8 ; bottom set are 8 bytes further on
				sta character
.keepSpace
				lda (shipToBeDrawnAt2),x
				sta charReplaced
				lda character
				sta (shipToBeDrawnAt2),x
				rts
				
clearship		lda #spacePrintable
				sta character
				jmp drawshipchar
									
control			subroutine				
				lda fuel	; if fuel is exhausted, no control is possible
				bne .notEmpty
				rts
.notEmpty
				; scan keyboard for key presses or joystick
				lda #0
				sta $9120 ; reset keyboard state

				lda joystickIn1
				ora #255 - 32 ; set all other bits. Only care about fire button
				and $9121 ; get any 0 bits from keyboard state (indicates a key is pressed)
								
				ldx lastkey
				sta lastkey

				cpx #255 ; 255 indicates nothing is pressed
				bne .notpress 
				cmp #255
				beq .notpress
				
				; space just pressed
				; apply an impulse
thrust			
				lda #255
				sta jetSound
				lda #shipimpulse
				sta shipdy
				clc
				adc shipdx
				adc shipdx
				bcc .storedx
				lda #255
.storedx
				sta shipdx
				
				lda shipDirection
				cmp #directionUp
				beq .alreadyGoingUp
				lda #directionUp
				sta shipDirection	
				jsr swapMinorY
.alreadyGoingUp
				jsr decreaseFuel
.notpress		
				rts
			
physics			subroutine
				; update horizontal position 2 times
				lda shipMinorX
				clc
				adc shipdx
				sta shipMinorX
				bcc .notSmoothScroll1
				jsr smoothScroll
				jsr handleFullScroll
				
.notSmoothScroll1
				lda shipMinorX
				clc
				adc shipdx
				sta shipMinorX
				bcc .notSmoothScroll2
				jsr smoothScroll
				jsr handleFullScroll


.notSmoothScroll2
				; is it time to update the rest of the physics
				dec physicsCountdown
				beq .timeToUpdate
				rts
.timeToUpdate
				lda physicsCountdownInitialValue ; reset countdown timer
				sta physicsCountdown
			
				; update ship x position
				lda shipdx
				cmp #2
				bpl .donedx
				sec
				sbc #2
				sta shipdx		
.donedx
				; update ship y position. First the minor position
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
				ldx #0
				lda jetSound
				cmp #230
				bmi .makeSound ; don't set to make a sound, just use 0 (sound off)
				and #3
				beq .makeSound
				ldx jetSound
.makeSound
				stx voice3
				
				lda fuelSoundCount
				sta voice2
				rol
				sta voice0
				lda fuelSoundCount
				cmp #0
				beq .donefuelsound
				tax
				inx
				inx
				inx
				inx
				stx fuelSoundCount
.donefuelsound
				lda effectCount
				ldx #0
				cmp #0
				beq .doneEffect
				; the effect is in progress
				ldx #250
				dec effectCount
				and #3
				beq .doneEffect
				ldx #255
				jsr increaseScoreBy10

.doneEffect
				stx	voice1
				rts
				
powerUp			subroutine
				lda #100
				sta effectCount
				rts
				
effectCount		dc.b 0


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
				cmp #21
				bmi .notShiftShipUp
				lda #21
				sta shipy
.notShiftShipUp
				cmp #1
				bpl .explodeLoop
				
				lda #1 ; shift ship down a bit
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
				cmp #1
				bmi .offTopEdge
				jmp .doneTopEdge
.offTopEdge
				lda #1
.doneTopEdge
				sta explosionTopEdge								
					
				; now draw the explosion box
				lda #explodePrintable
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
				
				; if we are beyond the first level, print the continue message
				lda levelNumber
				beq .notContinue
				lda #<continueMessage
				sta.z cursor;
				lda #>continueMessage
				sta.z cursor + 1
				jsr printline
.notContinue
				jsr waitForStartKey
				rts
								
waitForStartKey subroutine
				jsr random

				lda #0
				sta $9120 ; reset keyboard state
				lda $9121
								
				cmp #247 ; b key
				beq .restart

				cmp #254 ; space key
				beq .done
				
				lda joystickIn1
				and #32 ; set all other bits. Only care about fire button
				cmp #32								
				beq waitForStartKey
				jmp .done
				
.restart
				lda #0
				sta levelNumber ; reset level to start
				lda #10
				sta delayReduction
.done
				rts

; level format:
; Bytes 0 - 3: front, middle, middle, back edge characters for mid-way through tower on top
;       4 - 7: front, middle, middle, back edge characters for top and bottom edges of tower on top
;       8 - 11: front, middle, middle, back edge characters for mid-way through tower on bottom
;       12 - 15: front, middle, middle, back edge characters for top and bottom edges of tower on bottom

;       16    : horizontal gap between towers
;       17    : vertical gap between towers
;       18    : border and paper colour

;       19-20: number of moves before switching to next level (lo,hi)

;       21   : physics countdown timer initial value
;       22   : frames between scroll
;		23   : flags
;		24	 ; background map number

fuelActiveFlag	equ #1
space	equ spacePrintable

; specify the order of levels. 255 instructs to wrap around
finishedLevels	equ	255
levelOrder	dc.b	1,2,0,3,0,4,0,0,5,6,finishedLevels

startOfLevelDefinitions
spaceLevel				dc.b	space,space,space,space
						dc.b	space,space,starRightPrintable,starLeftPrintable
						dc.b	space,space,space,space
						dc.b	starRightPrintable,starLeftPrintable,space,space
						dc.b	1,14,8 ; black border, black paper
						dc.b	150,1
						dc.b	1, 1, 1, 1

towerChars0				dc.b	blackRightPrintable,blackLeftPrintable,blackRightPrintable,blackLeftPrintable
						dc.b	blackRightPrintable,blackPrintable,blackPrintable,blackLeftPrintable
						dc.b	blackRightPrintable,blackLeftPrintable,blackRightPrintable,blackLeftPrintable
						dc.b	blackRightPrintable,blackPrintable,blackPrintable,blackLeftPrintable
						dc.b	8,24,2
						dc.b	1,2 
						dc.b	1, 1, 0, 0
											
towerChars1				dc.b	space,towerRightPrintable,towerLeftPrintable,space
						dc.b	solidRightPrintable,solidPrintable,solidPrintable,solidLeftPrintable
						dc.b	space,towerRightPrintable,towerLeftPrintable,space
						dc.b	solidRightPrintable,solidPrintable,solidPrintable,solidLeftPrintable
						dc.b	10,10,0
						dc.b	1,2 
						dc.b	1,1, 0, 0
						
						dc.b	space,towerRightPrintable,towerLeftPrintable,space
						dc.b	solidRightPrintable,solidPrintable,solidPrintable,solidLeftPrintable
						dc.b	space,towerRightPrintable,towerLeftPrintable,space
						dc.b	solidRightPrintable,solidPrintable,solidPrintable,solidLeftPrintable
						dc.b	7,7,0
						dc.b	150,1 
						dc.b	1, 1, 0, 0
						
						dc.b	space,towerRightPrintable,towerLeftPrintable,space
						dc.b	solidRightPrintable,solidPrintable,solidPrintable,solidLeftPrintable
						dc.b	space,towerRightPrintable,towerLeftPrintable,space
						dc.b	solidRightPrintable,solidPrintable,solidPrintable,solidLeftPrintable
						dc.b	5,20,0
						dc.b	150,1 
						dc.b	1 ,1, 0, 0
						
maxLevel				equ 	7

;;;; Set up characters to use when drawing towers. X should have tower set number, 0 being the first
;;;; Every odd numbered level will be level 1. Otherwise, it's the level number divided by 2
setupTowerCharacters	subroutine				
				lda #<startOfLevelDefinitions
				sta cursor
				lda #>startOfLevelDefinitions
				sta cursor + 1
				ldy #0
.levelFindLoop
				lda levelOrder,y
				cmp #finishedLevels
				beq .endOfLevels
.endOfLevelsReturn
				cpx #0
				beq .foundLevel
				dex
				iny
				jmp .levelFindLoop
.foundLevel			
				tax
				; x now holds the number of the level description					
.xloop
				lda #25 ; number of positions to skip over to get next set of characters
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
				cpy #16
				bne .yloop
.done
				jsr .getNext
				sta distanceBetweenTowers
				
				jsr .getNext
				sta gapWidth
				
				jsr .getNext
				sta borderPaper
				
				jsr .getNext
				sta progressCounterLo
				
				jsr .getNext
				sta progressCounterHi
				
				jsr .getNext
				sta physicsCountdownInitialValue
				
				jsr .getNext
				sta delayAmount
				
				jsr .getNext
				sta flags
				
				jsr .getNext
				tax
				jsr prepareColors
				rts

.getNext		lda (cursor),y
				iny
				rts
.endOfLevels
				ldy #0 ; reset back to start
				; increase speed (TODO)
				jmp .endOfLevelsReturn
				
resetScroll		subroutine
				; reset smooth scrolling back to the start
				lda scrollCounter
				cmp #8  
				beq .scrollDone
				cmp #0
.scrollLoop
				beq .scrollDone
				jsr smoothScroll
				dec scrollCounter
				jmp .scrollLoop
.scrollDone
				lda #8
				sta scrollCounter
				sta lastFrameWasScroll
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
blackLeftChar	dc.b	0,0,0,0,0,0,0,0

rightEdges

solidRightChar	dc.b	255,255,0,255,255,0,255,255
towerRightChar	dc.b	145,137,197,163,145,137,197,163
fuelRightChar	dc.b	126,66,223,199,223,223,94,126
starRightChar	dc.b	16,16,56,254,56,16,16,16
blackRightChar	dc.b	255,255,255,255,255,255,255,255

numberOfScrollableCharacters equ (rightEdges - leftEdges) / 8

fuelLeftPrintable equ (fuelLeftChar - startOfChars) / 8
fuelRightPrintable equ (fuelRightChar - startOfChars) / 8
towerLeftPrintable equ (towerLeftChar - startOfChars) / 8
towerRightPrintable equ (towerRightChar - startOfChars) / 8
solidLeftPrintable equ (solidLeftChar - startOfChars) / 8
solidRightPrintable equ (solidRightChar - startOfChars) / 8
starLeftPrintable equ (starLeftChar - startOfChars) / 8
starRightPrintable equ (starRightChar - startOfChars) / 8
blackLeftPrintable equ (blackLeftChar - startOfChars) / 8
blackRightPrintable equ (blackRightChar - startOfChars) / 8

nonscrollable				
				; now the non scrollble characters
				; striped block, not scrollable
solidChar		dc.b	255,255,0,255,255,0,255,255				
solidPrintable equ (solidChar - startOfChars) / 8	
explodeChar		dc.b	85, 170, 85, 170, 85, 170, 85, 170
explodePrintable equ (explodeChar - startOfChars) / 8	
blackChar		dc.b	255,255,255,255,255,255,255,255
blackPrintable equ (blackChar - startOfChars) / 8	
	
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
				
				org	startOfChars + 32 * 8
				dc.b	0,0,0,0,0,0,0,0 ; define the space character
				
				org	startOfChars + 33 * 8
				
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
				dc	255, 4, 254, 0, 22 ; title
				dc	254, 2, 255, 4 ; change colours to clouds
				dc	70,1,20,2,1,2,16,7,7,1,1,2,4,7,6,6,16,6,115
				dc 	255, 5 ; change colour to buildings
				dc	1,3,1,15,1,1,1,3,1,15,3,1,1,1,1,6,1,7,4,1,5,3,2,7,11,2,3,5
				dc 	255, 1 ; change colour to grass
				dc	110
				dc 253 ; end
dataEnd
				dc.b	0

				echo "To run: SYS ", start
				echo "Total length ", dataEnd - start
				echo "Space left ", 7168 - programEnd


					