  .inesprg 2   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
gamestate  .rs 1  ; .rs 1 means reserve one byte of space
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button
nmicounter .rs 1  ; constant NMI counter for rng seeding purposes
titledrawn .rs 1  ; Has the title screen been drawn yet?
nkis	   .rs 1  ; Count of non-kitten-objects
nkiones    .rs 1  ; ones digit of NKIs for title screen
nkitens    .rs 1  ; tens digit of NKIs for title screen
lockup     .rs 1  ; Lock the up direction
lockdown   .rs 1  ; Lock the down direction
lockright  .rs 1  ; Lock the right direction
lockleft   .rs 1  ; Lock the left direction.
multtemp   .rs 1  ; Temp storage for multiplication
checkx  .rs 1 ; x position for movement checking
checky  .rs 1 ; y position for movement checking
founditem .rs 1 ; nki/kitten id bumped into
tmpx .rs 1; nki checking storage
tmpy .rs 1; nki checking storage
rngbuf .rs 14; buffer to make rng0 $0020
rng0 .rs 1; random number generator output
rng1 .rs 1; random number generator output
rng2 .rs 1; random number generator output
rng3 .rs 1; random number generator output
rng4 .rs 1; random number generator output
rng5 .rs 1; random number generator output
rng6 .rs 1; random number generator output
rng7 .rs 1; random number generator output
rng8 .rs 1; temp byte for rng manipulation
rngbuf2 .rs 7; 0020 for rng only
stringlist .rs 1 ; Which string list to use.
stringitself .rs 1; Which string to use
nkistrings .rs 63 ; Assign a string to the nki
nkislist .rs 63 ; Assign a list to the nki
linebuffer .rs 96 ; Took too long to do the conversion on the fly. So we're doing this.
displayingline .rs 1 ; Are we displaying a line? Lock out everything if we are.
buffertemp .rs 1 ; Using to tighten up the buffer display loop.
gameovercounter .rs 1 ; Counting gameover NMIs
gameoverstage .rs 1 ; Keeping track of what state we're in with the gameover animation. 

;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
MOVEDELAY  = $1A  ; NMI between moves

; Button codes and combinations
BUTRIGHT  =$1 
BUTLEFT   =$2 
BUTDOWN   =$4
BUTUP     =$8
BUTSTART  =$10
BUTSELECT =$20
BUTB      =$40
BUTA      =$80

;;;;;;;;;;;;;;;;;;



; start with bank 0
  .bank 0
  
  ; indirect addressing must be in the zero page
  .org $0080
addrLO:	.db 0  ; make "variable"s for our indirect addressing
addrHI: .db 0

; Our real bank 0 starts here
  .org $8000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  TXA
  AND #$F0
  CMP #$20 
  BEQ clrmem2 ; skip 0020s (rng)
  LDA #$00
  STA $0000, x
clrmem2:
  LDA #$00
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
  
  JSR first_seed

;;:Set starting game state. We want to start at title
  LDA #STATETITLE
  STA gamestate

  
  LDA #$20 ; Default number of nkis
  STA nkis
  
  ;JSR turn_screen_on
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  
Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI
  
 

NMI:
  STA tmpx

  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  LDA displayingline
  
  BNE skipnmi
  
  JSR random_number ; for randomness

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  ;LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ;STA $2000
  ;LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  ;STA $2001

    
  ;;;all graphics updates done by here, run game engine



  JSR ReadController1  ;;get the current button data for player 1
  JSR ReadController2  ;;get the current button data for player 2
  
  INC nmicounter
  
GameEngine:  
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle    ;;game is displaying title screen
    
  LDA gamestate
  CMP #STATEGAMEOVER
  BNE dj1
  JMP EngineGameOver  ;;game is displaying ending screen

dj1:
  LDA gamestate
  CMP #STATEPLAYING
  BNE dj2
  LDA #$DE
  STA $0500
  JMP EnginePlaying   ;;game is playing
dj2:
  
GameEngineDone:  
  
  JSR UpdateSprites  ;;set ball/paddle sprites from positions
  
skipnmi:
  LDA tmpx
  RTI             ; return from interrupt
 
 
 
 
;;;;;;;;


first_seed:
  LDA #0
  CMP rng0
  BNE first_seed_end
  CMP rng1
  BNE first_seed_end
  CMP rng2
  BNE first_seed_end
  CMP rng3
  BNE first_seed_end
  LDA #$FF
  CMP rng4
  BNE first_seed_end
  CMP rng5
  BNE first_seed_end
  CMP rng6
  BNE first_seed_end
  CMP rng7
  BNE first_seed_end

;;;Set some initial ball stats
  ; seed rng
  LDA #$69
  STA rng0
  LDA #$00
  STA rng1
  LDA #$34
  STA rng2
  LDA #$56
  STA rng3
  LDA #$78
  STA rng4
  LDA #$9A
  STA rng5
  LDA #$BC
  STA rng6
  LDA #$DF
  STA rng7
first_seed_end:
  RTS 
 
EngineTitle:
  
  LDA titledrawn ; Check to see if the title is done displaying. If not, keep going, otherwise skip the drawing.
  BNE DoneDisp
  LDA $2002    ; read PPU status to reset the high/low latch to high
  LDA #$3F
  STA $2006    ; write the high byte of $3F10 address
  LDA #$1D
  STA $2006    ; write the low byte of $3F10 address
  LDA #$2D
  STA $2007
  
  
  JSR turn_screen_off
  ;LDX #$40
  ;STX $4017    ; disable APU frame IRQ
  ;LDX #$FF
  ;TXS          ; Set up stack
  ;INX          ; now X = 0

  ;STX $2000    ; disable NMI
  ;STX $2001    ; disable rendering
  ;STX $4010    ; disable DMC IRQs
  
    ldx #0
	lda #$20  ; set the destination address in PPU memory
  	sta $2006  ; should be $2000
  	stx $2006
  	lda #low(title)   ; put the high and low bytes of the address "title"
  	sta addrLO        ; into the variables so we can use indirect addressing.
  	lda #high(title)
  	sta addrHI

	ldx #4  ; number of 256-byte chunks to load
  	ldy #0
bgloop:
  	lda [addrLO],y
  	sta $2007     ; load 256 bytes
  	iny
  	bne bgloop
;--------------------
  	inc addrHI  ; increment high byte of address title to next 256 byte chunk
  	dex        ; one chunk done so X = X - 1.
  	bne bgloop   ; if X isn't zero, do again
	jsr turn_screen_on
    lda #$01
	sta titledrawn
	
DoneDisp:
  ;LDA #$00        ;;tell the ppu there is no background scrolling
  ;STA $2005
  ;STA $2005

  ; We're done displaying the title. Start polling for the buttons to increase/decrease nkis and start to start
  LDA buttons1
  AND #BUTSTART
  BEQ NoStart
  LDA #STATEPLAYING
  STA gamestate
    ; seed rng
  ;LDA nmicounter
  
  JSR clear_screen
  JSR SpriteSetup
  JSR nkiPageSel
  ;LDA #$20 ; static kitteh for testing
  ;STA $0205 ; kitteh tile
  ;LDA #3 ; kitteh palette
  ;STA $0206 ; kitteh tile
  
  JMP GameEngineDone
  
  ; Start not pushed, check for up and increment nkis if need be
NoStart:
  LDA buttons1
  AND #BUTUP
  BEQ NoUp
  LDA nmicounter
  AND #%00000111
  BNE NoUp
  INC nkis
  LDA nkis
  CMP #$40 ; Prevent nkis from going over 63.
  BNE notmax
  LDA #$3F
  STA nkis
notmax:
  
  ; Up not pushed, decrement nkis
NoUp:
  LDA buttons1
  AND #BUTDOWN
  BEQ NoDown
  LDA nmicounter
  AND #%00000111
  BNE NoDown
  DEC nkis
  LDA nkis
  CMP #$00 ; Prevent nkis from going below 0 (underflow to 255)
  BNE notmin
  LDA #$01
  STA nkis
notmin:
  
  ; Down not pushed, go on with life.
NoDown:

  LDX #$00
  STX nkiones ; Blank out ones temp
  STX nkitens ; Blank out tens temp
  
  ; Routine to convert the hex nkis to decimal output for the screen display.
IncOnes:
  INX
  CPX nkis
  BEQ IncDone
  LDA nkiones
  CLC
  ADC #$01
  STA nkiones
  CMP #$0A
  BEQ IncTens
  JMP IncOnes
IncTens:
  LDA #$00
  STA nkiones
  LDA nkitens
  CLC
  ADC #$01
  STA nkitens
  JMP IncOnes
IncDone:
  
  LDA nkiones
  STA nkiones
  LDA nkitens
  STA nkitens
  
  LDA #$8D
  STA $0200
  LDA nkitens
  CLC
  ADC #$10
  STA $0201
  LDA #%00000011
  STA $0202
  LDA #$A4
  STA $0203
  
  LDA #$8D
  STA $0204
  LDA nkiones
  CLC
  ADC #$10
  ; Write the values to the sprites in the right spot.
  STA $0205
  LDA #%00000011
  STA $0206
  LDA #$AC
  STA $0207
  
  JMP GameEngineDone

;;;;;;;;; 
 
;;; GAMEOVERNMI HERE

EngineGameOver:

  LDA gameoverstage
  CMP #$04
  BEQ animdone
  
  INC gameovercounter
  CMP gameovercounter
  BEQ incgameoverstage
  JMP nostage
  
incgameoverstage:
	INC gameoverstage
	
nostage:

animdone:
  ; Checking for Start pushed
  LDA buttons1
  AND #BUTSTART
  BEQ NoGOStart
  LDA #$01
  STA multtemp
  JMP SkipHere
  
NoGOStart:
  ; Once start pushed and then released, return to title screen.
  LDA multtemp
  BEQ SkipHere
  JMP RESET
  
 ; Win/game over will go here.
  
SkipHere:
  JMP GameEngineDone
 
;;;;;;;;;;;
 
EnginePlaying:
  ; mark current position
  LDA $0200
  STA checky
  LDA $0203
  STA checkx


;-------------------UP--------------------------
  LDA buttons1
  AND #BUTUP
  BEQ NoRUp
  LDA lockup
  CMP #0
  BNE NoRUpLocked 

  LDA $0200
;  CMP #$27
;  BEQ NoRUpLocked
  SEC
  SBC #$08 ; Actually reducing the location
; A contains destination y
  STA checky

;  STA $0200
  LDA #MOVEDELAY ; 
  STA lockup
  JMP NoRUpLocked
NoRUp:
  LDA #$00
  STA lockup
  JMP RDown
NoRUpLocked:
  LDA lockup
  SEC
  SBC #1
  STA lockup

;-------------------DOWN------------------------
RDown:
  LDA buttons1
  AND #BUTDOWN
  BEQ NoRDown
  LDA lockdown
  CMP #0
  BNE NoRDownLocked
 
  LDA $0200
;  CMP #$DF
;  BEQ NoRDownLocked
  CLC
  ADC #$08
; A contains destination y
  STA checky

;  STA $0200
  LDA #MOVEDELAY
  STA lockdown
  JMP NoRDownLocked
;  JMP GameEngineDone
NoRDown:
  LDA #$00
  STA lockdown  
  JMP RRight
NoRDownLocked:
  LDA lockdown
  SEC
  SBC #1
  STA lockdown
;-------------------RIGHT-----------------------
  
RRight:
  LDA buttons1
  AND #BUTRIGHT
  BEQ NoRRight
  LDA lockright
  CMP #0
  BNE NoRRightLocked

  LDA  $0203
;  CMP #$F8
;  BEQ NoRRightLocked
  CLC
  ADC #$08
  STA checkx
;  STA $0203
  LDA #MOVEDELAY
  STA lockright
;  JMP GameEngineDone
  JMP NoRRightLocked
NoRRight:
  LDA #$00
  STA lockright
  JMP RLeft 
NoRRightLocked:
  LDA lockright
  SEC
  SBC #1
  STA lockright
;-------------------LEFT------------------------

RLeft:
  LDA buttons1
  AND #BUTLEFT
  BEQ NoRLeft
  LDA lockleft
  CMP #0
  BNE NoRLeftLocked
  LDA $0203
;  CMP #$00
;  BEQ NoRLeftLocked
  SEC
  SBC #$08
  STA checkx
;  STA $0203
  LDA #MOVEDELAY
  STA lockleft
;  JMP GameEngineDone
  JMP NoRLeftLocked
NoRLeft:
  LDA #$00
  STA lockleft
  JMP RChecks
NoRLeftLocked:
  LDA lockleft
  SEC
  SBC #1
  STA lockleft
  


RChecks:

  LDA checkx
  CMP $0203
  BNE CheckControls
  LDA checky
  CMP $0200
  BNE CheckControls
  JMP EndControls
  
CheckControls:
  JSR CheckBoundsAndCollisions

EndControls:
  JMP GameEngineDone

CheckBoundsAndCollisions:
  ; Lower Bounds
  LDA checkx
  CMP #$00 ; x minimum
  BEQ CBACCchecky ; on the line is in
  BCC EndOfCheckBounds ; not possible when xmin is 0

CBACCchecky:
  LDA checky
  CMP #$28
  BEQ CBACCcheckupy
  BCC EndOfCheckBounds


CBACCcheckupy:
  ; UpperBounds
  CMP #$D9
  BEQ CBACCcheckupx
  BCS EndOfCheckBounds
CBACCcheckupx:
  LDA checkx
  CMP #$F8
  BEQ CBACCLoopSetup
  BCS EndOfCheckBounds
CBACCLoopSetup:
  ; Collisions
  LDX #0 ; compare vs nkis
  LDY #0 ; increases by 4 per nki loop
CBACCLoop:
  ; loop through all nkis, checking y then x and bailing with a double match.
  CPX nkis
  BEQ CheckedOutCanMove
  LDA checky
  CMP $0204,y
  BEQ CBACCyMatch
  JMP CBACCNoMatch
CBACCyMatch:
  LDA checkx
  CMP $0207,y
  BEQ FoundMatch
CBACCNoMatch:
  ;doesn't match, increment and loop
  INX
  INY
  INY
  INY
  INY
  JMP CBACCLoop
FoundMatch:
  ; X = item which might be kitten
  STX founditem
  ;STX $D0  
  JMP HandleItem
CheckedOutCanMove:
  ; move robot here, to checkx,checky
  LDA checky
  STA $0200
  LDA checkx
  STA $0203
EndOfCheckBounds:
  RTS
 

HandleItem:
  ; display message correlating with X register'd item

  LDA founditem
  CMP #0 ; kitten!
  BEQ GameOver
  
  JSR DispLine
  RTS

GameOver:
  ; Blank out line for displaying win animation
  JSR LineBlank
  
  LDA #$00
  STA multtemp
  STA gameoverstage
  STA gameovercounter
  
  LDA #STATEGAMEOVER
  STA gamestate
GameOverLock
  JMP GameOverLock ; forever FIXME
 
 
 
UpdateSprites:
; Updating robot sprite
  
  
  ;;update paddle sprites
  RTS
 
 
ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS
  
ReadController2:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController2Loop:
  LDA $4017
  LSR A            ; bit0 -> Carry
  ROL buttons2     ; bit0 <- Carry
  DEX
  BNE ReadController2Loop
  RTS  
  
turn_screen_on:
  ; Setup the PPU
  ;LDA #%10010111   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  rts 

turn_screen_off:
  LDX #$00     ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs
  rts  

clear_screen:
    jsr turn_screen_off
	ldx #4  ; number of 256-byte chunks to load
  	ldy #0
	
clrloop:
	lda #$00
  	sta $2007     ; load 256 bytes
  	iny
  	bne clrloop
;--------------------
  	dex        ; one chunk done so X = X - 1.
  	bne clrloop   ; if X isn't zero, do again
	; Reset the Attribute table for strings
LoadAttribute:

  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
  LDA %01010101
LoadAttributeLoop:

  STA $2007             ; write to PPU
  INX  					; X = X + 1
  CPX #$40              ; Compare X to hex 64. Copying lots of bytes.
  BNE LoadAttributeLoop
  
  jsr turn_screen_on
  RTS
;;;;;;;;;;;;;;  

SpriteSetup:
  jsr turn_screen_off
  CLC
roby:
  JSR random_number
  AND #$F8 ; (multiples of 8)
  ;AND #$38 ; (for testing)
  STA $0200
;  LDA #$1 ; --n8TEST
;  AND #$1f ; reduce random bits to 0-31
;  AND #$7 ; --n8TEST
;  STA $0200
;  CMP #$17
;  BCS roby
;  LDX #$07
;ymult:
;  ADC $0200
;  DEX
;  CPX #$00
;  BNE ymult
;  ROL $0200
;  ROL $0200
;  ROL $0200

  ;ADC #$28
  CLC
  ADC #1 ; y offset
  STA $0200
  
  LDA #$B0
  STA $0201
  LDA #$00
  STA $0202
  CLC
  
robx:
  JSR random_number
  AND #$F8 ; reduce random bits to 0-31
  STA $0203
  ;CMP #$1f ; n8-- = xmax? was $17 (ymax?)
  ;BCS robx
  
;  LDX #$07
;xmult:
;  ADC $0203
;  DEX
;  CPX #$00
;  BNE xmult
;  ROL $0203
;  ROL $0203
;  ROL $0203

  ;SBC #$0E
  ;CLC
  ;ADC #2 ; x adjustment
  ;STA $0203
  
  
  LDX #$00
  LDY #$00
  
RandSpritesLoop:
  ;LDA #$AD
  ;STA $0503
nkiy:
  CLC
  JSR random_number
  ;STA $0164,x -- debug logging in memory
  AND #$F8 ; (multiples of 8)
  ;STA $0165,x -- debug logging in memory
  CLC
  ADC #$29 ; Y-offset 1 pixel + one tile width (drawing from the bottom left) + 3 tile widths for info
  BCS nkiy ; -- >FF is a rejection; could handle here or earlier with another CMP
  CMP #$D7 ; limit to usable tiles (y cutoff)
  BCS nkiy
  
  STA $0204,x ; y coord
nkiTile:
  JSR random_number
  AND #$7F
  BEQ nkiTile ; no zeros
  CMP #$03
  BNE nkiTileRow
  CLC
  ADC #1 ; fix #'s to $'s
nkiTileRow:
  STA $0205,x ; tile number
  AND #$70 
  CMP #$70 ; make row 7 row 6
  BNE nkiGotTile
  LDA $0205,x
  AND #$6F
  STA $0205,x ; tile number
nkiGotTile:  
  JSR random_number
  ;LDA #$00
  AND #03 ; random palette
  STA $0206,x ; attributes
nkix:
  CLC
  JSR random_number
  SEC
  SBC #1 ; range should really be 0-254 than 1-255
  
  ;STA $0160,x ; debug
  AND #$F8
  ;STA $0161,x ; debug
  ;CLC
  ;ADC #$1 ;  one pixel
  ;STA $0162,x ; debug
  ;BCS nkix
  CMP #$F1 ; x cutoff
  BEQ nkix0 
  BCS nkix
nkix0:

  STA $0207,x ; x coordinate
  JSR nkiAvoidOverlap
  BCS RandSpritesLoop ; overwrite current nki, no increments

nkiNextnki:
  TXA
  CLC
  ADC #4 ; increment X by 4
  TAX
  
  INY
  CPY nkis
  BNE RandSpritesLoop
  JSR turn_screen_on
  RTS

nkiAvoidOverlap:
  ; save x and y
  STX tmpx
  STY tmpy ; holds current nki to compare
  LDY tmpx ; current offset
nkiOverlapLoop:
  ; X is offset from 0204 to current
  ; current to previous is 0200,x
  ; decrementing to robot @ 0200,x=0 will bail loop

  LDA $0200,x
  CMP $0204,y
  BNE nkiOverlapNextLoop
  LDA $0203,x
  CMP $0207,y
  BEQ nkiOverlapFound
  

nkiOverlapNextLoop:
  ; decrement X by 4
  TXA
  SEC
  SBC #4
  ;BEQ nkiOverlapNotFound ; disabled to check against robot
  BCC nkiOverlapNotFound
  TAX
  JMP nkiOverlapLoop

nkiOverlapNotFound:
  CLC ; carry bit is the return value
  JMP nkiOverlapEnd
nkiOverlapFound:
  SEC ; carry bit is the return value
nkiOverlapEnd:
  LDX tmpx
  LDY tmpy  
  
  RTS

nkiPageSel:
  STX tmpx

  LDX #$00
  
  ; Select which page each item is going to have for a string
nkiPageSelLoop:
  JSR random_number
  AND #%00000011 ; Keeping the last two bits. We only have three pages, this gets us 0-3
  CMP #%00000011 ; We actually don't want 3. Try again. We only want 3 pages.
  BEQ nkiPageSelLoop
  
  STA nkislist,X 

RetryStringLoop:  
  JSR random_number ; This will be for the actual string. Needs 146
  CLC
  CMP #$92 ; We really only want numbers under 146. Not too bad a performance penalty here.
  BCS RetryStringLoop
  
  STA nkistrings,X
  INX
  
  ; Keep on going for all 63 nkis. This doesn't seem to take too long, so might as well do it even if we don't need it.
  CPX #$3F
  BNE nkiPageSelLoop
  
  
  
  LDX tmpx
  RTS
  
  
;random_number:
;  LDA #$AF
;  STA $0504
;  lda nmicounter+1
;  asl A
;  asl A
;  eor nmicounter+1
;  asl A
;  eor nmicounter+1
;  asl A
;  asl A
;  eor nmicounter+1
;  asl A
;  rol nmicounter         ;shift this left, "random" bit comes from low
;  rol nmicounter+1
;  RTS
  
random_number:
  LDA rng6
  ROL A     
  ROL rng7  ; put bit7 from rng6 as bit0 in rng7
  ROL A     ; keep bit6 from rng6 in carry
  LDA rng7  ; load rng7
  ROL A     ; shift A with bit6 from rng6 to align all 8 eor pairs
  EOR rng7  ; xor
  EOR #$FF  ; xnor
  ;EOR nmicounter
  STA rng8  ; store result in temp storage
  LDA rng6  ; slide all registers to the next register
  STA rng7  
  LDA rng5
  STA rng6
  LDA rng4
  STA rng5
  LDA rng3
  STA rng4
  LDA rng2
  STA rng3
  LDA rng1
  STA rng2
  LDA rng0
  STA rng1
  LDA rng8  ; temp into 0
  STA rng0
  RTS

DispLine:  

  LDA #$01
  STA displayingline
  
  LDX founditem
  
  LDA nkislist, X ; Should be the page to use. Storing for later
  STA stringlist
  ;STA $D1
  
  LDA nkistrings, X ; Should be string itself to use. Storing for later
  STA stringitself
  ;STA $D2
  
  LDX #$00 
  LDY #$00
  STY multtemp ; reusing this; blank it out. Hopefully don't come to regret this but it oughtn't be used.
  
DispLineLoop:

  ; Selector for which page to use. 
  LDA stringlist 
  ;JMP ldst1
  
  CMP #$00
  BEQ ldst1
  
  CMP #$01
  BEQ ldst2
  
  CMP #$02
  BEQ ldst3
  
  ; Each of these works the same:
  ; For each page, we want to load the first character in.
  ; Then we jump down to the common portion.
ldst1:
  
  LDA #high(strings)
  STA addrHI
  ;STA $D5
  
  LDA #low(strings)
  STA addrLO
  ;STA $D4
  
  JMP DoneInitLoad
  
ldst2:

  LDA #high(strings2)
  STA addrHI
  
  LDA #low(strings2)
  STA addrLO
  JMP DoneInitLoad
  
ldst3:
  LDA #high(strings3)
  STA addrHI
  
  LDA #low(strings3)
  STA addrLO
  JMP DoneInitLoad


DoneInitLoad:  
  LDA [addrLO], Y ; Loads in the actually character we're displaying
  CMP #$40  ; Check to see if it's the delimiter
  BEQ incLinePos
InitReturn:
  INY
  JMP DoneInitLoad ; It's not, move along
  
  ; Gotta fill up the buffer
FillBufferStart:
  LDY $00

FillBufferLoop:
  LDA [addrLO], Y
  CMP #$40 ; Next delineator? We're done
  BEQ BufferDone
  SEC
  SBC #$20 ; Convert to ASCII
  STA linebuffer, Y ; Stick it in the line buffer
  INY ; Inc Y-offset
  JMP FillBufferLoop
  
BufferDone:
  LDA #$00
  STA linebuffer, Y
  INY
  LDA $2002
  CPY #$60
  BNE BufferDone  
  
  LDA #$20
  STA buffertemp
  
  LDY #$00
LineStart:
  lda $2002    ;wait
  bpl LineStart
  
  lda #$20        ;set ppu to start of VRAM
  sta $2006       
  lda #$3F     
  sta $2006
LineCont:  
  LDA linebuffer, Y ; Load in the character to display
  STA $2007
  INY ; Incrementing for the next character
  CPY buffertemp
  BEQ buffersplit
  JMP LineCont
  
strdone:
  ; First we need to 
  ;LDA #$00 ; String's done, but we need to blank out the rest of the lines to remove old strings.
  ;STA $2007 ; Stick it in the PPU
  ;INY 
  ;CPY #$50 ; Are we done yet?
  ;BNE strdone ; Nope, back again we go.
  
  ; We don't actually want to disable sprite rendering, but here we are.
  ;LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  ;STA $2001
  
  ;LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ;STA $2000
  
  LDA #$00        ;;tell the ppu there is no background scrolling. Stupid PPU.
  STA displayingline ; No longer displaying line 
  STA $2005
  STA $2005
  RTS  

incLinePos:
  LDA multtemp
  CMP stringitself
  BEQ FillBufferStart
  
  INC multtemp ; Increment our line pos. This is to count the strings.
  TYA
  CLC
  ADC addrLO
  STA addrLO
  ;TAX
  ;LDA #$00
  ;ADC addrLO, X ; Increment our low address
  ;STA addrLO
  ;STA $D6
  BCC NoOverflow ; Did our LO address overflow? No? Well then we don't want to do anything to our Hi
  INC addrHI ; Since our overflow did get set, we need to increment our hi address.
  CLC
NoOverflow:
  LDA addrHI
  ;STA $D7
  LDY #$00
  
  JMP InitReturn ; Back home we go.
  
buffersplit:
  LDA #$00 ; Reset scrolling. Stupid PPU.
  STA $2005
  STA $2005
  
  LDA #$20 ; Add to our buffertemp for the next line
  CLC
  ADC buffertemp
  CMP #$80
  BEQ strdone
  STA buffertemp
  
newvblankwait:  
  LDA $2002
  BPL newvblankwait
  
  lda #$20        ;set ppu to start of VRAM
  sta $2006       
  lda buffertemp
  CLC
  ADC #$20
  sta $2006
  JMP LineCont
  
LineBlank:
  lda $2002    ;wait
  bpl LineStart
  
  lda #$20        ;set ppu to start of VRAM
  sta $2006       
  lda #$3F     
  sta $2006
  LDA #$00
LineBlankCont:  
 ; Load in the character to display
  STA $2007
  INY ; Incrementing for the next character
  CPY #$60
  BNE LineBlankCont
  RTS
  
title: 
  .incbin "title.bin"
  
  .bank 1
  .org $A000
palette:
  .db $0f,$2d,$10,$30,  $0f,$30,$21,$31,  $0f,$06,$16,$26,  $0f,$2d,$19,$29   ;; background palette
  .db $0f,$1a,$30,$37,  $16,$01,$21,$31,  $26,$28,$25,$35,  $36,$16,$29,$39   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .db $80, $B0, $00, $80   ;sprite 0
  ;.db $80, $33, $00, $88   ;sprite 1
  ;.db $88, $34, $00, $80   ;sprite 2
  ;.db $88, $35, $00, $88   ;sprite 3

;attribute:  
  ;.db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  ;.db %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000
  
strings:
  .db $40, $22, $49, $20, $70, $69, $74, $79, $20, $74, $68, $65, $20, $66, $6f, $6f
  .db $6c, $20, $77, $68, $6f, $20, $6d, $69, $73, $74, $61, $6b, $65, $73, $20, $6d
  .db $65, $20, $66, $6f, $72, $20, $6b, $69, $74, $74, $65, $6e, $21, $22, $2c, $20
  .db $73, $65, $7a, $20, $4d, $72, $2e, $20, $54, $2e, $0a, $40, $54, $68, $61, $74
  .db $27, $73, $20, $6a, $75, $73, $74, $20, $61, $6e, $20, $6f, $6c, $64, $20, $74
  .db $69, $6e, $20, $63, $61, $6e, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $6e
  .db $20, $61, $6c, $74, $61, $72, $20, $74, $6f, $20, $74, $68, $65, $20, $68, $6f
  .db $72, $73, $65, $20, $67, $6f, $64, $2e, $0a, $40, $41, $20, $62, $6f, $78, $20
  .db $6f, $66, $20, $64, $61, $6e, $63, $69, $6e, $67, $20, $6d, $65, $63, $68, $61
  .db $6e, $69, $63, $61, $6c, $20, $70, $65, $6e, $63, $69, $6c, $73, $2e, $20, $54
  .db $68, $65, $79, $20, $64, $61, $6e, $63, $65, $21, $20, $54, $68, $65, $79, $20
  .db $73, $69, $6e, $67, $21, $0a, $40, $49, $74, $27, $73, $20, $61, $6e, $20, $6f
  .db $6c, $64, $20, $44, $75, $6b, $65, $20, $45, $6c, $6c, $69, $6e, $67, $74, $6f
  .db $6e, $20, $72, $65, $63, $6f, $72, $64, $2e, $0a, $40, $41, $20, $62, $6f, $78
  .db $20, $6f, $66, $20, $66, $75, $6d, $69, $67, $61, $74, $69, $6f, $6e, $20, $70
  .db $65, $6c, $6c, $65, $74, $73, $2e, $0a, $40, $41, $20, $64, $69, $67, $69, $74
  .db $61, $6c, $20, $63, $6c, $6f, $63, $6b, $2e, $20, $49, $74, $27, $73, $20, $73
  .db $74, $75, $63, $6b, $20, $61, $74, $20, $32, $3a, $31, $37, $20, $50, $4d, $2e
  .db $0a, $40, $54, $68, $61, $74, $27, $73, $20, $6a, $75, $73, $74, $20, $61, $20
  .db $63, $68, $61, $72, $72, $65, $64, $20, $68, $75, $6d, $61, $6e, $20, $63, $6f
  .db $72, $70, $73, $65, $2e, $0a, $40, $49, $20, $64, $6f, $6e, $27, $74, $20, $6b
  .db $6e, $6f, $77, $20, $77, $68, $61, $74, $20, $74, $68, $61, $74, $20, $69, $73
  .db $2c, $20, $62, $75, $74, $20, $69, $74, $27, $73, $20, $6e, $6f, $74, $20, $6b
  .db $69, $74, $74, $65, $6e, $2e, $0a, $40, $41, $6e, $20, $65, $6d, $70, $74, $79
  .db $20, $73, $68, $6f, $70, $70, $69, $6e, $67, $20, $62, $61, $67, $2e, $20, $50
  .db $61, $70, $65, $72, $20, $6f, $72, $20, $70, $6c, $61, $73, $74, $69, $63, $3f
  .db $0a, $40, $43, $6f, $75, $6c, $64, $20, $69, $74, $20, $62, $65, $2e, $2e, $2e
  .db $20, $61, $20, $62, $69, $67, $20, $75, $67, $6c, $79, $20, $62, $6f, $77, $6c
  .db $69, $6e, $67, $20, $74, $72, $6f, $70, $68, $79, $3f, $0a, $40, $41, $20, $63
  .db $6f, $61, $74, $20, $68, $61, $6e, $67, $65, $72, $20, $68, $6f, $76, $65, $72
  .db $73, $20, $69, $6e, $20, $74, $68, $69, $6e, $20, $61, $69, $72, $2e, $20, $4f
  .db $64, $64, $2e, $0a, $40, $4e, $6f, $74, $20, $6b, $69, $74, $74, $65, $6e, $2c
  .db $20, $6a, $75, $73, $74, $20, $61, $20, $70, $61, $63, $6b, $65, $74, $20, $6f
  .db $66, $20, $4b, $6f, $6f, $6c, $2d, $41, $69, $64, $28, $74, $6d, $29, $2e, $0a
  .db $40, $41, $20, $73, $68, $61, $6d, $65, $6c, $65, $73, $73, $20, $70, $6c, $75
  .db $67, $20, $66, $6f, $72, $20, $52, $6f, $63, $6b, $62, $6f, $78, $3a, $20, $68
  .db $74, $74, $70, $3a, $2f, $2f, $77, $77, $77, $2e, $72, $6f, $63, $6b, $62, $6f
  .db $78, $2e, $6f, $72, $67, $0a, $40, $41, $20, $66, $72, $65, $73, $68, $6c, $79
  .db $2d, $62, $61, $6b, $65, $64, $20, $70, $75, $6d, $70, $6b, $69, $6e, $20, $70
  .db $69, $65, $2e, $0a, $40, $41, $20, $6c, $6f, $6e, $65, $2c, $20, $66, $6f, $72
  .db $67, $6f, $74, $74, $65, $6e, $20, $63, $6f, $6d, $6d, $61, $2c, $20, $73, $69
  .db $74, $73, $20, $68, $65, $72, $65, $2c, $20, $73, $6f, $62, $62, $69, $6e, $67
  .db $2e, $0a, $40, $4f, $4e, $45, $20, $48, $55, $4e, $44, $52, $45, $44, $20, $54
  .db $48, $4f, $55, $53, $41, $4e, $44, $20, $43, $41, $52, $50, $45, $54, $20, $46
  .db $49, $42, $45, $52, $53, $21, $21, $21, $21, $21, $0a, $40, $49, $74, $27, $73
  .db $20, $52, $69, $63, $68, $61, $72, $64, $20, $4e, $69, $78, $6f, $6e, $27, $73
  .db $20, $6e, $6f, $73, $65, $21, $0a, $40, $49, $74, $27, $73, $20, $4c, $75, $63
  .db $79, $20, $52, $69, $63, $61, $72, $64, $6f, $2e, $20, $22, $41, $61, $61, $61
  .db $68, $2c, $20, $52, $69, $63, $6b, $79, $21, $22, $2c, $20, $73, $68, $65, $20
  .db $73, $61, $79, $73, $2e, $0a, $40, $59, $6f, $75, $20, $73, $74, $75, $6d, $62
  .db $6c, $65, $20, $75, $70, $6f, $6e, $20, $42, $69, $6c, $6c, $20, $47, $61, $74
  .db $65, $73, $27, $20, $73, $74, $61, $6e, $64, $2d, $75, $70, $20, $61, $63, $74
  .db $2e, $0a, $40, $4a, $75, $73, $74, $20, $61, $6e, $20, $61, $75, $74, $6f, $67
  .db $72, $61, $70, $68, $65, $64, $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $74
  .db $68, $65, $20, $4b, $61, $6d, $61, $20, $53, $75, $74, $72, $61, $2e, $0a, $40
  .db $49, $74, $27, $73, $20, $74, $68, $65, $20, $57, $69, $6c, $6c, $20, $52, $6f
  .db $67, $65, $72, $73, $20, $48, $69, $67, $68, $77, $61, $79, $2e, $20, $57, $68
  .db $6f, $20, $77, $61, $73, $20, $57, $69, $6c, $6c, $20, $52, $6f, $67, $65, $72
  .db $73, $2c, $20, $61, $6e, $79, $77, $61, $79, $3f, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $6e, $6f, $74, $68, $65, $72, $20, $72, $6f, $62, $6f, $74, $2c, $20
  .db $66, $61, $6e, $63, $69, $65, $72, $20, $69, $6e, $20, $64, $65, $73, $69, $67
  .db $6e, $20, $74, $68, $61, $6e, $20, $79, $6f, $75, $20, $62, $75, $74, $20, $73
  .db $74, $72, $61, $6e, $67, $65, $6c, $79, $20, $69, $6d, $6d, $6f, $62, $69, $6c
  .db $65, $2e, $0a, $40, $4c, $65, $6f, $6e, $61, $72, $64, $20, $52, $69, $63, $68
  .db $61, $72, $64, $73, $6f, $6e, $20, $69, $73, $20, $68, $65, $72, $65, $2c, $20
  .db $61, $73, $6b, $69, $6e, $67, $20, $70, $65, $6f, $70, $6c, $65, $20, $74, $6f
  .db $20, $6c, $69, $63, $6b, $20, $68, $69, $6d, $2e, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $20, $73, $74, $75, $70, $69, $64, $20, $6d, $61, $73, $6b, $2c, $20
  .db $66, $61, $73, $68, $69, $6f, $6e, $65, $64, $20, $61, $66, $74, $65, $72, $20
  .db $61, $20, $62, $65, $61, $67, $6c, $65, $2e, $0a, $40, $59, $6f, $75, $72, $20
  .db $53, $74, $61, $74, $65, $20, $46, $61, $72, $6d, $20, $49, $6e, $73, $75, $72
  .db $61, $6e, $63, $65, $28, $74, $6d, $29, $20, $72, $65, $70, $72, $65, $73, $65
  .db $6e, $74, $61, $74, $69, $76, $65, $21, $0a, $40, $49, $74, $27, $73, $20, $74
  .db $68, $65, $20, $6c, $6f, $63, $61, $6c, $20, $64, $72, $61, $66, $74, $20, $62
  .db $6f, $61, $72, $64, $2e, $0a, $40, $53, $65, $76, $65, $6e, $20, $31, $2f, $34
  .db $22, $20, $73, $63, $72, $65, $77, $73, $20, $61, $6e, $64, $20, $61, $20, $70
  .db $69, $65, $63, $65, $20, $6f, $66, $20, $70, $6c, $61, $73, $74, $69, $63, $2e
  .db $0a, $40, $41, $6e, $20, $38, $30, $32, $38, $36, $20, $6d, $61, $63, $68, $69
  .db $6e, $65, $2e, $0a, $40, $4f, $6e, $65, $20, $6f, $66, $20, $74, $68, $6f, $73
  .db $65, $20, $73, $74, $75, $70, $69, $64, $20, $22, $48, $6f, $6d, $65, $73, $20
  .db $6f, $66, $20, $74, $68, $65, $20, $53, $74, $61, $72, $73, $22, $20, $6d, $61
  .db $70, $73, $2e, $0a, $40, $41, $20, $73, $69, $67, $6e, $70, $6f, $73, $74, $20
  .db $73, $61, $79, $69, $6e, $67, $20, $22, $54, $4f, $20, $4b, $49, $54, $54, $45
  .db $4e, $22, $2e, $20, $49, $74, $20, $70, $6f, $69, $6e, $74, $73, $20, $69, $6e
  .db $20, $6e, $6f, $20, $70, $61, $72, $74, $69, $63, $75, $6c, $61, $72, $20, $64
  .db $69, $72, $65, $63, $74, $69, $6f, $6e, $2e, $0a, $40, $41, $20, $68, $61, $6d
  .db $6d, $6f, $63, $6b, $20, $73, $74, $72, $65, $74, $63, $68, $65, $64, $20, $62
  .db $65, $74, $77, $65, $65, $6e, $20, $61, $20, $74, $72, $65, $65, $20, $61, $6e
  .db $64, $20, $61, $20, $76, $6f, $6c, $6c, $65, $79, $62, $61, $6c, $6c, $20, $70
  .db $6f, $6c, $65, $2e, $0a, $40, $41, $20, $54, $65, $78, $61, $73, $20, $49, $6e
  .db $73, $74, $72, $75, $6d, $65, $6e, $74, $73, $20, $6f, $66, $20, $44, $65, $73
  .db $74, $72, $75, $63, $74, $69, $6f, $6e, $20, $63, $61, $6c, $63, $75, $6c, $61
  .db $74, $6f, $72, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $64, $61, $72
  .db $6b, $2c, $20, $61, $6d, $70, $68, $6f, $72, $6f, $75, $73, $20, $62, $6c, $6f
  .db $62, $20, $6f, $66, $20, $6d, $61, $74, $74, $65, $72, $2e, $0a, $40, $4a, $75
  .db $73, $74, $20, $61, $20, $70, $69, $6e, $63, $75, $73, $68, $69, $6f, $6e, $2e
  .db $0a, $40, $4f, $68, $20, $68, $65, $79, $2c, $20, $79, $6f, $75, $27, $72, $65
  .db $20, $6e, $6f, $74, $20, $4c, $6c, $6f, $72, $65, $61, $6e, $21, $0a, $40, $49
  .db $74, $27, $73, $20, $61, $20, $6d, $69, $67, $68, $74, $79, $20, $7a, $6f, $6d
  .db $62, $69, $65, $20, $74, $61, $6c, $6b, $69, $6e, $67, $20, $61, $62, $6f, $75
  .db $74, $20, $73, $6f, $6d, $65, $20, $6c, $6f, $76, $65, $20, $61, $6e, $64, $20
  .db $70, $72, $6f, $73, $70, $65, $72, $69, $74, $79, $2e, $0a, $40, $22, $44, $65
  .db $61, $72, $20, $72, $6f, $62, $6f, $74, $2c, $20, $79, $6f, $75, $20, $6d, $61
  .db $79, $20, $68, $61, $76, $65, $20, $61, $6c, $72, $65, $61, $64, $79, $20, $77
  .db $6f, $6e, $20, $6f, $75, $72, $20, $31, $30, $20, $4d, $49, $4c, $4c, $49, $4f
  .db $4e, $20, $44, $4f, $4c, $4c, $41, $52, $20, $70, $72, $69, $7a, $65, $2e, $2e
  .db $2e, $22, $0a, $40, $49, $74, $27, $73, $20, $6a, $75, $73, $74, $20, $61, $6e
  .db $20, $6f, $62, $6a, $65, $63, $74, $2e, $0a, $40, $41, $20, $6d, $65, $72, $65
  .db $20, $63, $6f, $6c, $6c, $65, $63, $74, $69, $6f, $6e, $20, $6f, $66, $20, $70
  .db $69, $78, $65, $6c, $73, $2e, $0a, $40, $41, $20, $62, $61, $64, $6c, $79, $20
  .db $64, $65, $6e, $74, $65, $64, $20, $68, $69, $67, $68, $2d, $68, $61, $74, $20
  .db $63, $79, $6d, $62, $61, $6c, $20, $6c, $69, $65, $73, $20, $6f, $6e, $20, $69
  .db $74, $73, $20, $73, $69, $64, $65, $20, $68, $65, $72, $65, $2e, $0a, $40, $41
  .db $20, $6d, $61, $72, $69, $6a, $75, $61, $6e, $61, $20, $62, $72, $6f, $77, $6e
  .db $69, $65, $2e, $0a, $40, $41, $20, $70, $6c, $75, $73, $68, $20, $43, $68, $65
  .db $77, $62, $61, $63, $63, $61, $2e, $0a, $40, $44, $61, $69, $6c, $79, $20, $68
  .db $75, $6e, $67, $65, $72, $20, $63, $6f, $6e, $64, $69, $74, $69, $6f, $6e, $65
  .db $72, $20, $66, $72, $6f, $6d, $20, $41, $75, $73, $74, $72, $61, $6c, $61, $73
  .db $69, $61, $0a, $40, $4a, $75, $73, $74, $20, $73, $6f, $6d, $65, $20, $73, $74
  .db $75, $66, $66, $2e, $0a, $40, $57, $68, $79, $20, $61, $72, $65, $20, $79, $6f
  .db $75, $20, $74, $6f, $75, $63, $68, $69, $6e, $67, $20, $74, $68, $69, $73, $20
  .db $77, $68, $65, $6e, $20, $79, $6f, $75, $20, $73, $68, $6f, $75, $6c, $64, $20
  .db $62, $65, $20, $66, $69, $6e, $64, $69, $6e, $67, $20, $6b, $69, $74, $74, $65
  .db $6e, $3f, $0a, $40, $41, $20, $67, $6c, $6f, $72, $69, $6f, $75, $73, $20, $66
  .db $61, $6e, $20, $6f, $66, $20, $70, $65, $61, $63, $6f, $63, $6b, $20, $66, $65
  .db $61, $74, $68, $65, $72, $73, $2e, $0a, $40, $49, $74, $27, $73, $20, $73, $6f
  .db $6d, $65, $20, $63, $6f, $6d, $70, $72, $6f, $6d, $69, $73, $69, $6e, $67, $20
  .db $70, $68, $6f, $74, $6f, $73, $20, $6f, $66, $20, $42, $61, $62, $61, $72, $20
  .db $74, $68, $65, $20, $45, $6c, $65, $70, $68, $61, $6e, $74, $2e, $0a, $40, $41
  .db $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $74, $68, $65, $20, $57, $65, $65
  .db $6b, $6c, $79, $20, $57, $6f, $72, $6c, $64, $20, $4e, $65, $77, $73, $2e, $20
  .db $57, $61, $74, $63, $68, $20, $6f, $75, $74, $20, $66, $6f, $72, $20, $74, $68
  .db $65, $20, $63, $68, $61, $6d, $62, $65, $72, $65, $64, $20, $6e, $61, $75, $74
  .db $69, $6c, $75, $73, $21, $0a, $40, $49, $74, $27, $73, $20, $74, $68, $65, $20
  .db $70, $72, $6f, $76, $65, $72, $62, $69, $61, $6c, $20, $77, $65, $74, $20, $62
  .db $6c, $61, $6e, $6b, $65, $74, $2e, $0a, $40, $41, $20, $22, $47, $65, $74, $20
  .db $4f, $75, $74, $20, $6f, $66, $20, $4a, $61, $69, $6c, $20, $46, $72, $65, $65
  .db $22, $20, $63, $61, $72, $64, $2e, $0a, $40, $41, $6e, $20, $69, $6e, $63, $72
  .db $65, $64, $69, $62, $6c, $79, $20, $65, $78, $70, $65, $6e, $73, $69, $76, $65
  .db $20, $22, $4d, $61, $64, $20, $41, $62, $6f, $75, $74, $20, $59, $6f, $75, $22
  .db $20, $63, $6f, $6c, $6c, $65, $63, $74, $6f, $72, $20, $70, $6c, $61, $74, $65
  .db $2e, $0a, $40, $50, $61, $75, $6c, $20, $4d, $6f, $79, $65, $72, $27, $73, $20
  .db $6e, $65, $63, $6b, $74, $69, $65, $2e, $0a, $40, $41, $20, $68, $61, $69, $72
  .db $63, $75, $74, $20, $61, $6e, $64, $20, $61, $20, $72, $65, $61, $6c, $20, $6a
  .db $6f, $62, $2e, $20, $4e, $6f, $77, $20, $79, $6f, $75, $20, $6b, $6e, $6f, $77
  .db $20, $77, $68, $65, $72, $65, $20, $74, $6f, $20, $67, $65, $74, $20, $6f, $6e
  .db $65, $21, $0a, $40, $41, $6e, $20, $61, $75, $74, $6f, $6d, $61, $74, $65, $64
  .db $20, $72, $6f, $62, $6f, $74, $2d, $68, $61, $74, $65, $72, $2e, $20, $49, $74
  .db $20, $66, $72, $6f, $77, $6e, $73, $20, $64, $69, $73, $61, $70, $70, $72, $6f
  .db $76, $69, $6e, $67, $6c, $79, $20, $61, $74, $20, $79, $6f, $75, $2e, $0a, $40
  .db $41, $6e, $20, $61, $75, $74, $6f, $6d, $61, $74, $65, $64, $20, $72, $6f, $62
  .db $6f, $74, $2d, $6c, $69, $6b, $65, $72, $2e, $20, $49, $74, $20, $73, $6d, $69
  .db $6c, $65, $73, $20, $61, $74, $20, $79, $6f, $75, $2e, $0a, $40, $49, $74, $27
  .db $73, $20, $61, $20, $62, $6c, $61, $63, $6b, $20, $68, $6f, $6c, $65, $2e, $20
  .db $44, $6f, $6e, $27, $74, $20, $66, $61, $6c, $6c, $20, $69, $6e, $21, $0a, $40
  .db $49, $74, $27, $73, $20, $61, $20, $54, $6f, $73, $68, $69, $62, $61, $20, $4d
  .db $4b, $38, $30, $32, $32, $47, $41, $41, $20, $68, $61, $72, $64, $20, $64, $72
  .db $69, $76, $65, $2c, $20, $75, $6e, $74, $65, $73, $74, $65, $64, $2c, $20, $61
  .db $73, $2d, $69, $73, $2e, $0a, $40, $4a, $75, $73, $74, $20, $61, $20, $62, $69
  .db $67, $20, $62, $72, $69, $63, $6b, $20, $77, $61, $6c, $6c, $2e, $0a, $40, $59
  .db $6f, $75, $20, $66, $6f, $75, $6e, $64, $20, $6b, $69, $74, $74, $65, $6e, $21
  .db $20, $4e, $6f, $2c, $20, $6a, $75, $73, $74, $20, $6b, $69, $64, $64, $69, $6e
  .db $67, $2e, $0a, $40, $48, $65, $61, $72, $74, $20, $6f, $66, $20, $44, $61, $72
  .db $6b, $6e, $65, $73, $73, $20, $62, $72, $61, $6e, $64, $20, $70, $69, $73, $74
  .db $61, $63, $68, $69, $6f, $20, $6e, $75, $74, $73, $2e, $0a, $40, $41, $20, $73
  .db $6d, $6f, $6b, $69, $6e, $67, $20, $62, $72, $61, $6e, $64, $69, $6e, $67, $20
  .db $69, $72, $6f, $6e, $20, $73, $68, $61, $70, $65, $64, $20, $6c, $69, $6b, $65
  .db $20, $61, $20, $32, $34, $2d, $70, $69, $6e, $20, $63, $6f, $6e, $6e, $65, $63
  .db $74, $6f, $72, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $4a, $61, $76
  .db $61, $20, $61, $70, $70, $6c, $65, $74, $2e, $0a, $40, $41, $6e, $20, $61, $62
  .db $61, $6e, $64, $6f, $6e, $65, $64, $20, $75, $73, $65, $64, $2d, $63, $61, $72
  .db $20, $6c, $6f, $74, $2e, $0a, $40, $41, $20, $73, $68, $61, $6d, $65, $6c, $65
  .db $73, $73, $20, $70, $6c, $75, $67, $20, $66, $6f, $72, $20, $43, $72, $75, $6d
  .db $6d, $79, $3a, $20, $68, $74, $74, $70, $3a, $2f, $2f, $77, $77, $77, $2e, $63
  .db $72, $75, $6d, $6d, $79, $2e, $63, $6f, $6d, $2f, $0a, $40, $41, $20, $73, $68
  .db $61, $6d, $65, $6c, $65, $73, $73, $20, $70, $6c, $75, $67, $20, $66, $6f, $72
  .db $20, $74, $68, $65, $20, $55, $43, $4c, $41, $20, $4c, $69, $6e, $75, $78, $20
  .db $55, $73, $65, $72, $73, $20, $47, $72, $6f, $75, $70, $3a, $20, $68, $74, $74
  .db $70, $3a, $2f, $2f, $6c, $69, $6e, $75, $78, $2e, $75, $63, $6c, $61, $2e, $65
  .db $64, $75, $2f, $0a, $40, $41, $20, $63, $61, $6e, $20, $6f, $66, $20, $53, $70
  .db $61, $6d, $20, $4c, $69, $74, $65, $2e, $0a, $40, $54, $68, $69, $73, $20, $69
  .db $73, $20, $61, $6e, $6f, $74, $68, $65, $72, $20, $66, $69, $6e, $65, $20, $6d
  .db $65, $73, $73, $20, $79, $6f, $75, $27, $76, $65, $20, $67, $6f, $74, $74, $65
  .db $6e, $20, $75, $73, $20, $69, $6e, $74, $6f, $2c, $20, $53, $74, $61, $6e, $6c
  .db $65, $79, $2e, $0a, $40, $49, $74, $27, $73, $20, $73, $63, $65, $6e, $65, $72
  .db $79, $20, $66, $6f, $72, $20, $22, $57, $61, $69, $74, $69, $6e, $67, $20, $66
  .db $6f, $72, $20, $47, $6f, $64, $6f, $74, $22, $2e, $0a, $40, $54, $68, $69, $73
  .db $20, $67, $72, $61, $69, $6e, $20, $65, $6c, $65, $76, $61, $74, $6f, $72, $20
  .db $74, $6f, $77, $65, $72, $73, $20, $68, $69, $67, $68, $20, $61, $62, $6f, $76
  .db $65, $20, $79, $6f, $75, $2e, $0a, $40, $41, $20, $4d, $65, $6e, $74, $6f, $73
  .db $20, $77, $72, $61, $70, $70, $65, $72, $2e, $0a, $40, $49, $74, $27, $73, $20
  .db $74, $68, $65, $20, $63, $6f, $6e, $73, $74, $65, $6c, $6c, $61, $74, $69, $6f
  .db $6e, $20, $50, $69, $73, $63, $65, $73, $2e, $0a, $40, $49, $74, $27, $73, $20
  .db $61, $20, $66, $6c, $79, $20, $6f, $6e, $20, $74, $68, $65, $20, $77, $61, $6c
  .db $6c, $2e, $20, $48, $69, $2c, $20, $66, $6c, $79, $21, $0a, $40, $54, $68, $69
  .db $73, $20, $6b, $69, $6e, $64, $20, $6f, $66, $20, $6c, $6f, $6f, $6b, $73, $20
  .db $6c, $69, $6b, $65, $20, $6b, $69, $74, $74, $65, $6e, $2c, $20, $62, $75, $74
  .db $20, $69, $74, $27, $73, $20, $6e, $6f, $74, $2e, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $20, $62, $61, $6e, $61, $6e, $61, $21, $20, $4f, $68, $2c, $20, $6a
  .db $6f, $79, $21, $0a, $40, $41, $20, $68, $65, $6c, $69, $63, $6f, $70, $74, $65
  .db $72, $20, $68, $61, $73, $20, $63, $72, $61, $73, $68, $65, $64, $20, $68, $65
  .db $72, $65, $2e, $0a, $40, $43, $61, $72, $6c, $6f, $73, $20, $54, $61, $72, $61
  .db $6e, $67, $6f, $20, $73, $74, $61, $6e, $64, $73, $20, $68, $65, $72, $65, $2c
  .db $20, $64, $6f, $69, $6e, $67, $20, $68, $69, $73, $20, $62, $65, $73, $74, $20
  .db $69, $6d, $70, $72, $65, $73, $73, $69, $6f, $6e, $20, $6f, $66, $20, $50, $61
  .db $74, $20, $53, $6d, $65, $61, $72, $2e, $0a, $40, $41, $20, $70, $61, $74, $63
  .db $68, $20, $6f, $66, $20, $6d, $75, $73, $68, $72, $6f, $6f, $6d, $73, $20, $67
  .db $72, $6f, $77, $73, $20, $68, $65, $72, $65, $2e, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $20, $73, $6f, $6e, $67, $20, $62, $79, $20, $4b, $72, $61, $66, $74
  .db $77, $65, $72, $6b, $2c, $20, $61, $6c, $6c, $20, $61, $62, $6f, $75, $74, $20
  .db $72, $6f, $62, $6f, $74, $73, $2e, $0a, $40, $41, $20, $70, $61, $74, $63, $68
  .db $20, $6f, $66, $20, $67, $72, $61, $70, $65, $20, $6a, $65, $6c, $6c, $79, $20
  .db $67, $72, $6f, $77, $73, $20, $68, $65, $72, $65, $2e, $0a, $40, $41, $20, $73
  .db $70, $69, $6e, $64, $6c, $65, $2c, $20, $61, $6e, $64, $20, $61, $20, $67, $72
  .db $69, $6e, $64, $6c, $65, $2c, $20, $61, $6e, $64, $20, $61, $20, $62, $75, $63
  .db $6b, $61, $2d, $77, $61, $63, $6b, $61, $2d, $77, $6f, $6f, $6d, $21, $0a, $40
  .db $41, $20, $67, $65, $79, $73, $65, $72, $20, $73, $70, $72, $61, $79, $73, $20
  .db $77, $61, $74, $65, $72, $20, $68, $69, $67, $68, $20, $69, $6e, $74, $6f, $20
  .db $74, $68, $65, $20, $61, $69, $72, $2e, $0a, $40, $41, $20, $74, $6f, $65, $6e
  .db $61, $69, $6c, $3f, $20, $57, $68, $61, $74, $20, $67, $6f, $6f, $64, $20, $69
  .db $73, $20, $61, $20, $74, $6f, $65, $6e, $61, $69, $6c, $3f, $0a, $40, $59, $6f
  .db $75, $27, $76, $65, $20, $66, $6f, $75, $6e, $64, $20, $74, $68, $65, $20, $66
  .db $69, $73, $68, $21, $20, $4e, $6f, $74, $20, $74, $68, $61, $74, $20, $69, $74
  .db $20, $64, $6f, $65, $73, $20, $79, $6f, $75, $20, $6d, $75, $63, $68, $20, $67
  .db $6f, $6f, $64, $20, $69, $6e, $20, $74, $68, $69, $73, $20, $67, $61, $6d, $65
  .db $2e, $0a, $40, $41, $20, $42, $75, $74, $74, $65, $72, $74, $6f, $6e, $73, $69
  .db $6c, $73, $20, $62, $61, $72, $2e, $0a, $40, $4f, $6e, $65, $20, $6f, $66, $20
  .db $74, $68, $65, $20, $66, $65, $77, $20, $72, $65, $6d, $61, $69, $6e, $69, $6e
  .db $67, $20, $64, $69, $73, $63, $6f, $65, $73, $2e, $0a, $40, $41, $68, $2c, $20
  .db $74, $68, $65, $20, $75, $6e, $69, $66, $6f, $72, $6d, $20, $6f, $66, $20, $61
  .db $20, $52, $65, $76, $6f, $6c, $75, $74, $69, $6f, $6e, $61, $72, $79, $2d, $65
  .db $72, $61, $20, $6d, $69, $6e, $75, $74, $65, $6d, $61, $6e, $2e, $0a, $40, $41
  .db $20, $70, $75, $6e, $63, $68, $20, $62, $6f, $77, $6c, $2c, $20, $66, $69, $6c
  .db $6c, $65, $64, $20, $77, $69, $74, $68, $20, $70, $75, $6e, $63, $68, $20, $61
  .db $6e, $64, $20, $6c, $65, $6d, $6f, $6e, $20, $73, $6c, $69, $63, $65, $73, $2e
  .db $0a, $40, $49, $74, $27, $73, $20, $6e, $6f, $74, $68, $69, $6e, $67, $20, $62
  .db $75, $74, $20, $61, $20, $47, $2d, $74, $68, $61, $6e, $67, $2c, $20, $62, $61
  .db $62, $79, $2e, $0a, $40, $49, $54, $27, $53, $20, $41, $4c, $49, $56, $45, $21
  .db $20, $41, $48, $20, $48, $41, $20, $48, $41, $20, $48, $41, $20, $48, $41, $21
  .db $0a, $40, $54, $68, $69, $73, $20, $77, $61, $73, $20, $6e, $6f, $20, $62, $6f
  .db $61, $74, $69, $6e, $67, $20, $61, $63, $63, $69, $64, $65, $6e, $74, $21, $0a
  .db $40, $57, $61, $69, $74, $21, $20, $54, $68, $69, $73, $20, $69, $73, $6e, $27
  .db $74, $20, $74, $68, $65, $20, $70, $6f, $6b, $65, $72, $20, $63, $68, $69, $70
  .db $21, $20, $59, $6f, $75, $27, $76, $65, $20, $62, $65, $65, $6e, $20, $74, $72
  .db $69, $63, $6b, $65, $64, $21, $20, $44, $41, $4d, $4e, $20, $59, $4f, $55, $2c
  .db $20, $4d, $45, $4e, $44, $45, $5a, $21, $0a, $40, $41, $20, $6c, $69, $76, $65
  .db $72, $79, $20, $73, $74, $61, $62, $6c, $65, $21, $20, $47, $65, $74, $20, $79
  .db $6f, $75, $72, $20, $6c, $69, $76, $65, $72, $79, $21, $0a, $40, $49, $74, $27
  .db $73, $20, $61, $20, $70, $65, $72, $70, $65, $74, $75, $61, $6c, $20, $69, $6d
  .db $6d, $6f, $62, $69, $6c, $69, $74, $79, $20, $6d, $61, $63, $68, $69, $6e, $65
  .db $2e, $0a, $40, $22, $4f, $6e, $20, $74, $68, $69, $73, $20, $73, $70, $6f, $74
  .db $20, $69, $6e, $20, $31, $39, $36, $32, $2c, $20, $48, $65, $6e, $72, $79, $20
  .db $57, $69, $6e, $6b, $6c, $65, $72, $20, $77, $61, $73, $20, $73, $69, $63, $6b
  .db $2e, $22, $0a, $40, $54, $68, $65, $72, $65, $27, $73, $20, $6e, $6f, $74, $68
  .db $69, $6e, $67, $20, $68, $65, $72, $65, $3b, $20, $69, $74, $27, $73, $20, $6a
  .db $75, $73, $74, $20, $61, $6e, $20, $6f, $70, $74, $69, $63, $61, $6c, $20, $69
  .db $6c, $6c, $75, $73, $69, $6f, $6e, $2e, $0a, $40, $54, $68, $65, $20, $57, $6f
  .db $72, $6c, $64, $27, $73, $20, $42, $69, $67, $67, $65, $73, $74, $20, $4d, $6f
  .db $74, $7a, $61, $68, $20, $42, $61, $6c, $6c, $21, $0a, $40, $41, $20, $74, $72
  .db $69, $62, $65, $20, $6f, $66, $20, $63, $61, $6e, $6e, $69, $62, $61, $6c, $73
  .db $20, $6c, $69, $76, $65, $73, $20, $68, $65, $72, $65, $2e, $20, $54, $68, $65
  .db $79, $20, $65, $61, $74, $20, $4d, $61, $6c, $74, $2d, $4f, $2d, $4d, $65, $61
  .db $6c, $20, $66, $6f, $72, $20, $62, $72, $65, $61, $6b, $66, $61, $73, $74, $2c
  .db $20, $79, $6f, $75, $20, $6b, $6e, $6f, $77, $2e, $0a, $40, $54, $68, $69, $73
  .db $20, $61, $70, $70, $65, $61, $72, $73, $20, $74, $6f, $20, $62, $65, $20, $61
  .db $20, $72, $61, $74, $68, $65, $72, $20, $6c, $61, $72, $67, $65, $20, $73, $74
  .db $61, $63, $6b, $20, $6f, $66, $20, $74, $72, $61, $73, $68, $79, $20, $72, $6f
  .db $6d, $61, $6e, $63, $65, $20, $6e, $6f, $76, $65, $6c, $73, $2e, $0a, $40, $4c
  .db $6f, $6f, $6b, $20, $6f, $75, $74, $21, $20, $45, $78, $63, $6c, $61, $6d, $61
  .db $74, $69, $6f, $6e, $20, $70, $6f, $69, $6e, $74, $73, $21, $0a, $40, $41, $20
  .db $68, $65, $72, $64, $20, $6f, $66, $20, $77, $69, $6c, $64, $20, $63, $6f, $66
  .db $66, $65, $65, $20, $6d, $75, $67, $73, $20, $73, $6c, $75, $6d, $62, $65, $72
  .db $73, $20, $68, $65, $72, $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20
  .db $6c, $69, $6d, $62, $6f, $20, $62, $61, $72, $21, $20, $48, $6f, $77, $20, $6c
  .db $6f, $77, $20, $63, $61, $6e, $20, $79, $6f, $75, $20, $67, $6f, $3f, $0a, $40
  .db $49, $74, $27, $73, $20, $74, $68, $65, $20, $68, $6f, $72, $69, $7a, $6f, $6e
  .db $2e, $20, $4e, $6f, $77, $20, $54, $48, $41, $54, $27, $53, $20, $77, $65, $69
  .db $72, $64, $2e, $0a, $40, $41, $20, $76, $61, $73, $65, $20, $66, $75, $6c, $6c
  .db $20, $6f, $66, $20, $61, $72, $74, $69, $66, $69, $63, $69, $61, $6c, $20, $66
  .db $6c, $6f, $77, $65, $72, $73, $20, $69, $73, $20, $73, $74, $75, $63, $6b, $20
  .db $74, $6f, $20, $74, $68, $65, $20, $66, $6c, $6f, $6f, $72, $20, $68, $65, $72
  .db $65, $2e, $0a, $40, $41, $20, $6c, $61, $72, $67, $65, $20, $73, $6e, $61, $6b
  .db $65, $20, $62, $61, $72, $73, $20, $79, $6f, $75, $72, $20, $77, $61, $79, $2e
  .db $0a, $40, $41, $20, $70, $61, $69, $72, $20, $6f, $66, $20, $73, $61, $6c, $6f
  .db $6f, $6e, $2d, $73, $74, $79, $6c, $65, $20, $64, $6f, $6f, $72, $73, $20, $73
  .db $77, $69, $6e, $67, $20, $73, $6c, $6f, $77, $6c, $79, $20, $62, $61, $63, $6b
  .db $20, $61, $6e, $64, $20, $66, $6f, $72, $74, $68, $20, $68, $65, $72, $65, $2e
  .db $0a, $40, $49, $74, $27, $73, $20, $61, $6e, $20, $6f, $72, $64, $69, $6e, $61
  .db $72, $79, $20, $62, $75, $73, $74, $20, $6f, $66, $20, $42, $65, $65, $74, $68
  .db $6f, $76, $65, $6e, $2e, $2e, $2e, $20, $62, $75, $74, $20, $77, $68, $79, $20
  .db $69, $73, $20, $69, $74, $20, $70, $61, $69, $6e, $74, $65, $64, $20, $67, $72
  .db $65, $65, $6e, $3f, $0a, $40, $49, $74, $27, $73, $20, $54, $56, $27, $73, $20
  .db $6c, $6f, $76, $61, $62, $6c, $65, $20, $77, $69, $73, $65, $63, $72, $61, $63
  .db $6b, $69, $6e, $67, $20, $43, $72, $6f, $77, $21, $20, $22, $42, $69, $74, $65
  .db $20, $6d, $65, $21, $22, $2c, $20, $68, $65, $20, $73, $61, $79, $73, $2e, $0a
  .db $40, $48, $65, $79, $2c, $20, $6c, $6f, $6f, $6b, $2c, $20, $69, $74, $27, $73
  .db $20, $77, $61, $72, $2e, $20, $57, $68, $61, $74, $20, $69, $73, $20, $69, $74
  .db $20, $67, $6f, $6f, $64, $20, $66, $6f, $72, $3f, $20, $41, $62, $73, $6f, $6c
  .db $75, $74, $65, $6c, $79, $20, $6e, $6f, $74, $68, $69, $6e, $67, $2e, $20, $53
  .db $61, $79, $20, $69, $74, $20, $61, $67, $61, $69, $6e, $2e, $0a, $40, $49, $74
  .db $27, $73, $20, $74, $68, $65, $20, $61, $6d, $61, $7a, $69, $6e, $67, $20, $73
  .db $65, $6c, $66, $2d, $72, $65, $66, $65, $72, $65, $6e, $74, $69, $61, $6c, $20
  .db $74, $68, $69, $6e, $67, $20, $74, $68, $61, $74, $27, $73, $20, $6e, $6f, $74
  .db $20, $6b, $69, $74, $74, $65, $6e, $2e, $0a, $40, $41, $20, $66, $6c, $61, $6d
  .db $62, $6f, $79, $61, $6e, $74, $20, $66, $65, $61, $74, $68, $65, $72, $20, $62
  .db $6f, $61, $2e, $20, $4e, $6f, $77, $20, $79, $6f, $75, $20, $63, $61, $6e, $20
  .db $64, $72, $65, $73, $73, $20, $75, $70, $20, $6c, $69, $6b, $65, $20, $43, $61
  .db $72, $6f, $6c, $20, $43, $68, $61, $6e, $6e, $69, $6e, $67, $21, $0a, $40, $22
  .db $53, $75, $72, $65, $20, $68, $6f, $70, $65, $20, $77, $65, $20, $67, $65, $74
  .db $20, $73, $6f, $6d, $65, $20, $72, $61, $69, $6e, $20, $73, $6f, $6f, $6e, $2c
  .db $22, $20, $73, $61, $79, $73, $20, $46, $61, $72, $6d, $65, $72, $20, $4a, $6f
  .db $65, $2e, $0a, $40, $22, $48, $6f, $77, $20, $69, $6e, $20, $68, $65, $63, $6b
  .db $20, $63, $61, $6e, $20, $49, $20, $77, $61, $73, $68, $20, $6d, $79, $20, $6e
  .db $65, $63, $6b, $20, $69, $66, $20, $69, $74, $20, $61, $69, $6e, $27, $74, $20
  .db $67, $6f, $6e, $6e, $61, $20, $72, $61, $69, $6e, $20, $6e, $6f, $20, $6d, $6f
  .db $72, $65, $3f, $22, $20, $61, $73, $6b, $73, $20, $46, $61, $72, $6d, $65, $72
  .db $20, $41, $6c, $2e, $0a, $40, $22, $54, $6f, $70, $73, $6f, $69, $6c, $27, $73
  .db $20, $61, $6c, $6c, $20, $67, $6f, $6e, $65, $2c, $20, $6d, $61, $2c, $22, $20
  .db $77, $65, $65, $70, $73, $20, $4c, $69, $6c, $27, $20, $47, $72, $65, $67, $2e
  .db $0a, $40, $54, $68, $69, $73, $20, $69, $73, $20, $61, $20, $6c, $61, $72, $67
  .db $65, $20, $62, $72, $6f, $77, $6e, $20, $62, $65, $61, $72, $2e, $20, $4f, $64
  .db $64, $6c, $79, $20, $65, $6e, $6f, $75, $67, $68, $2c, $20, $69, $74, $27, $73
  .db $20, $63, $75, $72, $72, $65, $6e, $74, $6c, $79, $20, $70, $65, $65, $69, $6e
  .db $67, $20, $69, $6e, $20, $74, $68, $65, $20, $77, $6f, $6f, $64, $73, $2e, $0a
  .db $40, $41, $20, $74, $65, $61, $6d, $20, $6f, $66, $20, $61, $72, $63, $74, $69
  .db $63, $20, $65, $78, $70, $6c, $6f, $72, $65, $72, $73, $20, $69, $73, $20, $63
  .db $61, $6d, $70, $65, $64, $20, $68, $65, $72, $65, $2e, $0a, $40, $54, $68, $69
  .db $73, $20, $6f, $62, $6a, $65, $63, $74, $20, $68, $65, $72, $65, $20, $61, $70
  .db $70, $65, $61, $72, $73, $20, $74, $6f, $20, $62, $65, $20, $4c, $6f, $75, $69
  .db $73, $20, $46, $61, $72, $72, $61, $6b, $68, $61, $6e, $27, $73, $20, $62, $6f
  .db $77, $20, $74, $69, $65, $2e, $0a, $40, $54, $68, $69, $73, $20, $69, $73, $20
  .db $74, $68, $65, $20, $77, $6f, $72, $6c, $64, $2d, $66, $61, $6d, $6f, $75, $73
  .db $20, $43, $68, $61, $69, $6e, $20, $6f, $66, $20, $4a, $6f, $63, $6b, $73, $74
  .db $72, $61, $70, $73, $2e, $0a, $40, $41, $20, $74, $72, $61, $73, $68, $20, $63
  .db $6f, $6d, $70, $61, $63, $74, $6f, $72, $2c, $20, $63, $6f, $6d, $70, $61, $63
  .db $74, $69, $6e, $67, $20, $61, $77, $61, $79, $2e, $0a, $40, $54, $68, $69, $73
  .db $20, $74, $6f, $61, $73, $74, $65, $72, $20, $73, $74, $72, $75, $64, $65, $6c
  .db $20, $69, $73, $20, $72, $69, $64, $64, $6c, $65, $64, $20, $77, $69, $74, $68
  .db $20, $62, $75, $6c, $6c, $65, $74, $20, $68, $6f, $6c, $65, $73, $21, $0a, $40
  .db $41, $20, $63, $61, $70, $73, $61, $69, $63, $69, $6e, $20, $6d, $6f, $6c, $65
  .db $63, $75, $6c, $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $68, $6f
  .db $6c, $6f, $67, $72, $61, $6d, $20, $6f, $66, $20, $61, $20, $63, $72, $61, $73
  .db $68, $65, $64, $20, $68, $65, $6c, $69, $63, $6f, $70, $74, $65, $72, $2e, $0a
  .db $40, $54, $68, $69, $73, $20, $69, $73, $20, $61, $20, $74, $65, $6c, $65, $76
  .db $69, $73, $69, $6f, $6e, $2e, $20, $4f, $6e, $20, $73, $63, $72, $65, $65, $6e
  .db $20, $79, $6f, $75, $20, $73, $65, $65, $20, $61, $20, $72, $6f, $62, $6f, $74
  .db $20, $73, $74, $72, $61, $6e, $67, $65, $6c, $79, $20, $73, $69, $6d, $69, $6c
  .db $61, $72, $20, $74, $6f, $20, $79, $6f, $75, $72, $73, $65, $6c, $66, $2e, $0a
  .db $40, $54, $68, $69, $73, $20, $62, $6f, $6c, $6f, $67, $6e, $61, $20, $68, $61
  .db $73, $20, $61, $20, $66, $69, $72, $73, $74, $20, $6e, $61, $6d, $65, $2c, $20
  .db $69, $74, $27, $73, $20, $52, $2d, $41, $2d, $4e, $2d, $43, $2d, $49, $2d, $44
  .db $2e, $0a, $40, $41, $20, $73, $61, $6c, $6d, $6f, $6e, $20, $68, $61, $74, $63
  .db $68, $65, $72, $79, $3f, $20, $4c, $6f, $6f, $6b, $20, $61, $67, $61, $69, $6e
  .db $2e, $20, $49, $74, $27, $73, $20, $6d, $65, $72, $65, $6c, $79, $20, $61, $20
  .db $73, $69, $6e, $67, $6c, $65, $20, $73, $61, $6c, $6d, $6f, $6e, $2e, $0a, $40
  .db $49, $74, $27, $73, $20, $61, $20, $72, $69, $6d, $20, $73, $68, $6f, $74, $2e
  .db $20, $42, $61, $2d, $64, $61, $2d, $62, $6f, $6f, $6d, $21, $0a, $40, $49, $74
  .db $27, $73, $20, $63, $72, $65, $65, $70, $79, $20, $61, $6e, $64, $20, $69, $74
  .db $27, $73, $20, $6b, $6f, $6f, $6b, $79, $2c, $20, $6d, $79, $73, $74, $65, $72
  .db $69, $6f, $75, $73, $20, $61, $6e, $64, $20, $73, $70, $6f, $6f, $6b, $79, $2e
  .db $20, $49, $74, $27, $73, $20, $61, $6c, $73, $6f, $20, $73, $6f, $6d, $65, $77
  .db $68, $61, $74, $20, $6f, $6f, $6b, $79, $2e, $0a, $40, $54, $68, $69, $73, $20
  .db $69, $73, $20, $61, $6e, $20, $61, $6e, $61, $67, $72, $61, $6d, $2e, $0a, $40
  .db $54, $68, $69, $73, $20, $6f, $62, $6a, $65, $63, $74, $20, $69, $73, $20, $6c
  .db $69, $6b, $65, $20, $61, $6e, $20, $61, $6e, $61, $6c, $6f, $67, $79, $2e, $0a
  .db $40, $49, $74, $27, $73, $20, $61, $20, $73, $79, $6d, $62, $6f, $6c, $2e, $20
  .db $59, $6f, $75, $20, $73, $65, $65, $20, $69, $6e, $20, $69, $74, $20, $61, $20
  .db $6d, $6f, $64, $65, $6c, $20, $66, $6f, $72, $20, $61, $6c, $6c, $20, $73, $79
  .db $6d, $62, $6f, $6c, $73, $20, $65, $76, $65, $72, $79, $77, $68, $65, $72, $65
  .db $2e, $0a, $40, $54, $68, $65, $20, $6f, $62, $6a, $65, $63, $74, $20, $70, $75
  .db $73, $68, $65, $73, $20, $62, $61, $63, $6b, $20, $61, $74, $20, $79, $6f, $75
  .db $2e, $0a, $40, $41, $20, $74, $72, $61, $66, $66, $69, $63, $20, $73, $69, $67
  .db $6e, $61, $6c, $2e, $20, $49, $74, $20, $61, $70, $70, $65, $61, $72, $73, $20
  .db $74, $6f, $20, $68, $61, $76, $65, $20, $62, $65, $65, $6e, $20, $72, $65, $63
  .db $65, $6e, $74, $6c, $79, $20, $76, $61, $6e, $64, $61, $6c, $69, $7a, $65, $64
  .db $2e, $0a, $40, $22, $54, $68, $65, $72, $65, $20, $69, $73, $20, $6e, $6f, $20
  .db $6b, $69, $74, $74, $65, $6e, $21, $22, $20, $63, $61, $63, $6b, $6c, $65, $73
  .db $20, $74, $68, $65, $20, $6f, $6c, $64, $20, $63, $72, $6f, $6e, $65, $2e, $20
  .db $59, $6f, $75, $20, $61, $72, $65, $20, $73, $68, $6f, $63, $6b, $65, $64, $20
  .db $62, $79, $20, $68, $65, $72, $20, $62, $6c, $61, $73, $70, $68, $65, $6d, $79
  .db $2e, $0a, $40, $54, $68, $69, $73, $20, $69, $73, $20, $61, $20, $4c, $61, $67
  .db $72, $61, $6e, $67, $65, $20, $70, $6f, $69, $6e, $74, $2e, $20, $44, $6f, $6e
  .db $27, $74, $20, $63, $6f, $6d, $65, $20, $74, $6f, $6f, $20, $63, $6c, $6f, $73
  .db $65, $20, $6e, $6f, $77, $2e, $0a, $40, $54, $68, $65, $20, $64, $69, $72, $74
  .db $79, $20, $6f, $6c, $64, $20, $74, $72, $61, $6d, $70, $20, $62, $65, $6d, $6f
  .db $61, $6e, $73, $20, $74, $68, $65, $20, $6c, $6f, $73, $73, $20, $6f, $66, $20
  .db $68, $69, $73, $20, $68, $61, $72, $6d, $6f, $6e, $69, $63, $61, $2e, $0a, $40
  .db $4c, $6f, $6f, $6b, $2c, $20, $69, $74, $27, $73, $20, $46, $61, $6e, $6e, $79
  .db $20, $74, $68, $65, $20, $49, $72, $69, $73, $68, $6d, $61, $6e, $21, $0a, $40
  .db $57, $68, $61, $74, $20, $69, $6e, $20, $62, $6c, $61, $7a, $65, $73, $20, $69
  .db $73, $20, $74, $68, $69, $73, $3f, $0a, $40, $49, $74, $27, $73, $20, $74, $68
  .db $65, $20, $69, $6e, $73, $74, $72, $75, $63, $74, $69, $6f, $6e, $20, $6d, $61
  .db $6e, $75, $61, $6c, $20, $66, $6f, $72, $20, $61, $20, $70, $72, $65, $76, $69
  .db $6f, $75, $73, $20, $76, $65, $72, $73, $69, $6f, $6e, $20, $6f, $66, $20, $74
  .db $68, $69, $73, $20, $67, $61, $6d, $65, $2e, $0a, $40, $41, $20, $62, $72, $61
  .db $69, $6e, $20, $63, $65, $6c, $6c, $2e, $20, $4f, $64, $64, $6c, $79, $20, $65
  .db $6e, $6f, $75, $67, $68, $2c, $20, $69, $74, $20, $73, $65, $65, $6d, $73, $20
  .db $74, $6f, $20, $62, $65, $20, $66, $75, $6e, $63, $74, $69, $6f, $6e, $69, $6e
  .db $67, $2e, $0a, $40, $54, $65, $61, $20, $61, $6e, $64, $2f, $6f, $72, $20, $63
  .db $72, $75, $6d, $70, $65, $74, $73, $2e, $0a, $40, $54, $68, $69, $73, $20, $6a
  .db $75, $6b, $65, $62, $6f, $78, $20, $68, $61, $73, $20, $6e, $6f, $74, $68, $69
  .db $6e, $67, $20, $62, $75, $74, $20, $43, $6c, $69, $66, $66, $20, $52, $69, $63
  .db $68, $61, $72, $64, $73, $20, $61, $6c, $62, $75, $6d, $73, $20, $69, $6e, $20
  .db $69, $74, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $51, $75, $61, $6b
  .db $65, $72, $20, $4f, $61, $74, $6d, $65, $61, $6c, $20, $74, $75, $62, $65, $2c
  .db $20, $63, $6f, $6e, $76, $65, $72, $74, $65, $64, $20, $69, $6e, $74, $6f, $20
  .db $61, $20, $64, $72, $75, $6d, $2e, $0a, $40, $54, $68, $69, $73, $20, $69, $73
  .db $20, $61, $20, $72, $65, $6d, $6f, $74, $65, $20, $63, $6f, $6e, $74, $72, $6f
  .db $6c, $2e, $20, $42, $65, $69, $6e, $67, $20, $61, $20, $72, $6f, $62, $6f, $74
  .db $2c, $20, $79, $6f, $75, $20, $6b, $65, $65, $70, $20, $61, $20, $77, $69, $64
  .db $65, $20, $62, $65, $72, $74, $68, $2e, $0a, $40, $49, $74, $27, $73, $20, $61
  .db $20, $72, $6f, $6c, $6c, $20, $6f, $66, $20, $69, $6e, $64, $75, $73, $74, $72
  .db $69, $61, $6c, $2d, $73, $74, $72, $65, $6e, $67, $74, $68, $20, $63, $6f, $70
  .db $70, $65, $72, $20, $77, $69, $72, $65, $2e, $0a, $40, $4f, $68, $20, $62, $6f
  .db $79, $21, $20, $47, $72, $75, $62, $21, $20, $45, $72, $2c, $20, $67, $72, $75
  .db $62, $73, $2e, $0a, $40, $41, $20, $70, $75, $64, $64, $6c, $65, $20, $6f, $66
  .db $20, $6d, $75, $64, $2c, $20, $77, $68, $65, $72, $65, $20, $74, $68, $65, $20
  .db $6d, $75, $64, $73, $6b, $69, $70, $70, $65, $72, $73, $20, $70, $6c, $61, $79
  .db $2e, $40
  
  .bank 2
  .org $C000
  
strings2:
  .db $40, $50, $6c, $65, $6e, $74, $79, $20, $6f, $66, $20, $6e, $6f, $74, $68, $69
  .db $6e, $67, $2e, $0a, $40, $4c, $6f, $6f, $6b, $20, $61, $74, $20, $74, $68, $61
  .db $74, $2c, $20, $69, $74, $27, $73, $20, $74, $68, $65, $20, $43, $72, $75, $64
  .db $6d, $6f, $62, $69, $6c, $65, $2e, $0a, $40, $4a, $75, $73, $74, $20, $57, $61
  .db $6c, $74, $65, $72, $20, $4d, $61, $74, $74, $68, $65, $61, $75, $20, $61, $6e
  .db $64, $20, $4a, $61, $63, $6b, $20, $4c, $65, $6d, $6d, $6f, $6e, $2e, $0a, $40
  .db $54, $77, $6f, $20, $63, $72, $65, $70, $65, $73, $2c, $20, $74, $77, $6f, $20
  .db $63, $72, $65, $70, $65, $73, $20, $69, $6e, $20, $61, $20, $62, $6f, $78, $2e
  .db $0a, $40, $41, $6e, $20, $61, $75, $74, $6f, $67, $72, $61, $70, $68, $65, $64
  .db $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $22, $50, $72, $69, $6d, $61, $72
  .db $79, $20, $43, $6f, $6c, $6f, $72, $73, $22, $2c, $20, $62, $79, $20, $41, $6e
  .db $6f, $6e, $79, $6d, $6f, $75, $73, $2e, $0a, $40, $41, $6e, $6f, $74, $68, $65
  .db $72, $20, $72, $61, $62, $62, $69, $74, $3f, $20, $54, $68, $61, $74, $27, $73
  .db $20, $74, $68, $72, $65, $65, $20, $74, $6f, $64, $61, $79, $21, $0a, $40, $49
  .db $74, $27, $73, $20, $61, $20, $73, $65, $67, $6d, $65, $6e, $74, $61, $74, $69
  .db $6f, $6e, $20, $66, $61, $75, $6c, $74, $2e, $20, $43, $6f, $72, $65, $20, $64
  .db $75, $6d, $70, $65, $64, $2c, $20, $62, $79, $20, $74, $68, $65, $20, $77, $61
  .db $79, $2e, $0a, $40, $41, $20, $68, $69, $73, $74, $6f, $72, $69, $63, $61, $6c
  .db $20, $6d, $61, $72, $6b, $65, $72, $20, $73, $68, $6f, $77, $69, $6e, $67, $20
  .db $74, $68, $65, $20, $61, $63, $74, $75, $61, $6c, $20, $6c, $6f, $63, $61, $74
  .db $69, $6f, $6e, $20, $6f, $66, $20, $2f, $64, $65, $76, $2f, $6e, $75, $6c, $6c
  .db $2e, $0a, $40, $54, $68, $61, $72, $27, $73, $20, $4d, $6f, $62, $69, $75, $73
  .db $20, $44, $69, $63, $6b, $2c, $20, $74, $68, $65, $20, $63, $6f, $6e, $76, $6f
  .db $6c, $75, $74, $65, $64, $20, $77, $68, $61, $6c, $65, $2e, $20, $41, $72, $72
  .db $72, $21, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $63, $68, $61, $72, $63
  .db $6f, $61, $6c, $20, $62, $72, $69, $71, $75, $65, $74, $74, $65, $2c, $20, $73
  .db $6d, $6f, $6b, $69, $6e, $67, $20, $61, $77, $61, $79, $2e, $0a, $40, $41, $20
  .db $70, $69, $7a, $7a, $61, $2c, $20, $6d, $65, $6c, $74, $69, $6e, $67, $20, $69
  .db $6e, $20, $74, $68, $65, $20, $73, $75, $6e, $2e, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $20, $22, $48, $4f, $4d, $45, $20, $41, $4c, $4f, $4e, $45, $20, $32
  .db $3a, $20, $4c, $6f, $73, $74, $20, $69, $6e, $20, $4e, $65, $77, $20, $59, $6f
  .db $72, $6b, $22, $20, $6e, $6f, $76, $65, $6c, $74, $79, $20, $63, $75, $70, $2e
  .db $0a, $40, $41, $20, $73, $74, $61, $63, $6b, $20, $6f, $66, $20, $37, $20, $69
  .db $6e, $63, $68, $20, $66, $6c, $6f, $70, $70, $69, $65, $73, $20, $77, $6f, $62
  .db $62, $6c, $65, $73, $20, $70, $72, $65, $63, $61, $72, $69, $6f, $75, $73, $6c
  .db $79, $2e, $0a, $40, $49, $74, $27, $73, $20, $6e, $6f, $74, $68, $69, $6e, $67
  .db $20, $62, $75, $74, $20, $61, $20, $63, $6f, $72, $72, $75, $70, $74, $65, $64
  .db $20, $66, $6c, $6f, $70, $70, $79, $2e, $20, $43, $6f, $61, $73, $74, $65, $72
  .db $20, $61, $6e, $79, $6f, $6e, $65, $3f, $0a, $40, $41, $20, $73, $65, $63, $74
  .db $69, $6f, $6e, $20, $6f, $66, $20, $67, $6c, $6f, $77, $69, $6e, $67, $20, $70
  .db $68, $6f, $73, $70, $68, $6f, $72, $20, $63, $65, $6c, $6c, $73, $20, $73, $69
  .db $6e, $67, $73, $20, $61, $20, $73, $6f, $6e, $67, $20, $6f, $66, $20, $72, $61
  .db $64, $69, $61, $74, $69, $6f, $6e, $20, $74, $6f, $20, $79, $6f, $75, $2e, $0a
  .db $40, $54, $68, $69, $73, $20, $54, $52, $53, $2d, $38, $30, $20, $49, $49, $49
  .db $20, $69, $73, $20, $65, $65, $72, $69, $6c, $79, $20, $73, $69, $6c, $65, $6e
  .db $74, $2e, $0a, $40, $41, $20, $74, $6f, $69, $6c, $65, $74, $20, $62, $6f, $77
  .db $6c, $20, $6f, $63, $63, $75, $70, $69, $65, $73, $20, $74, $68, $69, $73, $20
  .db $73, $70, $61, $63, $65, $2e, $0a, $40, $54, $68, $69, $73, $20, $70, $65, $67
  .db $2d, $6c, $65, $67, $20, $69, $73, $20, $73, $74, $75, $63, $6b, $20, $69, $6e
  .db $20, $61, $20, $6b, $6e, $6f, $74, $68, $6f, $6c, $65, $21, $0a, $40, $49, $74
  .db $27, $73, $20, $61, $20, $73, $6f, $6c, $69, $74, $61, $72, $79, $20, $76, $61
  .db $63, $75, $75, $6d, $20, $74, $75, $62, $65, $2e, $0a, $40, $54, $68, $69, $73
  .db $20, $63, $6f, $72, $72, $6f, $64, $65, $64, $20, $72, $6f, $62, $6f, $74, $20
  .db $69, $73, $20, $63, $6c, $75, $74, $63, $68, $69, $6e, $67, $20, $61, $20, $6d
  .db $69, $74, $74, $65, $6e, $2e, $0a, $40, $22, $48, $69, $2c, $20, $49, $27, $6d
  .db $20, $41, $6e, $73, $6f, $6e, $20, $57, $69, $6c, $6c, $69, $61, $6d, $73, $2c
  .db $20, $54, $56, $27, $73, $20, $27, $50, $6f, $74, $73, $79, $27, $2e, $22, $0a
  .db $40, $54, $68, $69, $73, $20, $73, $75, $62, $77, $6f, $6f, $66, $65, $72, $20
  .db $77, $61, $73, $20, $62, $6c, $6f, $77, $6e, $20, $6f, $75, $74, $20, $69, $6e
  .db $20, $31, $39, $37, $34, $2e, $0a, $40, $54, $68, $72, $65, $65, $20, $68, $61
  .db $6c, $66, $2d, $70, $65, $6e, $6e, $69, $65, $73, $20, $61, $6e, $64, $20, $61
  .db $20, $77, $6f, $6f, $64, $65, $6e, $20, $6e, $69, $63, $6b, $65, $6c, $2e, $0a
  .db $40, $49, $74, $27, $73, $20, $74, $68, $65, $20, $6d, $69, $73, $73, $69, $6e
  .db $67, $20, $63, $68, $61, $70, $74, $65, $72, $20, $74, $6f, $20, $22, $41, $20
  .db $43, $6c, $6f, $63, $6b, $77, $6f, $72, $6b, $20, $4f, $72, $61, $6e, $67, $65
  .db $22, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $62, $75, $72, $72, $69
  .db $74, $6f, $20, $73, $74, $61, $6e, $64, $20, $66, $6c, $79, $65, $72, $2e, $20
  .db $22, $54, $61, $71, $75, $65, $72, $69, $61, $20, $45, $6c, $20, $52, $61, $6e
  .db $63, $68, $69, $74, $6f, $22, $2e, $0a, $40, $54, $68, $69, $73, $20, $73, $6d
  .db $69, $6c, $69, $6e, $67, $20, $66, $61, $6d, $69, $6c, $79, $20, $69, $73, $20
  .db $68, $61, $70, $70, $79, $20, $62, $65, $63, $61, $75, $73, $65, $20, $74, $68
  .db $65, $79, $20, $65, $61, $74, $20, $4c, $41, $52, $44, $2e, $0a, $40, $52, $6f
  .db $67, $65, $72, $20, $41, $76, $65, $72, $79, $2c, $20, $70, $65, $72, $73, $6f
  .db $6e, $61, $20, $75, $6e, $20, $66, $61, $6d, $6f, $73, $6f, $20, $64, $65, $20
  .db $6c, $6f, $73, $20, $45, $73, $74, $61, $64, $6f, $73, $20, $55, $6e, $69, $64
  .db $6f, $73, $2e, $0a, $40, $4e, $65, $27, $65, $72, $20, $62, $75, $74, $20, $61
  .db $20, $70, $6f, $74, $74, $65, $64, $20, $70, $6c, $61, $6e, $74, $2e, $0a, $40
  .db $41, $20, $70, $61, $72, $72, $6f, $74, $2c, $20, $6b, $69, $70, $70, $69, $6e
  .db $67, $20, $6f, $6e, $20, $69, $74, $73, $20, $62, $61, $63, $6b, $2e, $0a, $40
  .db $41, $20, $66, $6f, $72, $67, $6f, $74, $74, $65, $6e, $20, $74, $65, $6c, $65
  .db $70, $68, $6f, $6e, $65, $20, $73, $77, $69, $74, $63, $68, $62, $6f, $61, $72
  .db $64, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $74, $72, $69, $6f, $20
  .db $6f, $66, $20, $53, $77, $65, $64, $69, $73, $68, $20, $68, $61, $63, $6b, $65
  .db $72, $73, $2e, $20, $54, $68, $65, $79, $20, $73, $65, $65, $6d, $20, $74, $6f
  .db $20, $62, $65, $20, $62, $75, $73, $79, $20, $77, $69, $74, $68, $20, $74, $68
  .db $61, $74, $20, $4d, $50, $33, $20, $70, $6c, $61, $79, $65, $72, $2e, $0a, $40
  .db $41, $20, $66, $6f, $72, $67, $6f, $74, $74, $65, $6e, $20, $74, $65, $6c, $65
  .db $70, $68, $6f, $6e, $65, $20, $73, $77, $69, $74, $63, $68, $62, $6f, $61, $72
  .db $64, $20, $6f, $70, $65, $72, $61, $74, $6f, $72, $2e, $0a, $40, $49, $74, $27
  .db $73, $20, $61, $6e, $20, $61, $75, $74, $6f, $6d, $61, $74, $65, $64, $20, $72
  .db $6f, $62, $6f, $74, $2d, $64, $69, $73, $64, $61, $69, $6e, $65, $72, $2e, $20
  .db $49, $74, $20, $70, $72, $65, $74, $65, $6e, $64, $73, $20, $79, $6f, $75, $27
  .db $72, $65, $20, $6e, $6f, $74, $20, $74, $68, $65, $72, $65, $2e, $0a, $40, $49
  .db $74, $27, $73, $20, $61, $20, $70, $6f, $72, $74, $61, $62, $6c, $65, $20, $68
  .db $6f, $6c, $65, $2e, $20, $41, $20, $73, $69, $67, $6e, $20, $72, $65, $61, $64
  .db $73, $3a, $20, $22, $43, $6c, $6f, $73, $65, $64, $20, $66, $6f, $72, $20, $74
  .db $68, $65, $20, $77, $69, $6e, $74, $65, $72, $22, $2e, $0a, $40, $4a, $75, $73
  .db $74, $20, $61, $20, $6d, $6f, $6c, $64, $79, $20, $6c, $6f, $61, $66, $20, $6f
  .db $66, $20, $62, $72, $65, $61, $64, $2e, $0a, $40, $41, $20, $6c, $69, $74, $74
  .db $6c, $65, $20, $67, $6c, $61, $73, $73, $20, $74, $75, $62, $20, $6f, $66, $20
  .db $43, $61, $72, $6d, $65, $78, $2e, $20, $28, $24, $2e, $38, $39, $29, $20, $54
  .db $6f, $6f, $20, $62, $61, $64, $20, $79, $6f, $75, $20, $68, $61, $76, $65, $20
  .db $6e, $6f, $20, $6c, $69, $70, $73, $2e, $0a, $40, $41, $20, $53, $77, $69, $73
  .db $73, $2d, $41, $72, $6d, $79, $20, $6b, $6e, $69, $66, $65, $2e, $20, $41, $6c
  .db $6c, $20, $6f, $66, $20, $69, $74, $73, $20, $61, $70, $70, $65, $6e, $64, $61
  .db $67, $65, $73, $20, $61, $72, $65, $20, $6f, $75, $74, $2e, $20, $28, $74, $6f
  .db $6f, $74, $68, $70, $69, $63, $6b, $20, $6c, $6f, $73, $74, $29, $0a, $40, $49
  .db $74, $27, $73, $20, $61, $20, $7a, $65, $6e, $20, $73, $69, $6d, $75, $6c, $61
  .db $74, $69, $6f, $6e, $2c, $20, $74, $72, $61, $70, $70, $65, $64, $20, $77, $69
  .db $74, $68, $69, $6e, $20, $61, $6e, $20, $41, $53, $43, $49, $49, $20, $63, $68
  .db $61, $72, $61, $63, $74, $65, $72, $2e, $0a, $40, $49, $74, $27, $73, $20, $61
  .db $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $22, $54, $68, $65, $20, $52, $75
  .db $62, $61, $69, $79, $61, $74, $20, $6f, $66, $20, $53, $70, $69, $6b, $65, $20
  .db $53, $63, $68, $75, $64, $79, $22, $2e, $0a, $40, $49, $74, $27, $73, $20, $22
  .db $57, $61, $72, $20, $61, $6e, $64, $20, $50, $65, $61, $63, $65, $22, $20, $28
  .db $75, $6e, $61, $62, $72, $69, $64, $67, $65, $64, $2c, $20, $76, $65, $72, $79
  .db $20, $73, $6d, $61, $6c, $6c, $20, $70, $72, $69, $6e, $74, $29, $2e, $0a, $40
  .db $41, $20, $77, $69, $6c, $6c, $69, $6e, $67, $2c, $20, $72, $69, $70, $65, $20
  .db $74, $6f, $6d, $61, $74, $6f, $20, $62, $65, $6d, $6f, $61, $6e, $73, $20, $79
  .db $6f, $75, $72, $20, $69, $6e, $61, $62, $69, $6c, $69, $74, $79, $20, $74, $6f
  .db $20, $64, $69, $67, $65, $73, $74, $20, $66, $72, $75, $69, $74, $2e, $0a, $40
  .db $41, $20, $72, $6f, $62, $6f, $74, $20, $63, $6f, $6d, $65, $64, $69, $61, $6e
  .db $2e, $20, $59, $6f, $75, $20, $66, $65, $65, $6c, $20, $61, $6d, $75, $73, $65
  .db $64, $2e, $0a, $40, $49, $74, $27, $73, $20, $4b, $49, $54, $54, $2c, $20, $74
  .db $68, $65, $20, $74, $61, $6c, $6b, $69, $6e, $67, $20, $63, $61, $72, $2e, $0a
  .db $40, $48, $65, $72, $65, $27, $73, $20, $50, $65, $74, $65, $20, $50, $65, $74
  .db $65, $72, $73, $6f, $6e, $2e, $20, $48, $69, $73, $20, $62, $61, $74, $74, $65
  .db $72, $69, $65, $73, $20, $73, $65, $65, $6d, $20, $74, $6f, $20, $68, $61, $76
  .db $65, $20, $6c, $6f, $6e, $67, $20, $67, $6f, $6e, $65, $20, $64, $65, $61, $64
  .db $2e, $0a, $40, $22, $42, $6c, $75, $70, $2c, $20, $62, $6c, $75, $70, $2c, $20
  .db $62, $6c, $75, $70, $22, $2c, $20, $73, $61, $79, $73, $20, $74, $68, $65, $20
  .db $6d, $75, $64, $20, $70, $6f, $74, $2e, $0a, $40, $4d, $6f, $72, $65, $20, $67
  .db $72, $69, $73, $74, $20, $66, $6f, $72, $20, $74, $68, $65, $20, $6d, $69, $6c
  .db $6c, $2e, $0a, $40, $43, $6f, $72, $79, $20, $44, $6f, $63, $74, $6f, $72, $6f
  .db $77, $27, $73, $20, $68, $6f, $74, $20, $61, $69, $72, $20, $62, $61, $6c, $6c
  .db $6f, $6f, $6e, $20, $69, $73, $20, $74, $65, $74, $68, $65, $72, $65, $64, $20
  .db $68, $65, $72, $65, $2e, $0a, $40, $47, $72, $69, $6e, $64, $20, $27, $65, $6d
  .db $20, $75, $70, $2c, $20, $73, $70, $69, $74, $20, $27, $65, $6d, $20, $6f, $75
  .db $74, $2c, $20, $74, $68, $65, $79, $27, $72, $65, $20, $74, $77, $69, $67, $73
  .db $2e, $0a, $40, $54, $68, $65, $20, $62, $6f, $6f, $6d, $20, $62, $6f, $78, $20
  .db $63, $72, $61, $6e, $6b, $73, $20, $6f, $75, $74, $20, $61, $6e, $20, $6f, $6c
  .db $64, $20, $45, $74, $68, $65, $6c, $20, $4d, $65, $72, $6d, $61, $6e, $20, $74
  .db $75, $6e, $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $22, $46, $69, $6e, $64
  .db $69, $6e, $67, $20, $6b, $69, $74, $74, $65, $6e, $22, $2c, $20, $70, $75, $62
  .db $6c, $69, $73, $68, $65, $64, $20, $62, $79, $20, $4f, $27, $52, $65, $69, $6c
  .db $6c, $79, $20, $61, $6e, $64, $20, $41, $73, $73, $6f, $63, $69, $61, $74, $65
  .db $73, $2e, $0a, $40, $50, $75, $6d, $70, $6b, $69, $6e, $20, $70, $69, $65, $20
  .db $73, $70, $69, $63, $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $74, $68, $65
  .db $20, $42, $61, $73, $73, $2d, $4d, $61, $74, $69, $63, $20, $27, $37, $36, $21
  .db $20, $4d, $6d, $6d, $2c, $20, $74, $68, $61, $74, $27, $73, $20, $67, $6f, $6f
  .db $64, $20, $62, $61, $73, $73, $21, $0a, $40, $22, $4c, $65, $6e, $64, $20, $75
  .db $73, $20, $61, $20, $66, $69, $76, $65, $72, $20, $27, $74, $69, $6c, $20, $54
  .db $68, $75, $72, $73, $64, $61, $79, $22, $2c, $20, $70, $6c, $65, $61, $73, $20
  .db $41, $6e, $64, $79, $20, $43, $61, $70, $70, $2e, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $20, $74, $61, $70, $65, $20, $6f, $66, $20, $27, $37, $30, $73, $20
  .db $72, $6f, $63, $6b, $2e, $20, $41, $6c, $6c, $20, $6f, $72, $69, $67, $69, $6e
  .db $61, $6c, $20, $68, $69, $74, $73, $21, $20, $41, $6c, $6c, $20, $6f, $72, $69
  .db $67, $69, $6e, $61, $6c, $20, $61, $72, $74, $69, $73, $74, $73, $21, $0a, $40
  .db $59, $6f, $75, $27, $76, $65, $20, $66, $6f, $75, $6e, $64, $20, $74, $68, $65
  .db $20, $66, $61, $62, $6c, $65, $64, $20, $41, $6d, $65, $72, $69, $63, $61, $20
  .db $4f, $6e, $6c, $69, $6e, $65, $20, $64, $69, $73, $6b, $20, $67, $72, $61, $76
  .db $65, $79, $61, $72, $64, $21, $0a, $40, $45, $6d, $70, $74, $79, $20, $6a, $65
  .db $77, $65, $6c, $62, $6f, $78, $65, $73, $20, $6c, $69, $74, $74, $65, $72, $20
  .db $74, $68, $65, $20, $6c, $61, $6e, $64, $73, $63, $61, $70, $65, $2e, $0a, $40
  .db $49, $74, $27, $73, $20, $74, $68, $65, $20, $61, $73, $74, $6f, $75, $6e, $64
  .db $69, $6e, $67, $20, $6d, $65, $74, $61, $2d, $6f, $62, $6a, $65, $63, $74, $2e
  .db $0a, $40, $45, $64, $20, $4d, $63, $4d, $61, $68, $6f, $6e, $20, $73, $74, $61
  .db $6e, $64, $73, $20, $68, $65, $72, $65, $2c, $20, $6c, $6f, $73, $74, $20, $69
  .db $6e, $20, $74, $68, $6f, $75, $67, $68, $74, $2e, $20, $53, $65, $65, $69, $6e
  .db $67, $20, $79, $6f, $75, $2c, $20, $68, $65, $20, $62, $65, $6c, $6c, $6f, $77
  .db $73, $2c, $20, $22, $59, $45, $53, $20, $53, $49, $52, $21, $22, $0a, $40, $2e
  .db $2e, $2e, $74, $68, $69, $6e, $67, $79, $3f, $3f, $3f, $0a, $40, $49, $74, $27
  .db $73, $20, $31, $30, $30, $30, $20, $73, $65, $63, $72, $65, $74, $73, $20, $74
  .db $68, $65, $20, $67, $6f, $76, $65, $72, $6e, $6d, $65, $6e, $74, $20, $64, $6f
  .db $65, $73, $6e, $27, $74, $20, $77, $61, $6e, $74, $20, $79, $6f, $75, $20, $74
  .db $6f, $20, $6b, $6e, $6f, $77, $21, $0a, $40, $54, $68, $65, $20, $6c, $65, $74
  .db $74, $65, $72, $73, $20, $4f, $20, $61, $6e, $64, $20, $52, $2e, $0a, $40, $41
  .db $20, $6d, $61, $67, $69, $63, $61, $6c, $2e, $2e, $2e, $20, $6d, $61, $67, $69
  .db $63, $20, $74, $68, $69, $6e, $67, $2e, $0a, $40, $49, $74, $27, $73, $20, $61
  .db $20, $6d, $6f, $6d, $65, $6e, $74, $20, $6f, $66, $20, $73, $69, $6c, $65, $6e
  .db $63, $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $53, $69, $72, $68, $61, $6e
  .db $2d, $53, $69, $72, $68, $61, $6e, $2c, $20, $6c, $6f, $6f, $6b, $69, $6e, $67
  .db $20, $67, $75, $69, $6c, $74, $79, $2e, $0a, $40, $49, $74, $27, $73, $20, $22
  .db $43, $68, $69, $63, $6b, $65, $6e, $20, $53, $6f, $75, $70, $20, $66, $6f, $72
  .db $20, $74, $68, $65, $20, $4b, $69, $74, $74, $65, $6e, $2d, $73, $65, $65, $6b
  .db $69, $6e, $67, $20, $53, $6f, $75, $6c, $6c, $65, $73, $73, $20, $52, $6f, $62
  .db $6f, $74, $2e, $22, $0a, $40, $49, $74, $20, $69, $73, $20, $61, $20, $73, $65
  .db $74, $20, $6f, $66, $20, $77, $69, $6e, $64, $2d, $75, $70, $20, $63, $68, $61
  .db $74, $74, $65, $72, $20, $74, $65, $65, $74, $68, $2e, $0a, $40, $49, $74, $20
  .db $69, $73, $20, $61, $20, $63, $6c, $6f, $75, $64, $20, $73, $68, $61, $70, $65
  .db $64, $20, $6c, $69, $6b, $65, $20, $61, $6e, $20, $6f, $78, $2e, $0a, $40, $59
  .db $6f, $75, $20, $73, $65, $65, $20, $61, $20, $73, $6e, $6f, $77, $66, $6c, $61
  .db $6b, $65, $20, $68, $65, $72, $65, $2c, $20, $6d, $65, $6c, $74, $69, $6e, $67
  .db $20, $73, $6c, $6f, $77, $6c, $79, $2e, $0a, $40, $49, $74, $27, $73, $20, $39
  .db $31, $20, $79, $61, $72, $64, $73, $20, $6f, $66, $20, $74, $77, $69, $6e, $65
  .db $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $62, $69, $67, $20, $62, $6c
  .db $6f, $63, $6b, $20, $6f, $66, $20, $69, $63, $65, $2e, $20, $53, $6f, $6d, $65
  .db $74, $68, $69, $6e, $67, $20, $73, $65, $65, $6d, $73, $20, $74, $6f, $20, $62
  .db $65, $20, $66, $72, $6f, $7a, $65, $6e, $20, $69, $6e, $73, $69, $64, $65, $20
  .db $69, $74, $2e, $0a, $40, $56, $6c, $61, $64, $69, $6d, $69, $72, $20, $4c, $65
  .db $6e, $69, $6e, $27, $73, $20, $63, $61, $73, $6b, $65, $74, $20, $72, $65, $73
  .db $74, $73, $20, $68, $65, $72, $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $61
  .db $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $22, $5a, $65, $6e, $20, $61, $6e
  .db $64, $20, $54, $68, $65, $20, $41, $72, $74, $20, $6f, $66, $20, $52, $6f, $62
  .db $6f, $74, $20, $4d, $61, $69, $6e, $74, $65, $6e, $61, $6e, $63, $65, $22, $2e
  .db $0a, $40, $54, $68, $69, $73, $20, $69, $6e, $76, $69, $73, $69, $62, $6c, $65
  .db $20, $62, $6f, $78, $20, $63, $6f, $6e, $74, $61, $69, $6e, $73, $20, $61, $20
  .db $70, $61, $6e, $74, $6f, $6d, $69, $6d, $65, $20, $68, $6f, $72, $73, $65, $2e
  .db $0a, $40, $41, $20, $6d, $61, $73, $6f, $6e, $20, $6a, $61, $72, $20, $6c, $69
  .db $65, $73, $20, $68, $65, $72, $65, $20, $6f, $70, $65, $6e, $2e, $20, $49, $74
  .db $27, $73, $20, $6c, $61, $62, $65, $6c, $20, $72, $65, $61, $64, $73, $3a, $20
  .db $22, $64, $6f, $20, $6e, $6f, $74, $20, $6f, $70, $65, $6e, $21, $22, $2e, $0a
  .db $40, $41, $20, $74, $72, $61, $69, $6e, $20, $6f, $66, $20, $74, $68, $6f, $75
  .db $67, $68, $74, $20, $63, $68, $75, $67, $73, $20, $74, $68, $72, $6f, $75, $67
  .db $68, $20, $68, $65, $72, $65, $2e, $0a, $40, $54, $68, $69, $73, $20, $6a, $61
  .db $72, $20, $6f, $66, $20, $70, $69, $63, $6b, $6c, $65, $73, $20, $65, $78, $70
  .db $69, $72, $65, $64, $20, $69, $6e, $20, $31, $39, $35, $37, $2e, $0a, $40, $53
  .db $6f, $6d, $65, $6f, $6e, $65, $27, $73, $20, $69, $64, $65, $6e, $74, $69, $74
  .db $79, $20, $64, $69, $73, $6b, $20, $6c, $69, $65, $73, $20, $68, $65, $72, $65
  .db $2e, $0a, $40, $22, $59, $65, $73, $21, $22, $20, $73, $61, $79, $73, $20, $74
  .db $68, $65, $20, $62, $69, $74, $2e, $0a, $40, $22, $4e, $6f, $21, $22, $20, $73
  .db $61, $79, $73, $20, $74, $68, $65, $20, $62, $69, $74, $2e, $0a, $40, $41, $20
  .db $64, $6f, $64, $65, $63, $61, $68, $65, $64, $72, $6f, $6e, $20, $62, $61, $72
  .db $73, $20, $79, $6f, $75, $72, $20, $77, $61, $79, $2e, $0a, $40, $4d, $72, $2e
  .db $20, $48, $6f, $6f, $70, $65, $72, $20, $69, $73, $20, $68, $65, $72, $65, $2c
  .db $20, $73, $75, $72, $66, $69, $6e, $67, $2e, $0a, $40, $49, $74, $27, $73, $20
  .db $61, $20, $62, $69, $67, $20, $73, $6d, $6f, $6b, $69, $6e, $67, $20, $66, $69
  .db $73, $68, $2e, $0a, $40, $59, $6f, $75, $20, $68, $61, $76, $65, $20, $6e, $65
  .db $77, $20, $6d, $61, $69, $6c, $20, $69, $6e, $20, $2f, $76, $61, $72, $2f, $73
  .db $70, $6f, $6f, $6c, $2f, $72, $6f, $62, $6f, $74, $0a, $40, $4a, $75, $73, $74
  .db $20, $61, $20, $6d, $6f, $6e, $69, $74, $6f, $72, $20, $77, $69, $74, $68, $20
  .db $74, $68, $65, $20, $62, $6c, $75, $65, $20, $65, $6c, $65, $6d, $65, $6e, $74
  .db $20, $62, $75, $72, $6e, $74, $20, $6f, $75, $74, $2e, $0a, $40, $41, $20, $70
  .db $69, $6c, $65, $20, $6f, $66, $20, $63, $6f, $61, $78, $69, $61, $6c, $20, $70
  .db $6c, $75, $6d, $62, $69, $6e, $67, $20, $6c, $69, $65, $73, $20, $68, $65, $72
  .db $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $72, $6f, $74, $74, $65
  .db $6e, $20, $6f, $6c, $64, $20, $73, $68, $6f, $65, $2e, $0a, $40, $49, $74, $27
  .db $73, $20, $61, $20, $68, $75, $6e, $64, $72, $65, $64, $2d, $64, $6f, $6c, $6c
  .db $61, $72, $20, $62, $69, $6c, $6c, $2e, $0a, $40, $49, $74, $27, $73, $20, $61
  .db $20, $44, $76, $6f, $72, $61, $6b, $20, $6b, $65, $79, $62, $6f, $61, $72, $64
  .db $2e, $0a, $40, $49, $74, $27, $73, $20, $42, $65, $6e, $64, $65, $72, $2c, $20
  .db $74, $68, $65, $20, $6c, $6f, $76, $65, $61, $62, $6c, $65, $20, $6b, $6c, $65
  .db $70, $74, $6f, $6d, $61, $6e, $69, $61, $63, $20, $72, $6f, $62, $6f, $74, $21
  .db $20, $22, $42, $69, $74, $65, $20, $6d, $79, $20, $73, $68, $69, $6e, $79, $20
  .db $6d, $65, $74, $61, $6c, $20, $61, $73, $73, $21, $22, $0a, $40, $49, $74, $27
  .db $73, $20, $61, $20, $63, $61, $72, $64, $62, $6f, $61, $72, $64, $20, $62, $6f
  .db $78, $20, $66, $75, $6c, $6c, $20, $6f, $66, $20, $38, $2d, $74, $72, $61, $63
  .db $6b, $73, $2e, $0a, $40, $4a, $75, $73, $74, $20, $61, $20, $62, $72, $6f, $6b
  .db $65, $6e, $20, $68, $61, $72, $64, $20, $64, $72, $69, $76, $65, $20, $63, $6f
  .db $6e, $74, $61, $69, $6e, $67, $20, $74, $68, $65, $20, $61, $72, $63, $68, $69
  .db $76, $65, $73, $20, $6f, $66, $20, $4e, $65, $72, $74, $68, $20, $50, $6f, $72
  .db $6b, $2e, $0a, $40, $41, $20, $62, $72, $6f, $6b, $65, $6e, $20, $6d, $65, $74
  .db $72, $6f, $6e, $6f, $6d, $65, $20, $73, $69, $74, $73, $20, $68, $65, $72, $65
  .db $2c, $20, $69, $74, $27, $73, $20, $6e, $65, $65, $64, $6c, $65, $20, $6f, $66
  .db $66, $20, $74, $6f, $20, $6f, $6e, $65, $20, $73, $69, $64, $65, $2e, $0a, $40
  .db $41, $20, $73, $69, $67, $6e, $20, $72, $65, $61, $64, $73, $3a, $20, $22, $47
  .db $6f, $20, $68, $6f, $6d, $65, $21, $22, $0a, $40, $41, $20, $73, $69, $67, $6e
  .db $20, $72, $65, $61, $64, $73, $3a, $20, $22, $4e, $6f, $20, $72, $6f, $62, $6f
  .db $74, $73, $20, $61, $6c, $6c, $6f, $77, $65, $64, $21, $22, $0a, $40, $49, $74
  .db $27, $73, $20, $74, $68, $65, $20, $68, $61, $6e, $64, $68, $65, $6c, $64, $20
  .db $72, $6f, $62, $6f, $74, $66, $69, $6e, $64, $73, $6b, $69, $74, $74, $65, $6e
  .db $20, $67, $61, $6d, $65, $2c, $20, $62, $79, $20, $54, $69, $67, $65, $72, $2e
  .db $0a, $40, $54, $68, $69, $73, $20, $70, $61, $72, $74, $69, $63, $75, $6c, $61
  .db $72, $20, $6d, $6f, $6e, $73, $74, $72, $6f, $73, $69, $74, $79, $20, $61, $70
  .db $70, $65, $61, $72, $73, $20, $74, $6f, $20, $62, $65, $20, $45, $4e, $49, $41
  .db $43, $2e, $0a, $40, $54, $68, $69, $73, $20, $69, $73, $20, $61, $20, $74, $61
  .db $73, $74, $79, $2d, $6c, $6f, $6f, $6b, $69, $6e, $67, $20, $62, $61, $6e, $61
  .db $6e, $61, $20, $63, $72, $65, $6d, $65, $20, $70, $69, $65, $2e, $0a, $40, $41
  .db $20, $77, $69, $72, $65, $66, $72, $61, $6d, $65, $20, $6d, $6f, $64, $65, $6c
  .db $20, $6f, $66, $20, $61, $20, $68, $6f, $74, $20, $64, $6f, $67, $20, $72, $6f
  .db $74, $61, $74, $65, $73, $20, $69, $6e, $20, $73, $70, $61, $63, $65, $20, $68
  .db $65, $72, $65, $2e, $0a, $40, $4a, $75, $73, $74, $20, $74, $68, $65, $20, $65
  .db $6d, $70, $74, $79, $20, $68, $75, $73, $6b, $20, $6f, $66, $20, $61, $20, $6c
  .db $6f, $63, $75, $73, $74, $2e, $0a, $40, $59, $6f, $75, $20, $64, $69, $73, $74
  .db $75, $72, $62, $20, $61, $20, $6d, $75, $72, $64, $65, $72, $20, $6f, $66, $20
  .db $63, $72, $6f, $77, $73, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $63
  .db $6f, $70, $79, $20, $6f, $66, $20, $74, $68, $65, $20, $72, $6f, $62, $6f, $74
  .db $66, $69, $6e, $64, $73, $6b, $69, $74, $74, $65, $6e, $20, $45, $55, $4c, $41
  .db $2e, $0a, $40, $49, $74, $27, $73, $20, $44, $65, $61, $74, $68, $2e, $0a, $40
  .db $49, $74, $27, $73, $20, $61, $6e, $20, $61, $75, $74, $6f, $67, $72, $61, $70
  .db $68, $65, $64, $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $22, $53, $65, $63
  .db $6f, $6e, $64, $61, $72, $79, $20, $43, $6f, $6c, $6f, $72, $73, $2c, $22, $20
  .db $62, $79, $20, $42, $6f, $62, $20, $52, $6f, $73, $73, $2e, $0a, $40, $49, $74
  .db $20, $69, $73, $20, $61, $20, $6d, $61, $72, $7a, $69, $70, $61, $6e, $20, $64
  .db $72, $65, $61, $64, $6e, $6f, $75, $67, $68, $74, $20, $74, $68, $61, $74, $20
  .db $61, $70, $70, $65, $61, $72, $73, $20, $74, $6f, $20, $68, $61, $76, $65, $20
  .db $6d, $65, $6c, $74, $65, $64, $20, $61, $6e, $64, $20, $73, $74, $75, $63, $6b
  .db $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $44, $56, $44, $20, $6f, $66
  .db $20, $22, $43, $72, $6f, $75, $63, $68, $69, $6e, $67, $20, $4d, $6f, $6e, $6b
  .db $65, $79, $2c, $20, $48, $69, $64, $64, $65, $6e, $20, $4b, $69, $74, $74, $65
  .db $6e, $22, $2c, $20, $72, $65, $67, $69, $6f, $6e, $20, $65, $6e, $63, $6f, $64
  .db $65, $64, $20, $66, $6f, $72, $20, $74, $68, $65, $20, $6d, $6f, $6f, $6e, $2e
  .db $0a, $40, $49, $74, $27, $73, $20, $4b, $69, $65, $72, $61, $6e, $20, $48, $65
  .db $72, $76, $6f, $6c, $64, $2e, $20, $44, $61, $6d, $6e, $20, $64, $79, $73, $6c
  .db $65, $78, $69, $61, $21, $0a, $40, $41, $20, $6e, $6f, $6e, $2d, $64, $65, $73
  .db $63, $72, $69, $70, $74, $20, $62, $6f, $78, $20, $6f, $66, $20, $63, $72, $61
  .db $63, $6b, $65, $72, $73, $2e, $0a, $40, $43, $61, $72, $62, $6f, $6e, $61, $74
  .db $65, $64, $20, $57, $61, $74, $65, $72, $2c, $20, $48, $69, $67, $68, $20, $46
  .db $72, $75, $63, $74, $6f, $73, $65, $20, $43, $6f, $72, $6e, $20, $53, $79, $72
  .db $75, $70, $2c, $20, $43, $6f, $6c, $6f, $72, $2c, $20, $50, $68, $6f, $73, $70
  .db $68, $6f, $72, $69, $63, $20, $41, $63, $69, $64, $2c, $20, $46, $6c, $61, $76
  .db $6f, $72, $73, $2c, $20, $43, $61, $66, $66, $65, $69, $6e, $65, $2e, $0a, $40
  .db $22, $4d, $6f, $76, $65, $20, $61, $6c, $6f, $6e, $67, $21, $20, $4e, $6f, $74
  .db $68, $69, $6e, $67, $20, $74, $6f, $20, $73, $65, $65, $20, $68, $65, $72, $65
  .db $21, $22, $0a, $40, $49, $74, $27, $73, $20, $74, $68, $65, $20, $65, $6d, $62
  .db $61, $6c, $6d, $65, $64, $20, $63, $6f, $72, $70, $73, $65, $20, $6f, $66, $20
  .db $56, $6c, $61, $64, $69, $6d, $69, $72, $20, $4c, $65, $6e, $69, $6e, $2e, $0a
  .db $40, $41, $20, $63, $6f, $75, $70, $6f, $6e, $20, $66, $6f, $72, $20, $6f, $6e
  .db $65, $20, $66, $72, $65, $65, $20, $73, $74, $65, $61, $6b, $2d, $66, $69, $73
  .db $68, $20, $61, $74, $20, $79, $6f, $75, $72, $20, $6c, $6f, $63, $61, $6c, $20
  .db $66, $61, $6d, $69, $6c, $79, $20, $64, $69, $6e, $65, $72, $2e, $0a, $40, $41
  .db $20, $73, $65, $74, $20, $6f, $66, $20, $6b, $65, $79, $73, $20, $74, $6f, $20
  .db $61, $20, $32, $30, $30, $31, $20, $52, $6f, $6c, $6c, $73, $20, $52, $6f, $79
  .db $63, $65, $2e, $20, $57, $6f, $72, $74, $68, $6c, $65, $73, $73, $2e, $0a, $40
  .db $41, $20, $67, $72, $61, $76, $65, $73, $74, $6f, $6e, $65, $20, $73, $74, $61
  .db $6e, $64, $73, $20, $68, $65, $72, $65, $2e, $22, $49, $7a, $63, $68, $61, $6b
  .db $20, $4d, $69, $6c, $6c, $65, $72, $2c, $20, $61, $73, $63, $65, $6e, $64, $65
  .db $64, $2e, $22, $0a, $40, $53, $6f, $6d, $65, $6f, $6e, $65, $20, $68, $61, $73
  .db $20, $77, $72, $69, $74, $74, $65, $6e, $20, $22, $61, $64, $20, $61, $65, $72
  .db $61, $72, $69, $75, $6d, $22, $20, $6f, $6e, $20, $74, $68, $65, $20, $67, $72
  .db $6f, $75, $6e, $64, $20, $68, $65, $72, $65, $2e, $0a, $40, $41, $20, $6c, $61
  .db $72, $67, $65, $20, $62, $6c, $75, $65, $20, $65, $79, $65, $20, $66, $6c, $6f
  .db $61, $74, $73, $20, $69, $6e, $20, $6d, $69, $64, $61, $69, $72, $2e, $0a, $40
  .db $54, $68, $69, $73, $20, $61, $70, $70, $65, $61, $72, $73, $20, $74, $6f, $20
  .db $62, $65, $20, $61, $20, $73, $74, $61, $74, $75, $65, $20, $6f, $66, $20, $50
  .db $65, $72, $73, $65, $75, $73, $2e, $0a, $40, $54, $68, $65, $72, $65, $20, $69
  .db $73, $20, $61, $6e, $20, $6f, $70, $75, $6c, $65, $6e, $74, $20, $74, $68, $72
  .db $6f, $6e, $65, $20, $68, $65, $72, $65, $2e, $0a, $40, $49, $74, $27, $73, $20
  .db $61, $20, $73, $71, $75, $61, $64, $20, $6f, $66, $20, $4b, $65, $79, $73, $74
  .db $6f, $6e, $65, $20, $4b, $6f, $70, $73, $2e, $0a, $40, $54, $68, $69, $73, $20
  .db $73, $65, $65, $6d, $73, $20, $74, $6f, $20, $62, $65, $20, $6a, $75, $6e, $6b
  .db $20, $6d, $61, $69, $6c, $20, $61, $64, $64, $72, $65, $73, $73, $65, $64, $20
  .db $74, $6f, $20, $74, $68, $65, $20, $66, $69, $6e, $64, $65, $72, $20, $6f, $66
  .db $20, $74, $68, $65, $20, $45, $79, $65, $20, $6f, $66, $20, $4c, $61, $72, $6e
  .db $2e, $0a, $40, $41, $20, $77, $6f, $6e, $64, $72, $6f, $75, $73, $20, $61, $6e
  .db $64, $20, $69, $6e, $74, $72, $69, $63, $61, $74, $65, $20, $67, $6f, $6c, $64
  .db $65, $6e, $20, $61, $6d, $75, $6c, $65, $74, $2e, $20, $54, $6f, $6f, $20, $62
  .db $61, $64, $20, $79, $6f, $75, $20, $68, $61, $76, $65, $20, $6e, $6f, $20, $6e
  .db $65, $63, $6b, $2e, $0a, $40, $54, $68, $65, $20, $73, $77, $61, $6d, $70, $79
  .db $20, $67, $72, $6f, $75, $6e, $64, $20, $61, $72, $6f, $75, $6e, $64, $20, $79
  .db $6f, $75, $20, $73, $65, $65, $6d, $73, $20, $74, $6f, $20, $73, $74, $69, $6e
  .db $6b, $20, $77, $69, $74, $68, $20, $64, $69, $73, $65, $61, $73, $65, $2e, $0a
  .db $40, $41, $6e, $20, $61, $6e, $69, $6d, $61, $74, $65, $20, $62, $6c, $6f, $62
  .db $20, $6f, $66, $20, $61, $63, $69, $64, $2e, $20, $42, $65, $69, $6e, $67, $20
  .db $6d, $65, $74, $61, $6c, $6c, $69, $63, $2c, $20, $79, $6f, $75, $20, $6b, $65
  .db $65, $70, $20, $77, $65, $6c, $6c, $20, $61, $77, $61, $79, $2e, $0a, $40, $49
  .db $74, $27, $73, $20, $61, $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $4b, $6e
  .db $75, $74, $68, $20, $77, $69, $74, $68, $20, $74, $68, $65, $20, $63, $68, $61
  .db $70, $74, $65, $72, $20, $6f, $6e, $20, $6b, $69, $74, $74, $65, $6e, $2d, $73
  .db $65, $61, $72, $63, $68, $20, $61, $6c, $67, $6f, $72, $69, $74, $68, $6d, $73
  .db $20, $74, $6f, $72, $6e, $20, $6f, $75, $74, $2e, $0a, $40, $41, $20, $63, $72
  .db $6f, $77, $64, $20, $6f, $66, $20, $70, $65, $6f, $70, $6c, $65, $2c, $20, $61
  .db $6e, $64, $20, $61, $74, $20, $74, $68, $65, $20, $63, $65, $6e, $74, $65, $72
  .db $2c, $20, $61, $20, $70, $6f, $70, $75, $6c, $61, $72, $20, $6d, $69, $73, $63
  .db $6f, $6e, $63, $65, $70, $74, $69, $6f, $6e, $2e, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $20, $62, $6c, $69, $6e, $64, $20, $6d, $61, $6e, $2e, $20, $57, $68
  .db $65, $6e, $20, $79, $6f, $75, $20, $74, $6f, $75, $63, $68, $2c, $20, $68, $65
  .db $20, $65, $78, $63, $6c, $61, $69, $6d, $73, $20, $22, $49, $74, $27, $73, $20
  .db $61, $20, $6b, $69, $74, $74, $65, $6e, $20, $70, $72, $6f, $73, $70, $65, $63
  .db $74, $69, $6e, $67, $20, $72, $6f, $62, $6f, $74, $21, $22, $0a, $40, $49, $74
  .db $27, $73, $20, $61, $20, $6c, $6f, $73, $74, $20, $77, $61, $6c, $6c, $65, $74
  .db $2e, $20, $49, $74, $27, $73, $20, $6f, $77, $6e, $65, $72, $20, $64, $69, $64
  .db $6e, $27, $74, $20, $68, $61, $76, $65, $20, $70, $65, $74, $73, $2c, $20, $73
  .db $6f, $20, $79, $6f, $75, $20, $64, $69, $73, $63, $61, $72, $64, $20, $69, $74
  .db $2e, $0a, $40, $54, $68, $69, $73, $20, $70, $6c, $61, $63, $65, $20, $69, $73
  .db $20, $63, $61, $6c, $6c, $65, $64, $20, $41, $6e, $74, $61, $72, $63, $74, $69
  .db $63, $61, $2e, $20, $54, $68, $65, $72, $65, $20, $69, $73, $20, $6e, $6f, $20
  .db $6b, $69, $74, $74, $65, $6e, $20, $68, $65, $72, $65, $2e, $0a, $40, $49, $74
  .db $27, $73, $20, $61, $20, $6d, $6f, $75, $73, $65, $74, $72, $61, $70, $2c, $20
  .db $62, $61, $69, $74, $65, $64, $20, $77, $69, $74, $68, $20, $73, $6f, $61, $70
  .db $2e, $0a, $40, $41, $20, $62, $6f, $6f, $6b, $20, $77, $69, $74, $68, $20, $22
  .db $44, $6f, $6e, $27, $74, $20, $50, $61, $6e, $69, $63, $22, $20, $69, $6e, $20
  .db $6c, $61, $72, $67, $65, $20, $66, $72, $69, $65, $6e, $64, $6c, $79, $20, $6c
  .db $65, $74, $74, $65, $72, $73, $20, $61, $63, $72, $6f, $73, $73, $20, $74, $68
  .db $65, $20, $63, $6f, $76, $65, $72, $2e, $0a, $40, $41, $20, $63, $6f, $6d, $70
  .db $65, $6e, $64, $69, $75, $6d, $20, $6f, $66, $20, $68, $61, $69, $6b, $75, $20
  .db $61, $62, $6f, $75, $74, $20, $6d, $65, $74, $61, $6c, $73, $2e, $0a, $40, $41
  .db $20, $64, $69, $73, $63, $72, $65, $64, $69, $74, $65, $64, $20, $63, $6f, $73
  .db $6d, $6f, $6c, $6f, $67, $79, $2c, $20, $72, $65, $6c, $69, $63, $20, $6f, $66
  .db $20, $61, $20, $62, $79, $67, $6f, $6e, $65, $20, $65, $72, $61, $2e, $0a, $40
  .db $41, $20, $68, $6f, $6c, $6c, $6f, $77, $20, $76, $6f, $69, $63, $65, $20, $73
  .db $61, $79, $73, $20, $22, $50, $6c, $75, $67, $68, $22, $2e, $0a, $40, $41, $20
  .db $6b, $6e, $69, $67, $68, $74, $20, $77, $68, $6f, $20, $73, $61, $79, $73, $20
  .db $22, $45, $69, $74, $68, $65, $72, $20, $49, $20, $61, $6d, $20, $61, $6e, $20
  .db $69, $6e, $73, $61, $6e, $65, $20, $6b, $6e, $61, $76, $65, $2c, $20, $6f, $72
  .db $20, $79, $6f, $75, $20, $77, $69, $6c, $6c, $20, $66, $69, $6e, $64, $20, $6b
  .db $69, $74, $74, $65, $6e, $2e, $22, $0a, $40, $54, $68, $65, $20, $73, $65, $63
  .db $72, $65, $74, $20, $6d, $65, $65, $74, $69, $6e, $67, $20, $70, $6c, $61, $63
  .db $65, $20, $6f, $66, $20, $74, $68, $65, $20, $4b, $6e, $69, $67, $68, $74, $73
  .db $20, $6f, $66, $20, $74, $68, $65, $20, $4c, $61, $6d, $62, $64, $61, $20, $43
  .db $61, $6c, $63, $75, $6c, $75, $73, $2e, $0a, $40, $41, $20, $6e, $65, $75, $72
  .db $61, $6c, $20, $6e, $65, $74, $20, $2d, $2d, $20, $6d, $61, $79, $62, $65, $20
  .db $69, $74, $27, $73, $20, $74, $72, $79, $69, $6e, $67, $20, $74, $6f, $20, $72
  .db $65, $63, $6f, $67, $6e, $69, $7a, $65, $20, $6b, $69, $74, $74, $65, $6e, $2e
  .db $0a, $40, $41, $20, $73, $63, $72, $65, $77, $64, $72, $69, $76, $65, $72, $2e
  .db $0a, $40, $41, $20, $73, $74, $61, $74, $75, $65, $20, $6f, $66, $20, $61, $20
  .db $67, $69, $72, $6c, $20, $68, $6f, $6c, $64, $69, $6e, $67, $20, $61, $20, $67
  .db $6f, $6f, $73, $65, $20, $6c, $69, $6b, $65, $20, $74, $68, $65, $20, $6f, $6e
  .db $65, $20, $69, $6e, $20, $47, $6f, $74, $74, $69, $6e, $67, $65, $6e, $2c, $20
  .db $47, $65, $72, $6d, $61, $6e, $79, $2e, $0a, $40, $41, $20, $74, $65, $74, $72
  .db $61, $64, $72, $61, $63, $68, $6d, $20, $64, $61, $74, $65, $64, $20, $22, $34
  .db $32, $20, $42, $2e, $43, $2e, $22, $0a, $40, $41, $20, $76, $6f, $69, $63, $65
  .db $20, $62, $6f, $6f, $6d, $73, $20, $6f, $75, $74, $20, $22, $4f, $6e, $77, $61
  .db $72, $64, $2c, $20, $6b, $69, $74, $74, $65, $6e, $20, $73, $6f, $6c, $64, $69
  .db $65, $72, $73, $2e, $2e, $2e, $22, $0a, $40, $41, $6e, $20, $65, $6d, $69, $6e
  .db $65, $6e, $74, $6c, $79, $20, $66, $6f, $72, $67, $65, $74, $74, $61, $62, $6c
  .db $65, $20, $7a, $61, $68, $69, $72, $2e, $0a, $40, $41, $70, $70, $61, $72, $65
  .db $6e, $74, $6c, $79, $2c, $20, $69, $74, $27, $73, $20, $45, $64, $6d, $75, $6e
  .db $64, $20, $42, $75, $72, $6b, $65, $2e, $0a, $40, $46, $6f, $72, $20, $61, $20
  .db $6d, $6f, $6d, $65, $6e, $74, $2c, $20, $79, $6f, $75, $20, $66, $65, $65, $6c
  .db $20, $73, $6f, $6d, $65, $74, $68, $69, $6e, $67, $20, $69, $6e, $20, $79, $6f
  .db $75, $72, $20, $68, $61, $6e, $64, $73, $2c, $20, $62, $75, $74, $20, $69, $74
  .db $20, $64, $69, $73, $61, $70, $70, $65, $61, $72, $73, $21, $0a, $40, $48, $65
  .db $72, $65, $20, $69, $73, $20, $61, $20, $62, $6f, $6f, $6b, $20, $61, $62, $6f
  .db $75, $74, $20, $52, $6f, $62, $65, $72, $74, $20, $4b, $65, $6e, $6e, $65, $64
  .db $79, $2e, $0a, $40, $48, $65, $79, $2c, $20, $72, $6f, $62, $6f, $74, $2c, $20
  .db $6c, $65, $61, $76, $65, $20, $74, $68, $6f, $73, $65, $20, $6c, $69, $73, $74
  .db $73, $20, $61, $6c, $6f, $6e, $65, $2e, $0a, $40, $48, $6f, $20, $68, $75, $6d
  .db $2e, $20, $41, $6e, $6f, $74, $68, $65, $72, $20, $73, $79, $6e, $74, $68, $65
  .db $74, $69, $63, $20, $61, $20, $70, $6f, $73, $74, $65, $72, $69, $6f, $72, $69
  .db $2e, $0a, $40, $49, $74, $27, $73, $20, $41, $73, $69, $6d, $6f, $76, $27, $73
  .db $20, $4c, $61, $77, $73, $20, $6f, $66, $20, $52, $6f, $62, $6f, $74, $69, $63
  .db $73, $2e, $20, $59, $6f, $75, $20, $66, $65, $65, $6c, $20, $61, $20, $73, $74
  .db $72, $61, $6e, $67, $65, $20, $61, $66, $66, $69, $6e, $69, $74, $79, $20, $66
  .db $6f, $72, $20, $74, $68, $65, $6d, $2e, $40
  
  .bank 3
  .org $E000
  
strings3:
  .db $40, $49, $74, $27, $73, $20, $42, $61, $63, $68, $27, $73, $20, $4d, $61, $73
  .db $73, $20, $69, $6e, $20, $42, $2d, $6d, $69, $6e, $6f, $72, $21, $0a, $40, $49
  .db $74, $27, $73, $20, $61, $20, $62, $75, $67, $2e, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $20, $73, $79, $6e, $74, $68, $65, $74, $69, $63, $20, $61, $20, $70
  .db $72, $69, $6f, $72, $69, $20, $74, $72, $75, $74, $68, $21, $20, $49, $6d, $6d
  .db $61, $6e, $75, $65, $6c, $20, $77, $6f, $75, $6c, $64, $20, $62, $65, $20, $73
  .db $6f, $20, $70, $6c, $65, $61, $73, $65, $64, $21, $0a, $40, $49, $74, $27, $73
  .db $20, $74, $68, $65, $20, $54, $69, $6b, $69, $20, $52, $6f, $6f, $6d, $2e, $0a
  .db $40, $4a, $75, $73, $74, $20, $73, $6f, $6d, $65, $20, $6f, $6c, $64, $20, $70
  .db $6c, $61, $79, $20, $62, $79, $20, $61, $20, $43, $7a, $65, $63, $68, $20, $70
  .db $6c, $61, $79, $77, $72, $69, $67, $68, $74, $2c, $20, $61, $6e, $64, $20, $79
  .db $6f, $75, $20, $63, $61, $6e, $27, $74, $20, $72, $65, $61, $64, $20, $43, $7a
  .db $65, $63, $68, $2e, $0a, $40, $4b, $69, $74, $74, $65, $6e, $20, $69, $73, $20
  .db $74, $68, $65, $20, $6c, $65, $74, $74, $65, $72, $20, $27, $51, $27, $2e, $20
  .db $4f, $68, $2c, $20, $77, $61, $69, $74, $2c, $20, $6d, $61, $79, $62, $65, $20
  .db $6e, $6f, $74, $2e, $0a, $40, $51, $75, $69, $64, $71, $75, $69, $64, $20, $4c
  .db $61, $74, $69, $6e, $65, $20, $64, $69, $63, $74, $75, $6d, $20, $73, $69, $74
  .db $2c, $20, $6b, $69, $74, $74, $65, $6e, $20, $6e, $6f, $6e, $20, $65, $73, $74
  .db $2e, $0a, $40, $53, $75, $74, $72, $6f, $20, $54, $6f, $77, $65, $72, $20, $69
  .db $73, $20, $76, $69, $73, $69, $62, $6c, $65, $20, $61, $74, $20, $73, $6f, $6d
  .db $65, $20, $64, $69, $73, $74, $61, $6e, $63, $65, $20, $74, $68, $72, $6f, $75
  .db $67, $68, $20, $74, $68, $65, $20, $66, $6f, $67, $2e, $0a, $40, $54, $68, $65
  .db $20, $44, $69, $67, $69, $74, $61, $6c, $20, $4d, $69, $6c, $6c, $65, $6e, $6e
  .db $69, $75, $6d, $20, $43, $6f, $70, $79, $72, $69, $67, $68, $74, $20, $41, $63
  .db $74, $20, $6f, $66, $20, $31, $39, $39, $38, $2e, $0a, $40, $54, $68, $65, $20
  .db $55, $6e, $69, $74, $65, $64, $20, $53, $74, $61, $74, $65, $73, $20, $43, $6f
  .db $75, $72, $74, $20, $6f, $66, $20, $41, $70, $70, $65, $61, $6c, $73, $20, $66
  .db $6f, $72, $20, $74, $68, $65, $20, $46, $65, $64, $65, $72, $61, $6c, $20, $43
  .db $69, $72, $63, $75, $69, $74, $2e, $0a, $40, $54, $68, $65, $20, $6e, $6f, $6e
  .db $2d, $6b, $69, $74, $74, $65, $6e, $20, $69, $74, $65, $6d, $20, $6c, $69, $6b
  .db $65, $20, $74, $68, $69, $73, $20, $62, $75, $74, $20, $77, $69, $74, $68, $20
  .db $22, $66, $61, $6c, $73, $65, $22, $20, $61, $6e, $64, $20, $22, $74, $72, $75
  .db $65, $22, $20, $73, $77, $69, $74, $63, $68, $65, $64, $20, $69, $73, $20, $74
  .db $72, $75, $65, $2e, $0a, $40, $54, $68, $65, $20, $6e, $6f, $6e, $2d, $6b, $69
  .db $74, $74, $65, $6e, $20, $69, $74, $65, $6d, $20, $6c, $69, $6b, $65, $20, $74
  .db $68, $69, $73, $20, $62, $75, $74, $20, $77, $69, $74, $68, $20, $22, $74, $72
  .db $75, $65, $22, $20, $61, $6e, $64, $20, $22, $66, $61, $6c, $73, $65, $22, $20
  .db $73, $77, $69, $74, $63, $68, $65, $64, $20, $69, $73, $20, $66, $61, $6c, $73
  .db $65, $2e, $0a, $40, $54, $68, $69, $73, $20, $69, $73, $20, $74, $68, $65, $20
  .db $63, $68, $61, $70, $74, $65, $72, $20, $63, $61, $6c, $6c, $65, $64, $20, $22
  .db $41, $20, $4d, $61, $70, $20, $6f, $66, $20, $74, $68, $65, $20, $43, $61, $74
  .db $3f, $22, $20, $66, $72, $6f, $6d, $20, $46, $65, $79, $6e, $6d, $61, $6e, $27
  .db $73, $20, $61, $75, $74, $6f, $62, $69, $6f, $67, $72, $61, $70, $68, $79, $2e
  .db $0a, $40, $54, $68, $69, $73, $20, $69, $73, $20, $74, $68, $65, $20, $66, $6f
  .db $72, $65, $73, $74, $20, $70, $72, $69, $6d, $65, $76, $61, $6c, $2e, $0a, $40
  .db $57, $65, $72, $6e, $65, $72, $27, $73, $20, $22, $50, $6f, $63, $6b, $65, $74
  .db $20, $46, $69, $65, $6c, $64, $20, $47, $75, $69, $64, $65, $20, $74, $6f, $20
  .db $54, $68, $69, $6e, $67, $73, $20, $54, $68, $61, $74, $20, $41, $72, $65, $20
  .db $4e, $6f, $74, $20, $4b, $69, $74, $74, $65, $6e, $22, $2e, $0a, $40, $59, $6f
  .db $75, $20, $66, $6f, $75, $6e, $64, $20, $6e, $65, $74, $74, $69, $6b, $2c, $20
  .db $62, $75, $74, $20, $74, $68, $61, $74, $27, $73, $20, $62, $61, $63, $6b, $77
  .db $61, $72, $64, $73, $2e, $0a, $40, $59, $6f, $75, $20, $68, $61, $76, $65, $20
  .db $66, $6f, $75, $6e, $64, $20, $73, $6f, $6d, $65, $20, $7a, $69, $6e, $63, $2c
  .db $20, $62, $75, $74, $20, $79, $6f, $75, $20, $6d, $75, $73, $74, $20, $6e, $6f
  .db $74, $20, $73, $74, $6f, $70, $20, $68, $65, $72, $65, $2c, $20, $66, $6f, $72
  .db $20, $79, $6f, $75, $20, $6d, $75, $73, $74, $20, $66, $69, $6e, $64, $20, $6b
  .db $69, $74, $74, $65, $6e, $2e, $0a, $40, $22, $35, $30, $20, $59, $65, $61, $72
  .db $73, $20, $41, $6d, $6f, $6e, $67, $20, $74, $68, $65, $20, $4e, $6f, $6e, $2d
  .db $4b, $69, $74, $74, $65, $6e, $20, $49, $74, $65, $6d, $73, $22, $2c, $20, $62
  .db $79, $20, $41, $6e, $6e, $20, $44, $72, $6f, $79, $64, $2e, $0a, $40, $22, $52
  .db $6f, $62, $6f, $74, $20, $6d, $61, $79, $20, $6e, $6f, $74, $20, $69, $6e, $6a
  .db $75, $72, $65, $20, $6b, $69, $74, $74, $65, $6e, $2c, $20, $6f, $72, $2c, $20
  .db $74, $68, $72, $6f, $75, $67, $68, $20, $69, $6e, $61, $63, $74, $69, $6f, $6e
  .db $2c, $20, $2e, $2e, $2e, $22, $0a, $40, $22, $41, $64, $64, $72, $65, $73, $73
  .db $20, $41, $6c, $6c, $6f, $63, $61, $74, $69, $6f, $6e, $20, $66, $6f, $72, $20
  .db $50, $72, $69, $76, $61, $74, $65, $20, $49, $6e, $74, $65, $72, $6e, $65, $74
  .db $73, $22, $20, $62, $79, $20, $59, $61, $6b, $6f, $76, $20, $52, $65, $6b, $68
  .db $74, $65, $72, $20, $65, $74, $20, $61, $6c, $2e, $0a, $40, $22, $4d, $61, $69
  .db $6c, $20, $52, $6f, $75, $74, $69, $6e, $67, $20, $61, $6e, $64, $20, $74, $68
  .db $65, $20, $44, $6f, $6d, $61, $69, $6e, $20, $53, $79, $73, $74, $65, $6d, $22
  .db $20, $62, $79, $20, $43, $72, $61, $69, $67, $20, $50, $61, $72, $74, $72, $69
  .db $64, $67, $65, $2e, $0a, $40, $22, $54, $68, $65, $20, $54, $68, $65, $6f, $72
  .db $79, $20, $61, $6e, $64, $20, $50, $72, $61, $63, $74, $69, $63, $65, $20, $6f
  .db $66, $20, $4f, $6c, $69, $67, $61, $72, $63, $68, $69, $63, $61, $6c, $20, $43
  .db $6f, $6c, $6c, $65, $63, $74, $69, $76, $69, $73, $6d, $22, $20, $62, $79, $20
  .db $45, $6d, $6d, $61, $6e, $75, $65, $6c, $20, $47, $6f, $6c, $64, $73, $74, $65
  .db $69, $6e, $2e, $0a, $40, $22, $32, $30, $31, $20, $4b, $69, $74, $74, $65, $6e
  .db $20, $56, $65, $72, $62, $73, $2c, $20, $46, $75, $6c, $6c, $79, $20, $43, $6f
  .db $6e, $6a, $75, $67, $61, $74, $65, $64, $22, $2e, $20, $59, $6f, $75, $20, $6c
  .db $6f, $6f, $6b, $20, $66, $6f, $72, $20, $22, $66, $69, $6e, $64, $22, $2e, $0a
  .db $40, $41, $20, $63, $61, $72, $64, $20, $73, $68, $61, $72, $6b, $20, $73, $69
  .db $74, $73, $20, $68, $65, $72, $65, $2c, $20, $70, $72, $61, $63, $74, $69, $63
  .db $69, $6e, $67, $20, $68, $69, $73, $20, $46, $61, $72, $6f, $20, $73, $68, $75
  .db $66, $66, $6c, $65, $2e, $20, $48, $65, $20, $69, $67, $6e, $6f, $72, $65, $73
  .db $20, $79, $6f, $75, $2e, $0a, $40, $41, $20, $63, $6f, $70, $79, $20, $6f, $66
  .db $20, $44, $65, $43, $53, $53, $2e, $20, $54, $68, $65, $79, $27, $72, $65, $20
  .db $61, $20, $64, $69, $6d, $65, $20, $61, $20, $64, $6f, $7a, $65, $6e, $20, $74
  .db $68, $65, $73, $65, $20, $64, $61, $79, $73, $2e, $0a, $40, $41, $20, $64, $65
  .db $6d, $6f, $6e, $69, $63, $20, $76, $6f, $69, $63, $65, $20, $70, $72, $6f, $63
  .db $6c, $61, $69, $6d, $73, $20, $22, $54, $68, $65, $72, $65, $20, $69, $73, $20
  .db $6e, $6f, $20, $6b, $69, $74, $74, $65, $6e, $2c, $20, $6f, $6e, $6c, $79, $20
  .db $5a, $75, $75, $6c, $22, $2e, $20, $59, $6f, $75, $20, $66, $6c, $65, $65, $2e
  .db $0a, $40, $41, $20, $6c, $6f, $74, $75, $73, $2e, $20, $59, $6f, $75, $20, $6d
  .db $61, $6b, $65, $20, $61, $6e, $20, $69, $6e, $74, $65, $72, $65, $73, $74, $69
  .db $6e, $67, $20, $70, $61, $69, $72, $2e, $0a, $40, $49, $74, $27, $73, $20, $74
  .db $68, $65, $20, $6d, $69, $73, $73, $69, $6e, $67, $20, $32, $34, $20, $73, $65
  .db $63, $6f, $6e, $64, $73, $20, $6f, $66, $20, $22, $4d, $6f, $6e, $74, $79, $20
  .db $50, $79, $74, $68, $6f, $6e, $20, $61, $6e, $64, $20, $74, $68, $65, $20, $48
  .db $6f, $6c, $79, $20, $47, $72, $61, $69, $6c, $22, $2e, $0a, $40, $41, $20, $6d
  .db $69, $6c, $6b, $20, $63, $61, $72, $74, $6f, $6e, $2c, $20, $77, $69, $74, $68
  .db $20, $61, $20, $62, $6c, $61, $63, $6b, $20, $61, $6e, $64, $20, $77, $68, $69
  .db $74, $65, $20, $70, $69, $63, $74, $75, $72, $65, $20, $6f, $66, $20, $6b, $69
  .db $74, $74, $65, $6e, $20, $6f, $6e, $20, $74, $68, $65, $20, $73, $69, $64, $65
  .db $2e, $0a, $40, $41, $6e, $79, $20, $6f, $72, $64, $69, $6e, $61, $72, $79, $20
  .db $72, $6f, $62, $6f, $74, $20, $63, $6f, $75, $6c, $64, $20, $73, $65, $65, $20
  .db $66, $72, $6f, $6d, $20, $61, $20, $6d, $69, $6c, $65, $20, $61, $77, $61, $79
  .db $20, $74, $68, $61, $74, $20, $74, $68, $69, $73, $20, $77, $61, $73, $6e, $27
  .db $74, $20, $6b, $69, $74, $74, $65, $6e, $2e, $0a, $40, $41, $20, $73, $74, $65
  .db $67, $6f, $73, $61, $75, $72, $75, $73, $2c, $20, $65, $73, $63, $61, $70, $65
  .db $64, $20, $66, $72, $6f, $6d, $20, $74, $68, $65, $20, $73, $74, $65, $67, $6f
  .db $73, $61, $75, $72, $75, $73, $66, $69, $6e, $64, $73, $72, $6f, $62, $6f, $74
  .db $20, $67, $61, $6d, $65, $2e, $20, $49, $74, $20, $66, $69, $6e, $64, $73, $20
  .db $79, $6f, $75, $2e, $0a, $40, $42, $61, $6c, $69, $6e, $67, $20, $77, $69, $72
  .db $65, $20, $61, $6e, $64, $20, $63, $68, $65, $77, $69, $6e, $67, $20, $67, $75
  .db $6d, $2e, $0a, $40, $43, $68, $65, $77, $69, $6e, $67, $20, $67, $75, $6d, $20
  .db $61, $6e, $64, $20, $62, $61, $6c, $69, $6e, $67, $20, $77, $69, $72, $65, $2e
  .db $0a, $40, $48, $65, $72, $65, $20, $69, $73, $20, $6e, $6f, $20, $6b, $69, $74
  .db $74, $65, $6e, $20, $62, $75, $74, $20, $6f, $6e, $6c, $79, $20, $72, $6f, $63
  .db $6b, $2c, $20, $72, $6f, $63, $6b, $20, $61, $6e, $64, $20, $6e, $6f, $20, $6b
  .db $69, $74, $74, $65, $6e, $20, $61, $6e, $64, $20, $74, $68, $65, $20, $73, $61
  .db $6e, $64, $79, $20, $72, $6f, $61, $64, $2e, $0a, $40, $48, $65, $79, $2c, $20
  .db $49, $20, $62, $65, $74, $20, $79, $6f, $75, $20, $74, $68, $6f, $75, $67, $68
  .db $74, $20, $74, $68, $69, $73, $20, $77, $61, $73, $20, $6b, $69, $74, $74, $65
  .db $6e, $2e, $0a, $40, $49, $74, $20, $69, $73, $20, $61, $6e, $20, $61, $6e, $63
  .db $69, $65, $6e, $74, $20, $6d, $61, $72, $69, $6e, $65, $72, $2c, $20, $61, $6e
  .db $64, $20, $68, $65, $20, $73, $74, $6f, $70, $70, $65, $74, $68, $20, $6f, $6e
  .db $65, $20, $6f, $66, $20, $74, $68, $72, $65, $65, $2e, $0a, $40, $49, $74, $20
  .db $70, $6c, $65, $61, $73, $65, $73, $20, $79, $6f, $75, $20, $74, $6f, $20, $62
  .db $65, $20, $6b, $69, $6e, $64, $20, $74, $6f, $20, $77, $68, $61, $74, $20, $61
  .db $70, $70, $65, $61, $72, $73, $20, $74, $6f, $20, $62, $65, $20, $6b, $69, $74
  .db $74, $65, $6e, $20, $2d, $2d, $20, $62, $75, $74, $20, $69, $74, $27, $73, $20
  .db $6e, $6f, $74, $21, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $62, $6c, $61
  .db $74, $61, $6e, $74, $20, $70, $6c, $75, $67, $20, $66, $6f, $72, $20, $4f, $67
  .db $67, $20, $56, $6f, $72, $62, $69, $73, $2c, $20, $68, $74, $74, $70, $3a, $2f
  .db $2f, $77, $77, $77, $2e, $76, $6f, $72, $62, $69, $73, $2e, $63, $6f, $6d, $2f
  .db $0a, $40, $49, $74, $27, $73, $20, $61, $20, $62, $75, $73, $69, $6e, $65, $73
  .db $73, $20, $70, $6c, $61, $6e, $20, $66, $6f, $72, $20, $61, $20, $6e, $65, $77
  .db $20, $73, $74, $61, $72, $74, $75, $70, $2c, $20, $6b, $69, $74, $74, $65, $6e
  .db $2e, $6e, $65, $74, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $72, $65
  .db $76, $69, $73, $65, $64, $20, $62, $75, $73, $69, $6e, $65, $73, $73, $20, $70
  .db $6c, $61, $6e, $20, $66, $6f, $72, $20, $61, $20, $6e, $65, $77, $20, $73, $74
  .db $61, $72, $74, $75, $70, $2c, $20, $6d, $79, $2e, $6b, $69, $74, $74, $65, $6e
  .db $2e, $6e, $65, $74, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $73, $71
  .db $75, $61, $72, $65, $2e, $0a, $40, $49, $74, $20, $73, $65, $65, $6d, $73, $20
  .db $74, $6f, $20, $62, $65, $20, $61, $20, $63, $6f, $70, $79, $20, $6f, $66, $20
  .db $22, $41, $20, $54, $61, $69, $6c, $20, $6f, $66, $20, $54, $77, $6f, $20, $4b
  .db $69, $74, $74, $69, $65, $73, $22, $2e, $0a, $40, $49, $74, $27, $73, $20, $74
  .db $68, $65, $20, $44, $6f, $6e, $61, $74, $69, $6f, $6e, $20, $6f, $66, $20, $43
  .db $6f, $6e, $73, $74, $61, $6e, $74, $69, $6e, $65, $21, $0a, $40, $49, $74, $27
  .db $73, $20, $74, $68, $69, $73, $20, $6d, $65, $73, $73, $61, $67, $65, $2c, $20
  .db $6e, $6f, $74, $68, $69, $6e, $67, $20, $6d, $6f, $72, $65, $2e, $0a, $40, $4c
  .db $79, $73, $69, $6e, $65, $2c, $20, $61, $6e, $20, $65, $73, $73, $65, $6e, $74
  .db $69, $61, $6c, $20, $61, $6d, $69, $6e, $6f, $20, $61, $63, $69, $64, $2e, $20
  .db $57, $65, $6c, $6c, $2c, $20, $6d, $61, $79, $62, $65, $20, $6e, $6f, $74, $20
  .db $66, $6f, $72, $20, $72, $6f, $62, $6f, $74, $73, $2e, $0a, $40, $4e, $6f, $20
  .db $6b, $69, $74, $74, $65, $6e, $20, $68, $65, $72, $65, $2e, $0a, $40, $54, $68
  .db $65, $20, $73, $63, $6f, $72, $65, $20, $66, $6f, $72, $20, $61, $20, $43, $7a
  .db $65, $63, $68, $20, $63, $6f, $6d, $70, $6f, $73, $65, $72, $27, $73, $20, $22
  .db $4b, $69, $74, $74, $65, $6e, $2d, $46, $69, $6e, $64, $69, $6e, $67, $20, $53
  .db $79, $6d, $70, $68, $6f, $6e, $79, $20, $69, $6e, $20, $43, $22, $2e, $0a, $40
  .db $54, $68, $69, $73, $20, $6c, $6f, $6f, $6b, $73, $20, $6c, $69, $6b, $65, $20
  .db $42, $72, $61, $64, $6c, $65, $79, $27, $73, $20, $22, $41, $70, $70, $65, $61
  .db $72, $61, $6e, $63, $65, $20, $61, $6e, $64, $20, $52, $65, $61, $6c, $69, $74
  .db $79, $22, $2c, $20, $62, $75, $74, $20, $69, $74, $27, $73, $20, $72, $65, $61
  .db $6c, $6c, $79, $20, $6e, $6f, $74, $2e, $0a, $40, $54, $68, $69, $73, $20, $6e
  .db $6f, $6e, $2d, $6b, $69, $74, $74, $65, $6e, $20, $69, $74, $65, $6d, $20, $6e
  .db $6f, $20, $76, $65, $72, $62, $2e, $0a, $40, $59, $6f, $75, $20, $66, $65, $65
  .db $6c, $20, $73, $74, $72, $61, $6e, $67, $65, $6c, $79, $20, $75, $6e, $66, $75
  .db $6c, $66, $69, $6c, $6c, $65, $64, $2e, $0a, $40, $59, $6f, $75, $20, $68, $69
  .db $74, $20, $74, $68, $65, $20, $6e, $6f, $6e, $2d, $6b, $69, $74, $74, $65, $6e
  .db $20, $69, $74, $65, $6d, $2e, $20, $54, $68, $65, $20, $6e, $6f, $6e, $2d, $6b
  .db $69, $74, $74, $65, $6e, $20, $69, $74, $65, $6d, $20, $66, $61, $69, $6c, $73
  .db $20, $74, $6f, $20, $79, $6f, $77, $6c, $2e, $0a, $40, $59, $6f, $75, $20, $73
  .db $75, $64, $64, $65, $6e, $6c, $79, $20, $79, $65, $61, $72, $6e, $20, $66, $6f
  .db $72, $20, $79, $6f, $75, $72, $20, $64, $69, $73, $74, $61, $6e, $74, $20, $68
  .db $6f, $6d, $65, $6c, $61, $6e, $64, $2e, $0a, $40, $59, $6f, $75, $27, $76, $65
  .db $20, $66, $6f, $75, $6e, $64, $20, $74, $68, $65, $20, $73, $6e, $6f, $77, $73
  .db $20, $6f, $66, $20, $79, $65, $73, $74, $65, $72, $79, $65, $61, $72, $21, $20
  .db $53, $6f, $20, $74, $68, $61, $74, $27, $73, $20, $77, $68, $65, $72, $65, $20
  .db $74, $68, $65, $79, $20, $61, $6c, $6c, $20, $77, $65, $6e, $74, $20, $74, $6f
  .db $2e, $0a, $40, $41, $70, $70, $72, $6f, $61, $63, $68, $69, $6e, $67, $2e, $20
  .db $4f, $6e, $65, $20, $63, $61, $72, $2e, $20, $4a, $2e, $20, $46, $6f, $6c, $6c
  .db $6f, $77, $65, $64, $20, $62, $79, $2e, $20, $54, $77, $6f, $20, $63, $61, $72
  .db $2e, $20, $4d, $2c, $20, $4d, $2e, $20, $49, $6e, $20, $66, $69, $76, $65, $2e
  .db $20, $4d, $69, $6e, $75, $74, $65, $73, $2e, $0a, $40, $46, $72, $65, $65, $20
  .db $4a, $6f, $6e, $20, $4a, $6f, $68, $61, $6e, $73, $65, $6e, $21, $0a, $40, $46
  .db $72, $65, $65, $20, $44, $6d, $69, $74, $72, $79, $20, $53, $6b, $6c, $79, $61
  .db $72, $6f, $76, $21, $0a, $40, $4f, $6e, $65, $20, $70, $65, $72, $73, $6f, $6e
  .db $20, $73, $68, $6f, $75, $74, $73, $20, $22, $57, $68, $61, $74, $20, $64, $6f
  .db $20, $77, $65, $20, $77, $61, $6e, $74, $3f, $22, $20, $54, $68, $65, $20, $63
  .db $72, $6f, $77, $64, $20, $61, $6e, $73, $77, $65, $72, $73, $20, $22, $46, $72
  .db $65, $65, $20, $44, $6d, $69, $74, $72, $79, $21, $22, $0a, $40, $4a, $75, $64
  .db $69, $74, $68, $20, $50, $6c, $61, $74, $74, $20, $69, $6e, $73, $75, $6c, $74
  .db $73, $20, $6c, $69, $62, $72, $61, $72, $69, $61, $6e, $73, $2e, $0a, $40, $54
  .db $68, $69, $73, $20, $6d, $61, $70, $20, $69, $73, $20, $6e, $6f, $74, $20, $74
  .db $68, $65, $20, $74, $65, $72, $72, $69, $74, $6f, $72, $79, $2e, $0a, $40, $22
  .db $47, $6f, $20, $62, $61, $63, $6b, $20, $74, $6f, $20, $4c, $69, $62, $72, $61
  .db $72, $69, $61, $21, $22, $2c, $20, $73, $61, $79, $73, $20, $50, $61, $74, $20
  .db $53, $63, $68, $72, $6f, $65, $64, $65, $72, $2e, $0a, $40, $54, $68, $69, $73
  .db $20, $69, $73, $20, $61, $20, $70, $6f, $72, $63, $65, $6c, $61, $69, $6e, $20
  .db $6b, $69, $74, $74, $65, $6e, $2d, $63, $6f, $75, $6e, $74, $65, $72, $2e, $20
  .db $30, $2c, $20, $30, $2c, $20, $30, $2c, $20, $30, $2c, $20, $30, $2e, $2e, $2e
  .db $0a, $40, $41, $6e, $20, $6f, $6c, $64, $20, $62, $6f, $6f, $74, $61, $62, $6c
  .db $65, $20, $62, $75, $73, $69, $6e, $65, $73, $73, $20, $63, $61, $72, $64, $2c
  .db $20, $75, $6e, $66, $6f, $72, $74, $75, $6e, $61, $74, $65, $6c, $79, $20, $63
  .db $72, $61, $63, $6b, $65, $64, $20, $64, $6f, $77, $6e, $20, $74, $68, $65, $20
  .db $6d, $69, $64, $64, $6c, $65, $2e, $0a, $40, $41, $20, $6b, $69, $74, $74, $65
  .db $6e, $20, $73, $69, $6e, $6b, $2c, $20, $66, $6f, $72, $20, $77, $61, $73, $68
  .db $69, $6e, $67, $20, $6b, $69, $74, $74, $65, $6e, $20, $28, $69, $66, $20, $6f
  .db $6e, $6c, $79, $20, $6b, $69, $74, $74, $65, $6e, $20, $6c, $69, $6b, $65, $64
  .db $20, $77, $61, $74, $65, $72, $29, $2e, $0a, $40, $41, $20, $6b, $69, $74, $74
  .db $65, $6e, $20, $73, $6f, $75, $72, $63, $65, $20, $28, $74, $6f, $20, $6d, $61
  .db $74, $63, $68, $20, $74, $68, $65, $20, $6b, $69, $74, $74, $65, $6e, $20, $73
  .db $69, $6e, $6b, $29, $2e, $0a, $40, $49, $66, $20, $69, $74, $27, $73, $20, $6f
  .db $6e, $65, $20, $74, $68, $69, $6e, $67, $2c, $20, $69, $74, $27, $73, $20, $6e
  .db $6f, $74, $20, $61, $6e, $6f, $74, $68, $65, $72, $2e, $0a, $40, $49, $66, $20
  .db $69, $74, $27, $73, $20, $6e, $6f, $74, $20, $6f, $6e, $65, $20, $74, $68, $69
  .db $6e, $67, $2c, $20, $69, $74, $27, $73, $20, $61, $6e, $6f, $74, $68, $65, $72
  .db $2e, $0a, $40, $41, $20, $63, $61, $62, $6f, $6f, $64, $6c, $65, $2e, $0a, $40
  .db $41, $20, $67, $72, $69, $6e, $2e, $0a, $40, $41, $20, $68, $65, $64, $67, $65
  .db $68, $6f, $67, $2e, $20, $49, $74, $20, $6c, $6f, $6f, $6b, $73, $20, $6c, $69
  .db $6b, $65, $20, $69, $74, $20, $6b, $6e, $6f, $77, $73, $20, $73, $6f, $6d, $65
  .db $74, $68, $69, $6e, $67, $20, $69, $6d, $70, $6f, $72, $74, $61, $6e, $74, $2e
  .db $0a, $40, $59, $6f, $75, $27, $76, $65, $20, $66, $6f, $75, $6e, $64, $2e, $2e
  .db $2e, $20, $4f, $68, $20, $77, $61, $69, $74, $2c, $20, $74, $68, $61, $74, $27
  .db $73, $20, $6a, $75, $73, $74, $20, $61, $20, $63, $61, $74, $2e, $0a, $40, $52
  .db $6f, $62, $6f, $74, $20, $73, $68, $6f, $75, $6c, $64, $20, $6e, $6f, $74, $20
  .db $62, $65, $20, $74, $6f, $75, $63, $68, $69, $6e, $67, $20, $74, $68, $61, $74
  .db $2e, $0a, $40, $41, $69, $72, $20, $47, $75, $69, $74, $61, $72, $21, $21, $21
  .db $20, $4e, $41, $20, $6e, $61, $20, $4e, $41, $20, $6e, $61, $21, $21, $0a, $40
  .db $41, $6e, $20, $61, $72, $6f, $6d, $61, $74, $68, $65, $72, $61, $70, $79, $20
  .db $63, $61, $6e, $64, $6c, $65, $20, $62, $75, $72, $6e, $73, $20, $77, $69, $74
  .db $68, $20, $68, $65, $61, $6c, $69, $6e, $67, $20, $6c, $69, $67, $68, $74, $2e
  .db $0a, $40, $59, $6f, $75, $20, $66, $69, $6e, $64, $20, $61, $20, $62, $72, $69
  .db $67, $68, $74, $20, $73, $68, $69, $6e, $79, $20, $70, $65, $6e, $6e, $79, $2e
  .db $0a, $40, $49, $74, $27, $73, $20, $61, $20, $66, $72, $65, $65, $20, $4a, $6f
  .db $6e, $20, $4a, $6f, $68, $61, $6e, $73, $65, $6e, $21, $0a, $40, $49, $74, $27
  .db $73, $20, $61, $20, $66, $72, $65, $65, $20, $44, $6d, $69, $74, $72, $79, $20
  .db $53, $6b, $6c, $79, $61, $72, $6f, $76, $21, $0a, $40, $54, $68, $65, $20, $72
  .db $6f, $74, $68, $65, $20, $68, $69, $74, $73, $21, $20, $54, $68, $65, $20, $72
  .db $6f, $74, $68, $65, $20, $68, $69, $74, $73, $21, $0a, $40, $49, $74, $27, $73
  .db $20, $61, $6e, $20, $49, $6e, $74, $65, $72, $6e, $65, $74, $20, $63, $68, $61
  .db $69, $6e, $20, $6c, $65, $74, $74, $65, $72, $20, $61, $62, $6f, $75, $74, $20
  .db $73, $6f, $64, $69, $75, $6d, $20, $6c, $61, $75, $72, $65, $74, $68, $20, $73
  .db $75, $6c, $66, $61, $74, $65, $2e, $0a, $40, $45, $64, $20, $57, $69, $74, $74
  .db $65, $6e, $20, $73, $69, $74, $73, $20, $68, $65, $72, $65, $2c, $20, $70, $6f
  .db $6e, $64, $65, $72, $69, $6e, $67, $20, $73, $74, $72, $69, $6e, $67, $20, $74
  .db $68, $65, $6f, $72, $79, $2e, $0a, $40, $53, $6f, $6d, $65, $74, $68, $69, $6e
  .db $67, $20, $69, $73, $20, $77, $72, $69, $74, $74, $65, $6e, $20, $68, $65, $72
  .db $65, $20, $69, $6e, $20, $74, $68, $65, $20, $64, $75, $73, $74, $2e, $20, $59
  .db $6f, $75, $20, $72, $65, $61, $64, $3a, $20, $22, $72, $4a, $62, $6f, $74, $66
  .db $20, $6e, $64, $51, $6b, $74, $74, $74, $65, $6e, $22, $2e, $0a, $40, $57, $65
  .db $20, $77, $69, $73, $68, $20, $79, $6f, $75, $20, $61, $20, $6d, $65, $72, $72
  .db $79, $20, $6b, $69, $74, $74, $65, $6e, $2c, $20, $61, $6e, $64, $20, $61, $20
  .db $68, $61, $70, $70, $79, $20, $4e, $65, $77, $20, $59, $65, $61, $72, $21, $0a
  .db $40, $52, $75, $6e, $20, $61, $77, $61, $79, $21, $20, $52, $75, $6e, $20, $61
  .db $77, $61, $79, $21, $0a, $40, $59, $6f, $75, $20, $63, $61, $6e, $20, $73, $65
  .db $65, $20, $72, $69, $67, $68, $74, $20, $74, $68, $72, $6f, $75, $67, $68, $20
  .db $74, $68, $69, $73, $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $42, $72, $69
  .db $6e, $27, $73, $20, $22, $54, $72, $61, $6e, $73, $70, $61, $72, $65, $6e, $74
  .db $20, $53, $6f, $63, $69, $65, $74, $79, $22, $2e, $0a, $40, $54, $68, $69, $73
  .db $20, $63, $6f, $70, $79, $20, $6f, $66, $20, $22, $53, $74, $65, $61, $6c, $20
  .db $54, $68, $69, $73, $20, $42, $6f, $6f, $6b, $22, $20, $68, $61, $73, $20, $62
  .db $65, $65, $6e, $20, $73, $74, $6f, $6c, $65, $6e, $20, $66, $72, $6f, $6d, $20
  .db $61, $20, $62, $6f, $6f, $6b, $73, $74, $6f, $72, $65, $2e, $0a, $40, $49, $74
  .db $27, $73, $20, $52, $6f, $79, $61, $20, $4e, $61, $69, $6e, $69, $2e, $0a, $40
  .db $54, $68, $69, $73, $20, $6b, $69, $74, $20, $69, $73, $20, $74, $68, $65, $20
  .db $66, $6f, $75, $72, $74, $65, $65, $6e, $74, $68, $20, $69, $6e, $20, $61, $20
  .db $73, $65, $72, $69, $65, $73, $20, $6f, $66, $20, $6b, $69, $74, $73, $20, $6e
  .db $61, $6d, $65, $64, $20, $77, $69, $74, $68, $20, $52, $6f, $6d, $61, $6e, $20
  .db $6c, $65, $74, $74, $65, $72, $73, $2e, $0a, $40, $54, $68, $69, $73, $20, $69
  .db $73, $20, $74, $68, $65, $20, $74, $65, $6e, $74, $68, $20, $6b, $65, $79, $20
  .db $79, $6f, $75, $27, $76, $65, $20, $66, $6f, $75, $6e, $64, $20, $73, $6f, $20
  .db $66, $61, $72, $2e, $0a, $40, $59, $6f, $75, $20, $66, $69, $6e, $64, $20, $61
  .db $20, $66, $72, $61, $75, $64, $20, $73, $63, $68, $65, $6d, $65, $20, $69, $6e
  .db $20, $77, $68, $69, $63, $68, $20, $6c, $6f, $61, $6e, $73, $20, $61, $72, $65
  .db $20, $75, $73, $65, $64, $20, $61, $73, $20, $73, $65, $63, $75, $72, $69, $74
  .db $79, $20, $66, $6f, $72, $20, $6f, $74, $68, $65, $72, $20, $6c, $6f, $61, $6e
  .db $73, $2e, $0a, $40, $49, $74, $27, $73, $20, $74, $68, $65, $20, $70, $68, $72
  .db $61, $73, $65, $20, $22, $61, $6e, $64, $20, $68, $65, $72, $22, $2c, $20, $77
  .db $72, $69, $74, $74, $65, $6e, $20, $69, $6e, $20, $61, $6e, $63, $69, $65, $6e
  .db $74, $20, $47, $72, $65, $65, $6b, $2e, $0a, $40, $49, $74, $27, $73, $20, $74
  .db $68, $65, $20, $61, $75, $74, $68, $6f, $72, $20, $6f, $66, $20, $22, $52, $61
  .db $6e, $64, $6f, $6d, $6e, $65, $73, $73, $20, $61, $6e, $64, $20, $4d, $61, $74
  .db $68, $65, $6d, $61, $74, $69, $63, $61, $6c, $20, $50, $72, $6f, $6f, $66, $22
  .db $2e, $0a, $40, $49, $74, $27, $73, $20, $74, $68, $65, $20, $63, $72, $75, $73
  .db $74, $79, $20, $65, $78, $6f, $73, $6b, $65, $6c, $65, $74, $6f, $6e, $20, $6f
  .db $66, $20, $61, $6e, $20, $61, $72, $74, $68, $72, $6f, $70, $6f, $64, $21, $0a
  .db $40, $49, $74, $27, $73, $20, $45, $6d, $70, $6f, $72, $65, $72, $20, $53, $68
  .db $61, $64, $64, $61, $6d, $20, $74, $68, $65, $20, $34, $74, $68, $27, $73, $20
  .db $70, $6c, $61, $6e, $65, $74, $21, $0a, $40, $49, $74, $27, $73, $20, $74, $68
  .db $65, $20, $74, $72, $69, $61, $6e, $67, $6c, $65, $20, $6c, $65, $67, $20, $61
  .db $64, $6a, $61, $63, $65, $6e, $74, $20, $74, $6f, $20, $61, $6e, $20, $61, $6e
  .db $67, $6c, $65, $20, $64, $69, $76, $69, $64, $65, $64, $20, $62, $79, $20, $74
  .db $68, $65, $20, $6c, $65, $67, $20, $6f, $70, $70, $6f, $73, $69, $74, $65, $20
  .db $69, $74, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $62, $6f, $74, $74
  .db $6c, $65, $20, $6f, $66, $20, $6e, $61, $69, $6c, $20, $70, $6f, $6c, $69, $73
  .db $68, $20, $72, $65, $6d, $6f, $76, $65, $72, $2e, $0a, $40, $59, $6f, $75, $20
  .db $66, $6f, $75, $6e, $64, $20, $6e, $65, $74, $6b, $69, $74, $21, $20, $57, $61
  .db $79, $20, $74, $6f, $20, $67, $6f, $2c, $20, $72, $6f, $62, $6f, $74, $21, $0a
  .db $40, $49, $74, $27, $73, $20, $74, $68, $65, $20, $41, $53, $43, $49, $49, $20
  .db $46, $6c, $6f, $61, $74, $69, $6e, $67, $20, $48, $65, $61, $64, $20, $6f, $66
  .db $20, $53, $65, $74, $68, $20, $44, $61, $76, $69, $64, $20, $53, $63, $68, $6f
  .db $65, $6e, $21, $0a, $40, $41, $20, $66, $72, $6f, $73, $74, $65, $64, $20, $70
  .db $69, $6e, $6b, $20, $70, $61, $72, $74, $79, $2d, $63, $61, $6b, $65, $2c, $20
  .db $68, $61, $6c, $66, $20, $65, $61, $74, $65, $6e, $2e, $0a, $40, $41, $20, $62
  .db $69, $74, $63, $68, $69, $6e, $27, $20, $68, $6f, $6d, $65, $6d, $61, $64, $65
  .db $20, $74, $65, $73, $6c, $61, $20, $63, $6f, $69, $6c, $2e, $0a, $40, $43, $6f
  .db $6e, $61, $6e, $20, $4f, $27, $42, $72, $69, $61, $6e, $2c, $20, $73, $61, $6e
  .db $73, $20, $6a, $61, $77, $62, $6f, $6e, $65, $2e, $0a, $40, $49, $74, $27, $73
  .db $20, $65, $69, $74, $68, $65, $72, $20, $61, $20, $6d, $69, $72, $72, $6f, $72
  .db $2c, $20, $6f, $72, $20, $61, $6e, $6f, $74, $68, $65, $72, $20, $73, $6f, $75
  .db $6c, $6c, $65, $73, $73, $20, $6b, $69, $74, $74, $65, $6e, $2d, $73, $65, $65
  .db $6b, $69, $6e, $67, $20, $72, $6f, $62, $6f, $74, $2e, $0a, $40, $50, $72, $65
  .db $6f, $63, $63, $75, $70, $61, $74, $69, $6f, $6e, $20, $77, $69, $74, $68, $20
  .db $66, $69, $6e, $64, $69, $6e, $67, $20, $6b, $69, $74, $74, $65, $6e, $20, $70
  .db $72, $65, $76, $65, $6e, $74, $73, $20, $79, $6f, $75, $20, $66, $72, $6f, $6d
  .db $20, $69, $6e, $76, $65, $73, $74, $69, $67, $61, $74, $69, $6e, $67, $20, $66
  .db $75, $72, $74, $68, $65, $72, $2e, $0a, $40, $46, $6f, $6e, $7a, $69, $65, $20
  .db $73, $69, $74, $73, $20, $68, $65, $72, $65, $2c, $20, $6d, $75, $6d, $62, $6c
  .db $69, $6e, $67, $20, $69, $6e, $63, $6f, $68, $65, $72, $65, $6e, $74, $6c, $79
  .db $20, $61, $62, $6f, $75, $74, $20, $61, $20, $73, $68, $61, $72, $6b, $20, $61
  .db $6e, $64, $20, $61, $20, $70, $61, $69, $72, $20, $6f, $66, $20, $77, $61, $74
  .db $65, $72, $73, $6b, $69, $73, $2e, $0a, $40, $54, $68, $65, $20, $67, $68, $6f
  .db $73, $74, $20, $6f, $66, $20, $79, $6f, $75, $72, $20, $64, $61, $6e, $63, $65
  .db $20, $69, $6e, $73, $74, $72, $75, $63, $74, $6f, $72, $2c, $20, $68, $69, $73
  .db $20, $66, $61, $63, $65, $20, $61, $20, $70, $61, $70, $65, $72, $2d, $77, $68
  .db $69, $74, $65, $20, $6d, $61, $73, $6b, $20, $6f, $66, $20, $65, $76, $69, $6c
  .db $2e, $0a, $40, $41, $20, $62, $61, $67, $20, $6f, $66, $20, $67, $72, $6f, $63
  .db $65, $72, $69, $65, $73, $20, $74, $61, $6b, $65, $6e, $20, $6f, $66, $66, $20
  .db $74, $68, $65, $20, $73, $68, $65, $6c, $66, $20, $62, $65, $66, $6f, $72, $65
  .db $20, $74, $68, $65, $20, $65, $78, $70, $69, $72, $61, $74, $69, $6f, $6e, $20
  .db $64, $61, $74, $65, $2e, $0a, $40, $41, $20, $62, $6f, $6f, $6b, $3a, $20, $46
  .db $65, $6e, $67, $20, $53, $68, $75, $69, $2c, $20, $5a, $65, $6e, $3a, $20, $74
  .db $68, $65, $20, $61, $72, $74, $20, $6f, $66, $20, $72, $61, $6e, $64, $6f, $6d
  .db $6c, $79, $20, $61, $72, $72, $61, $6e, $67, $69, $6e, $67, $20, $69, $74, $65
  .db $6d, $73, $20, $74, $68, $61, $74, $20, $61, $72, $65, $20, $6e, $6f, $74, $20
  .db $6b, $69, $74, $74, $65, $6e, $2e, $0a, $40, $54, $68, $69, $73, $20, $6d, $69
  .db $67, $68, $74, $20, $62, $65, $20, $74, $68, $65, $20, $66, $6f, $75, $6e, $74
  .db $61, $69, $6e, $20, $6f, $66, $20, $79, $6f, $75, $74, $68, $2c, $20, $62, $75
  .db $74, $20, $79, $6f, $75, $27, $6c, $6c, $20, $6e, $65, $76, $65, $72, $20, $6b
  .db $6e, $6f, $77, $2e, $0a, $40, $54, $69, $67, $65, $72, $62, $6f, $74, $20, $48
  .db $65, $73, $68, $2e, $0a, $40, $53, $74, $69, $6d, $75, $74, $61, $63, $73, $2e
  .db $0a, $40, $41, $20, $63, $61, $6e, $69, $73, $74, $65, $72, $20, $6f, $66, $20
  .db $70, $72, $65, $73, $73, $75, $72, $69, $7a, $65, $64, $20, $77, $68, $69, $70
  .db $70, $65, $64, $20, $63, $72, $65, $61, $6d, $2c, $20, $73, $61, $6e, $73, $20
  .db $77, $68, $69, $70, $70, $65, $64, $20, $63, $72, $65, $61, $6d, $2e, $0a, $40
  .db $54, $68, $65, $20, $6e, $6f, $6e, $2d, $6b, $69, $74, $74, $65, $6e, $20, $69
  .db $74, $65, $6d, $20, $62, $69, $74, $65, $73, $21, $0a, $40, $41, $20, $63, $68
  .db $61, $69, $6e, $20, $68, $61, $6e, $67, $69, $6e, $67, $20, $66, $72, $6f, $6d
  .db $20, $74, $77, $6f, $20, $70, $6f, $73, $74, $73, $20, $72, $65, $6d, $69, $6e
  .db $64, $73, $20, $79, $6f, $75, $20, $6f, $66, $20, $74, $68, $65, $20, $47, $61
  .db $74, $65, $77, $61, $79, $20, $41, $72, $63, $68, $2e, $0a, $40, $41, $20, $6d
  .db $61, $74, $68, $65, $6d, $61, $74, $69, $63, $69, $61, $6e, $20, $63, $61, $6c
  .db $63, $75, $6c, $61, $74, $65, $73, $20, $74, $68, $65, $20, $68, $61, $6c, $74
  .db $69, $6e, $67, $20, $70, $72, $6f, $62, $61, $62, $69, $6c, $69, $74, $79, $20
  .db $6f, $66, $20, $61, $20, $54, $75, $72, $69, $6e, $67, $20, $6d, $61, $63, $68
  .db $69, $6e, $65, $2e, $0a, $40, $41, $20, $6e, $75, $6d, $62, $65, $72, $20, $6f
  .db $66, $20, $73, $68, $6f, $72, $74, $20, $74, $68, $65, $61, $74, $72, $69, $63
  .db $61, $6c, $20, $70, $72, $6f, $64, $75, $63, $74, $69, $6f, $6e, $73, $20, $61
  .db $72, $65, $20, $69, $6e, $64, $65, $78, $65, $64, $20, $31, $2c, $20, $32, $2c
  .db $20, $33, $2c, $20, $2e, $2e, $2e, $20, $6e, $2e, $0a, $40, $41, $20, $74, $65
  .db $63, $68, $6e, $69, $63, $61, $6c, $20, $75, $6e, $69, $76, $65, $72, $73, $69
  .db $74, $79, $20, $69, $6e, $20, $41, $75, $73, $74, $72, $61, $6c, $69, $61, $2e
  .db $0a, $40, $49, $74, $20, $69, $73, $20, $2d, $2d, $20, $49, $20, $6a, $75, $73
  .db $74, $20, $66, $65, $65, $6c, $20, $73, $6f, $6d, $65, $74, $68, $69, $6e, $67
  .db $20, $77, $6f, $6e, $64, $65, $72, $66, $75, $6c, $20, $69, $73, $20, $61, $62
  .db $6f, $75, $74, $20, $74, $6f, $20, $68, $61, $70, $70, $65, $6e, $2e, $0a, $40
  .db $49, $74, $27, $73, $20, $61, $20, $43, $61, $74, $20, $35, $20, $63, $61, $62
  .db $6c, $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $20, $55, $2e, $53, $2e
  .db $20, $70, $72, $65, $73, $69, $64, $65, $6e, $74, $2e, $0a, $40, $49, $74, $27
  .db $73, $20, $61, $20, $70, $69, $65, $63, $65, $20, $6f, $66, $20, $63, $6c, $6f
  .db $74, $68, $20, $75, $73, $65, $64, $20, $74, $6f, $20, $63, $6f, $76, $65, $72
  .db $20, $61, $20, $73, $74, $61, $67, $65, $20, $69, $6e, $20, $62, $65, $74, $77
  .db $65, $65, $6e, $20, $70, $65, $72, $66, $6f, $72, $6d, $61, $6e, $63, $65, $73
  .db $2e, $0a, $40, $54, $68, $65, $20, $69, $6f, $6e, $6f, $73, $70, $68, $65, $72
  .db $65, $20, $73, $65, $65, $6d, $73, $20, $63, $68, $61, $72, $67, $65, $64, $20
  .db $77, $69, $74, $68, $20, $6d, $65, $61, $6e, $69, $6e, $67, $2e, $0a, $40, $54
  .db $68, $69, $73, $20, $74, $6f, $6d, $6f, $67, $72, $61, $70, $68, $79, $20, $69
  .db $73, $20, $6c, $69, $6b, $65, $2c, $20, $68, $65, $6c, $6c, $61, $20, $61, $78
  .db $69, $61, $6c, $2c, $20, $6d, $61, $6e, $21, $0a, $40, $49, $74, $27, $73, $20
  .db $79, $6f, $75, $72, $20, $66, $61, $76, $6f, $72, $69, $74, $65, $20, $67, $61
  .db $6d, $65, $20, $2d, $2d, $20, $72, $6f, $62, $6f, $74, $66, $69, $6e, $64, $73
  .db $63, $61, $74, $61, $6e, $21, $0a, $40, $4a, $75, $73, $74, $20, $61, $20, $6d
  .db $61, $6e, $20, $73, $65, $6c, $6c, $69, $6e, $67, $20, $61, $6e, $20, $61, $6c
  .db $62, $61, $74, $72, $6f, $73, $73, $2e, $0a, $40, $54, $68, $65, $20, $69, $6e
  .db $74, $65, $72, $6d, $69, $73, $73, $69, $6f, $6e, $20, $66, $72, $6f, $6d, $20
  .db $61, $20, $31, $39, $33, $30, $73, $20, $73, $69, $6c, $65, $6e, $74, $20, $6d
  .db $6f, $76, $69, $65, $2e, $0a, $40, $49, $74, $27, $73, $20, $61, $6e, $20, $69
  .db $6e, $76, $65, $72, $74, $65, $64, $20, $62, $69, $6c, $6c, $69, $61, $72, $64
  .db $20, $62, $61, $6c, $6c, $21, $0a, $40, $54, $68, $65, $20, $73, $70, $65, $63
  .db $74, $72, $65, $20, $6f, $66, $20, $53, $68, $65, $72, $6c, $6f, $63, $6b, $20
  .db $48, $6f, $6c, $6d, $65, $73, $20, $77, $69, $6c, $6c, $73, $20, $79, $6f, $75
  .db $20, $6f, $6e, $77, $61, $72, $64, $73, $2e, $0a, $40, $49, $74, $27, $73, $20
  .db $4b, $49, $54, $54, $20, $66, $72, $6f, $6d, $20, $54, $56, $27, $73, $20, $4b
  .db $6e, $69, $67, $68, $74, $20, $52, $69, $64, $65, $72, $2e, $0a, $40, $41, $20
  .db $66, $75, $6c, $6c, $2d, $73, $63, $61, $6c, $65, $20, $6d, $6f, $64, $65, $6c
  .db $20, $6f, $66, $20, $74, $68, $65, $20, $55, $53, $53, $20, $45, $6e, $74, $65
  .db $72, $70, $72, $69, $73, $65, $2e, $0a, $40, $41, $20, $4d, $61, $63, $69, $6e
  .db $74, $6f, $73, $68, $20, $43, $6c, $61, $73, $73, $69, $63, $20, $72, $75, $6e
  .db $6e, $69, $6e, $67, $20, $53, $79, $73, $74, $65, $6d, $20, $36, $2e, $30, $2e
  .db $38, $2e, $0a, $40, $41, $20, $4e, $69, $6e, $74, $65, $6e, $64, $6f, $20, $45
  .db $6e, $74, $65, $72, $74, $61, $69, $6e, $6d, $65, $6e, $74, $20, $53, $79, $73
  .db $74, $65, $6d, $2c, $20, $69, $74, $73, $20, $70, $6f, $77, $65, $72, $20, $6c
  .db $69, $67, $68, $74, $20, $66, $6c, $61, $73, $68, $69, $6e, $67, $2e, $0a, $40
  .db $41, $20, $53, $65, $67, $61, $20, $47, $65, $6e, $65, $73, $69, $73, $2c, $20
  .db $64, $6f, $69, $6e, $67, $20, $77, $68, $61, $74, $20, $4e, $69, $6e, $74, $65
  .db $6e, $64, $6f, $6e, $27, $74, $2e, $0a, $40, $41, $20, $4e, $69, $6e, $74, $65
  .db $6e, $64, $6f, $20, $47, $61, $6d, $65, $20, $42, $6f, $79, $2c, $20, $70, $6c
  .db $61, $79, $69, $6e, $67, $20, $54, $65, $74, $72, $69, $73, $20, $77, $69, $74
  .db $68, $20, $69, $74, $73, $65, $6c, $66, $2e, $20, $59, $6f, $75, $20, $61, $76
  .db $65, $72, $74, $20, $79, $6f, $75, $72, $20, $65, $79, $65, $73, $2e, $0a, $40
  .db $41, $20, $52, $6f, $6f, $6d, $62, $61, $20, $77, $69, $74, $68, $20, $6e, $6f
  .db $20, $62, $61, $74, $74, $65, $72, $79, $2e, $20, $59, $6f, $75, $20, $66, $65
  .db $65, $6c, $20, $61, $20, $74, $77, $69, $6e, $67, $65, $20, $6f, $66, $20, $73
  .db $61, $64, $6e, $65, $73, $73, $2e, $0a, $40, $41, $20, $73, $69, $6e, $67, $6c
  .db $65, $20, $67, $72, $61, $69, $6e, $20, $6f, $66, $20, $73, $61, $6e, $64, $2e
  .db $0a, $40, $41, $20, $46, $65, $64, $65, $72, $61, $6c, $20, $53, $69, $67, $6e
  .db $61, $6c, $20, $54, $68, $75, $6e, $64, $65, $72, $62, $6f, $6c, $74, $20, $31
  .db $30, $30, $33, $41, $20, $73, $69, $72, $65, $6e, $20, $69, $73, $20, $67, $6f
  .db $69, $6e, $67, $20, $6f, $66, $66, $20, $68, $65, $72, $65, $2e, $20, $49, $74
  .db $73, $20, $72, $6f, $74, $61, $74, $6f, $72, $20, $69, $73, $20, $62, $72, $6f
  .db $6b, $65, $6e, $2e, $0a, $40, $41, $20, $73, $63, $72, $61, $70, $70, $65, $64
  .db $20, $46, $65, $64, $65, $72, $61, $6c, $20, $53, $69, $67, $6e, $61, $6c, $20
  .db $4d, $6f, $64, $65, $6c, $20, $35, $20, $63, $69, $76, $69, $6c, $20, $64, $65
  .db $66, $65, $6e, $73, $65, $20, $73, $69, $72, $65, $6e, $20, $6c, $69, $65, $73
  .db $20, $68, $65, $72, $65, $2c, $20, $62, $72, $6f, $6b, $65, $6e, $20, $61, $6e
  .db $64, $20, $73, $69, $6c, $65, $6e, $74, $2e, $0a, $40, $41, $20, $44, $65, $6c
  .db $6c, $20, $4f, $70, $74, $69, $70, $6c, $65, $78, $20, $47, $58, $32, $38, $30
  .db $20, $73, $6c, $69, $6d, $6c, $69, $6e, $65, $20, $63, $6f, $6d, $70, $75, $74
  .db $65, $72, $2e, $20, $49, $74, $73, $20, $6f, $70, $74, $69, $63, $61, $6c, $20
  .db $64, $72, $69, $76, $65, $20, $69, $73, $20, $62, $72, $6f, $6b, $65, $6e, $2e
  .db $0a, $40, $41, $20, $54, $79, $70, $65, $20, $32, $20, $68, $61, $6e, $64, $20
  .db $70, $68, $61, $73, $65, $72, $20, $70, $72, $6f, $70, $20, $66, $72, $6f, $6d
  .db $20, $53, $74, $61, $72, $20, $54, $72, $65, $6b, $3a, $20, $54, $68, $65, $20
  .db $4e, $65, $78, $74, $20, $47, $65, $6e, $65, $72, $61, $74, $69, $6f, $6e, $2e
  .db $0a, $40, $49, $54, $27, $53, $20, $41, $20, $46, $41, $41, $41, $41, $41, $4b
  .db $45, $0a, $40, $41, $20, $46, $72, $65, $6e, $63, $68, $20, $53, $70, $61, $64
  .db $20, $58, $49, $49, $49, $20, $42, $69, $70, $6c, $61, $6e, $65, $20, $66, $69
  .db $67, $68, $74, $65, $72, $0a, $40, $41, $20, $43, $69, $73, $63, $6f, $20, $53
  .db $50, $41, $35, $30, $34, $47, $20, $49, $50, $20, $70, $68, $6f, $6e, $65, $2e
  .db $20, $49, $74, $27, $73, $20, $63, $75, $72, $72, $65, $6e, $74, $6c, $79, $20
  .db $73, $68, $6f, $77, $69, $6e, $67, $20, $61, $20, $72, $65, $67, $69, $73, $74
  .db $72, $61, $74, $69, $6f, $6e, $20, $66, $61, $69, $6c, $75, $72, $65, $2e, $0a
  .db $40, $41, $20, $31, $39, $37, $34, $20, $54, $72, $61, $6e, $73, $20, $41, $6d
  .db $20, $77, $69, $74, $68, $20, $74, $68, $65, $20, $34, $35, $35, $53, $44, $20
  .db $65, $6e, $67, $69, $6e, $65, $20, $61, $6e, $64, $20, $66, $6f, $75, $72, $20
  .db $6f, $6e, $20, $74, $68, $65, $20, $66, $6c, $6f, $6f, $72, $2e, $20, $54, $6f
  .db $6f, $20, $62, $61, $64, $20, $79, $6f, $75, $20, $63, $61, $6e, $27, $74, $20
  .db $64, $72, $69, $76, $65, $2e, $0a, $40, $41, $20, $62, $75, $72, $6e, $69, $6e
  .db $67, $20, $62, $75, $73, $68, $2e, $20, $59, $6f, $75, $20, $67, $69, $76, $65
  .db $20, $69, $74, $20, $61, $20, $77, $69, $64, $65, $20, $62, $65, $72, $74, $68
  .db $2e, $0a, $40, $54, $68, $69, $73, $20, $6d, $61, $79, $20, $68, $61, $76, $65
  .db $20, $62, $65, $65, $6e, $20, $6b, $69, $74, $74, $65, $6e, $20, $61, $74, $20
  .db $6f, $6e, $65, $20, $74, $69, $6d, $65, $2c, $20, $62, $75, $74, $20, $79, $6f
  .db $75, $27, $72, $65, $20, $66, $61, $72, $20, $74, $6f, $6f, $20, $6c, $61, $74
  .db $65, $2e, $20, $49, $74, $20, $6d, $65, $6f, $77, $73, $20, $61, $74, $20, $79
  .db $6f, $75, $2e, $0a, $40, $41, $20, $73, $65, $63, $6f, $6e, $64, $20, $64, $65
  .db $72, $69, $76, $61, $74, $69, $76, $65, $20, $73, $69, $74, $73, $20, $68, $65
  .db $72, $65, $20, $6c, $6f, $6f, $6b, $69, $6e, $67, $20, $61, $74, $20, $79, $6f
  .db $75, $2e, $0a, $40, $41, $20, $62, $6f, $61, $74, $2e, $0a, $40, $41, $20, $6b
  .db $61, $79, $61, $6b, $2e, $40
  
  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
;;;;;;;;;;;;;;  
  
  
  .bank 4
  .org $0000
  .incbin "rfk.chr"   ;includes 8KB graphics file from SMB1
