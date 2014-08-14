  .inesprg 1   ; 1x 16KB PRG code
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


;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen

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




  .bank 0
  
  	.org $0010
addrLO:	.db 0  ; make "variable"s for our indirect addressing
addrHI: .db 0

  .org $C000 
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
  LDA #$00
  STA $0000, x
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


  


;;;Set some initial ball stats


;;:Set starting game state
  LDA #STATETITLE
  STA gamestate
  
  LDA #$20
  STA nkis
  
  ;JSR turn_screen_on
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  ;LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ;STA $2000
  ;LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  ;STA $2001
  ;LDA #$00        ;;tell the ppu there is no background scrolling
  ;STA $2005
  ;STA $2005
    
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

  RTI             ; return from interrupt
 
 
 
 
;;;;;;;;
 
EngineTitle:
  LDA titledrawn
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
  LDA buttons1
  AND #BUTSTART
  BEQ NoStart
  LDA #STATEPLAYING
  STA gamestate
  JSR clear_screen
  JSR SpriteSetup
  JMP GameEngineDone
  
  NoStart:
  LDA buttons1
  AND #BUTUP
  BEQ NoUp
  LDA nmicounter
  AND #%00000111
  BNE NoUp
  INC nkis
  LDA nkis
  CMP #$40
  BNE notmax
  LDA #$3F
  STA nkis
  notmax:
  
  NoUp:
  LDA buttons1
  AND #BUTDOWN
  BEQ NoDown
  LDA nmicounter
  AND #%00000111
  BNE NoDown
  DEC nkis
  LDA nkis
  CMP #$00
  BNE notmin
  LDA #$01
  STA nkis
  notmin:
  
  NoDown:

  LDX #$00
  STX nkiones ; Blank out ones temp
  STX nkitens ; Blank out tens temp
  
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
  STA $0205
  LDA #%00000011
  STA $0206
  LDA #$AC
  STA $0207
  
  JMP GameEngineDone

;;;;;;;;; 
 
EngineGameOver:

  JMP GameEngineDone
 
;;;;;;;;;;;
 
EnginePlaying:

  LDA #$AD
  STA $0508


  JMP GameEngineDone
 
 
 
 
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
    ldx #0
	lda #$20  ; set the destination address in PPU memory
  	sta $2006  ; should be $2000
  	stx $2006
  	;lda #low(title)   ; put the high and low bytes of the address "title"
	lda #$00
  	sta addrLO        ; into the variables so we can use indirect addressing.
  	;lda #high(title)
  	sta addrHI

	ldx #4  ; number of 256-byte chunks to load
  	ldy #0
clrloop:
  	lda [addrLO],y
  	sta $2007     ; load 256 bytes
  	iny
  	bne clrloop
;--------------------
  	inc addrHI  ; increment high byte of address title to next 256 byte chunk
  	dex        ; one chunk done so X = X - 1.
  	bne clrloop   ; if X isn't zero, do again        
	jsr turn_screen_on
	RTS
;;;;;;;;;;;;;;  

SpriteSetup:
  JSR random_number
  STA $0200
  STA $0501
  LDA #$03
  STA $0201
  STA $0401
  LDA #$00
  STA $0202
  STA $0402
  JSR random_number
  STA $0203
  STA $0404
  LDX #$04
RandSpritesLoop:
  LDA #$AD
  STA $0503
  JSR random_number
  STA $0200,x
  STA $0400,x
  INX
  INX
  INX
  JSR random_number
  STA $0200,x
  STA $0400,x
  CPX nkis
  BNE RandSpritesLoop
RTS

random_number:
  LDA #$AF
  STA $0504
  lda nmicounter+1
  asl A
  asl A
  eor nmicounter+1
  asl A
  eor nmicounter+1
  asl A
  asl A
  eor nmicounter+1
  asl A
  rol nmicounter         ;shift this left, "random" bit comes from low
  rol nmicounter+1
  RTS

  
title: 
  .incbin "title.bin"
  
  .bank 1
  .org $E000
palette:
  .db $0f,$2d,$10,$30,  $0f,$01,$21,$31,  $0f,$06,$16,$26,  $0f,$2d,$19,$29   ;; background palette
  .db $0f,$1a,$30,$37,  $16,$01,$21,$31,  $26,$28,$25,$35,  $36,$16,$29,$39   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .db $80, $03, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  ;.db $88, $34, $00, $80   ;sprite 2
  ;.db $88, $35, $00, $88   ;sprite 3



  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  


;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "rfk.chr"   ;includes 8KB graphics file from SMB1