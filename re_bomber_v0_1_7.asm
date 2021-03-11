;/////////////////////////////////////////////////////////////////
;//   Atari 2600 development study
;//   by Rodrigo Gonzales (rodrivas78)
;//   Game: Re-Bomber
;//   Vs.: 0.1.7
;/////////////////////////////////////////////////////////////////

	processor 6502
        include "vcs.h"
        include "macro.h"
        include "xmacro.h"

	seg.u Variables
	org  $80

counter		byte	; increments each frame
yplyr		byte	; player y pos
yball		byte	; ball y pos
animofs		byte	; sprite data offset, used for animation
ysprofs		byte	; temp sprite offset
yballvel	byte	; ball Y velocity
xballvel	byte	; ball X velocity
xballerr	byte	; ball X fractional error
captured	byte	; ball capture flag
avol0		byte	; shadow register for AVOL0
XPos		byte
YPos		byte
SpritePtr	word	; sprite pointer
SpritePtr2	word	; sprite pointer
;//ScoreB
Score		byte	; current BCD-encoded score
Temp		byte	; temporary storage
Temp2		byte	; temporary storage
ScoreHeight	equ 20	; height of top scoreboard
Score0		byte	; BCD score of player 0
Score1		byte	; BCD score of player 1
FontBuf		ds 10	; 2x5 array of playfield bytes

;pointHolder1 byte  ; armazena os pontos do placar 01
;pointHolder2 byte


; Color constants
BGCOLOR		equ $80
PLCOLOR		equ $ee
GNDCOLOR	equ $c0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Enable ball if it is on this scanline (in X register)
; Modifies A.
	 MAC DRAW_BALL
	lda #%00000000        
	 cpx yball
        bne .noball
        lda #%00000010	; for ENAM0 the 2nd bit is enable
.noball
	sta ENABL	; enable ball
       ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	seg Code
        org $f000

; Initialize and set initial offsets of objects.
Start	CLEAN_START


	lda #167   ;controla a altura do player 1 (ground)
        sta YPos
        ;lda #20
        ;ldx #0
        ;jsr SetHorizPos2   
 ;---------------------------
	lda #$60
       ; lda TargetPositions,y
       ; and #$0F	; mask out leftmost digit
        
        sta yplyr	; player Y position, top to bottom

	;lda #75 ; player 1 horizontal position 
        ;ldy #$00 ;//NEW
        
        ;iny  ; Incremente a tabela de posições
        ;iny                           
        ;lda TargetPositions,y
        ;iny
        ;iny
        ; lda #$28    
        
        ;lda TargetPositions,y ;//NEW
        
        ;and #$0F	; mask out leftmost digit
        lda #$f
        jsr SetHorizPos2         
; Set ball horizontal position
	lda #20
        ldx #5
        jsr SetHorizPos2
	sta WSYNC	
       ; sta HMOVE
; Set ball initial velocity
	lda #1
        sta yballvel
        lda #$57
        sta xballvel
         lda #1
        sta VDELP0
       ; lda #0
       ; sta VDELBL
            
       ; lda #<Frame0
       ; sta SpritePtr
       ; lda #>Frame1
       ; sta SpritePtr+1	; normal sprite bitmap
        
        lda #<ground_sprite
        sta SpritePtr
        lda #>ground_sprite2
        sta SpritePtr+1	; normal sprite bitmap
        
      ;Score vars        
           
        lda #$00
        sta Score
        
        lda #$00
        sta Score0
        lda #$80
        sta Score1
        
        lda #$01
        sta Temp
        lda #$01
        sta Temp2
      
        
; Next frame loop
NextFrame
	VERTICAL_SYNC
        ;SLEEP 20  
       
              
; in case the ball is on screen
        lda ColorFrame0	; load 1st entry of color data
        sta COLUP0	; set sprite 0 color
; Set up playfield
        lda #BGCOLOR	; set the background color
        sta COLUBK	
        lda #PLCOLOR	; set the playfield color
        sta COLUPF
        lda #%00010100	; playfield reflection and ball size/priority
        sta CTRLPF
        lda #0		; blank out the playfield
        sta PF0
        sta PF1
        sta PF2
        
       lda #%00010100	; score mode + 2 pixel ball
       sta CTRLPF
       lda #$e
       sta COLUP0	; set color for left
        lda #$a8
       sta COLUP1	; set color for right

; 37 lines of VBLANK
        ldx #20  ;Precisa sincronizar com o SLEEP 
        lda #1
        sta VDELP1
     
Underscan
	sta WSYNC
        dex
	bne Underscan     
        
        lda #0
        sta PF1
        
ScanLoop0  ;Desenha primeira linha superior
	sta WSYNC
        lda #$95  ;Nova cor do BG
        sta COLUBK
              
              
         ;DRAW_BALL	
        ;sta PF1
       ; sta PF2
       ;sta PF0
        inx
        cpx #11
        bne ScanLoop0
       
       ;Carrega os valores do Score       
        lda Score0
        ldx #0
	jsr GetBCDBitmap
	lda Score1
        ldx #5
	jsr GetBCDBitmap  
     
; First, we'll draw the scoreboard.
; Put the playfield into score mode (bit 2) which gives
; two different colors for the left/right side of
; the playfield (given by COLUP0 and COLUP1).
       ; lda #%00010010	; score mode + 2 pixel ball
       ; sta CTRLPF
       ;lda #$8
        ;sta COLUP0	; set color for left
      ; lda #$a8
      ;  sta COLUP1	; set color for right
; Now we draw all four digits.
	ldy #0		; Y will contain the frame Y coordinate
ScanLoop1a
	sta WSYNC
        tya
        lsr		; divide Y by two for double-height lines
        tax		; -> X        
        lda FontBuf+0,x
        sta PF1		; set left score bitmap
        SLEEP 28
        lda FontBuf+5,x
        sta PF1		; set right score bitmap
        iny
        cpy #10
        bcc ScanLoop1a


; Draw 192 scanlines
; First, 20 lines at top of screen
        ;ldx #0
                

; Top border of screen (8 lines)
;ScanLoop2
; Fetch 1st part of bitmap data from table (we start on 20th line)
        ;lda #TopBorder0-17,x
	;sta WSYNC
        ;sta PF0
        ;DRAW_BALL	; draw the ball on this line?
        		; (only for collision purposes)
       
; Fetch 2nd and 3rd parts of bitmap
	;lda #TopBorder1,x
    ;sta PF1
     ; lda #TopBorder2,x
      ; sta PF2
        inx
                 
        ;cpx #10
       ; bne ScanLoop2

; Top half of screen (100 pixels)
        lda #0
        sta PF0
        sta PF1
        sta PF2
   
        
        ;//Agora obsoleto: utilizar SetHorizPos Linha 74
        ;//Posição GRP1 / sprite de solo
         ;SLEEP 21                  ;Ver 37 Lines of Blanck
         ;sta RESP1               
        lda yplyr
        sec
        sbc #150
        sta ysprofs
       
LVScan  
	asl
        sta WSYNC
        txa		; X -> A
        sec		; set carry for subtract
        sbc YPos     
        cmp #SpriteHeight ; in sprite?
        bcc InSprite	; yes, skip over next
        lda #0		; not in sprite, load 0
InSprite
	tay		; local coord -> Y        
        lda (SpritePtr),y	; lookup color           
        sta GRP1	; store bitmap
       lda ColorFrame0,y ; lookup color
        sta COLUP1	; store color               
        txa		; X -> A   
        sbc yplyr
        cmp #SpriteHeight ; in sprite?
        bcc InSprite2	; yes, skip over next
        lda #0		; not in sprite, load 0
InSprite2
	tay		; local coord -> Y
        lda Frame0,y	; lookup color
        ;sta WSYNC	; sync w/ scanline
        sta GRP0	; store bitmap
       ; lda ColorFrame0,y ; lookup color        
         DRAW_BALL	; draw the ball on this line?        
        inx
        cpx #175      ;controla a altura do terreno  
       ;dex		; decrement X
         bne LVScan	; repeat until 192 lines

; 8 more pixels for bottom border
        lda #GNDCOLOR	; set the background color
        sta COLUPF
ScanLoop5
        lda #$ff
	sta WSYNC
        sta PF0
        sta PF1
        sta PF2
        lda #0
        sta GRP0
        lda #0
        sta GRP1
        inx
        cpx #183
        bne ScanLoop5

; Disable ball
        lda #0
        sta ENABL        
        
        sta WSYNC
        ;sta WSYNC
      

; 29 lines of overscan needed, but we have lots of logic to do.
; So we're going to use the PIA timer to let us know when
; almost 29 lines of overscan have passed.
; This handy macro does a WSYNC and then sets up the timer.
	TIMER_SETUP 29

; Check for collisions
 	lda #%01000000
	bit CXP1FB	; collision between player 1 and ball?
        bne Player1Collision
        lda #%01000000
	bit CXP0FB	; collision between player 0 and ball?
        bne PlayerCollision
        lda #%10000000
        bit CXBLPF	; collision between playfield and ball?
        bne PlayfieldCollision
        beq NoCollision
        
Player1Collision
     
        lda #<ground_sprite2
        sta SpritePtr
        
       ;NEW //Score addition
         sed ; enter BCD mode
         lda Score0
         adc Temp
         sta Score0
        ; iny
         ;lda TargetPositions,y
         cld ; exit BCD mode
         
        
     
; Store final velocity
        stx yballvel
; Make a little sound
	txa
        adc #45
	sta AUDF0	; frequency
        lda #16
        sta avol0	; shadow register for volume
    
; Now we bounce the ball depending on where it is
PlayerCollision
; Is the button pressed? if so, just capture the ball
	 
	lda INPT4		;read button input           
	bmi ButtonNotPressed	;skip if button not pressed        
        inc captured		; set capture flag
        lda #SpriteHeight*2
        sta animofs	; use different bitmap when captured
        ;NEW
         lda #<Frame1
        sta SpritePtr
        bne NoCollision       
        
       
    
        
PlayfieldCollision       
         
; If bouncing off top of playfield, bounce down
	ldx #1
	lda yball
              
         
         sta CXCLR
           ; Subtrai uma unidade cada vez que atinge o solo 
         sed ; enter BCD mode
         lda Score1
         sec
         sbc Temp2
         sta Score1
         cld ; exit BCD mode
         
          ;Atualiza a posição do Player 0
     ;    lda Score1 ;/NEW
     ;    tay ;/NEW
        ; adc Score0
         ; lda Score0
        ;  tay
       ;  lda TargetPositions,y ;/NEW
        ; iny
        ; lda TargetPositions,y
        ;  jsr SetHorizPos2 ;/NEW   
        
         ;sed ; enter BCD mode
         lda Score1
         sec
         sbc Temp2
         sta Score1
         jsr SetHorizPos2
         ;cld ; exit BCD mode
        
ButtonNotPressed
	
        lda #0
	sta captured	; clear capture flag
        sta animofs	; use regular bitmap
; See if we bounce off of top half or bottom half  of player
; (yplyr + height/2 - yball)
	
	ldx #1
	lda yplyr
        clc
        adc #SpriteHeight-2
        sec
        sbc yball
      ;  bne StoreVel	; bottom half, bounce down
	;ldx #$ff	; top half, bounce up
        ;bne StoreVel                     
        
        ;/Testar aqui rotina de movimentação do tanque
       ; ldy #1
       ; iny
        ;tyx
       ; jsr SetHorizPos2
       ;iny
      
               
        ;bpl StoreVel
; Otherwise bounce up
	;ldx #$ff
StoreVel
; Store final velocity
        stx yballvel
; Make a little sound
	txa
        adc #45
	sta AUDF0	; frequency
        lda #3
        sta avol0	; shadow register for volume     
     

        
NoCollision
; Clear collision registers for next frame
	sta CXCLR
; Ball captured? if so, no motion
	lda captured
        bne ballCaptured
; Move ball vertically
	lda yball
        clc
        adc yballvel        
        sta yball
; Move ball horizontally
; We use an fractional counter for the ball, and we have to
; set a different HMOVE value depending on if it's left or right
        lda xballvel
        bmi ballMoveLeft
        clc
        adc xballerr
        sta xballerr
       bcc ballCaptured
        lda #$f0
        sta HMBL
        bne ballCaptured     
  
        
ballMoveLeft
	sec
        sbc xballerr
        sta xballerr
        bcs ballCaptured
        lda #$10
        sta HMBL
ballCaptured
        sta WSYNC
        sta HMOVE	; apply the move(s)
        sta HMCLR
               
      
; Joystick movement
; For up and down, we INC or DEC the Y Position
	lda #%00010000	;Up?
	bit SWCHA 
	bne SkipMoveUp
        ldx yplyr        
        cpx #32
        bcc SkipMoveUp
        dex
        stx yplyr        
        lda captured	; captured? move the ball too
        beq SkipMoveUp
        dec yball
SkipMoveUp
	lda #%00100000	;Down?
	bit SWCHA 
	bne SkipMoveDown            
        ldx yplyr        
        cpx #142
        bcs SkipMoveDown
        inx
        stx yplyr
        lda captured	; captured? move the ball too
        beq SkipMoveDown
        inc yball
SkipMoveDown
; Note that the horizontal position is not contained in RAM,
; but inaccessibly inside the TIA's registers! Some games can
; get away with this if they use the collision registers.
	;ldx #0		; assume speed is 0 if no movement	
        lda #%01000000	;Left? Desacelera?                
	;bit SWCHA      ;Elimina o stop com D-Left    
        
        ; ldy #0
	;sty captured	; clear capture flag
        ;dec captured
	bne SkipMoveLeft        
        lda captured	; captured? move the ball too
        beq NoCaptureMove
	ldx #$10	;a 1 in the left nibble means go left        
SkipMoveLeft
	lda #%10000000	;Right?
	bit SWCHA            
	;bne SkipMoveRight
	ldx #$f0	;a -1 in the left nibble means go right...
        
SkipMoveRight
        ldy #$10
        stx HMP0	; set the move for player 0        
        ;sty HMP1
        lda captured	; captured? move the ball too
        beq NoCaptureMove
        stx HMBL	; set ball move register
NoCaptureMove
       ;Solução para o problema de travar a carga no aviao
        lda #0
        sta captured       
        sta WSYNC
        sta HMOVE	; apply the move(s)

; Play audio from shadow register
	ldx avol0
        beq NoAudio
        dex		; decrement volume every frame
	stx AUDV0	; store in volume hardware register
        stx avol0	; store in shadow register
        lda #3
        sta AUDC0	; shift counter mode 3 for weird bounce sound
NoAudio

; Wait until our timer expires and then WSYNC, so then we'll have
; passed 30 scanlines. This handy macro does this.
	TIMER_WAIT 
        ;sta WSYNC     
   
; Goto next frame
        jmp NextFrame
  
 
 ; Fetches bitmap data for two digits of a
; BCD-encoded number, storing it in addresses
; FontBuf+x to FontBuf+4+x.
GetBCDBitmap subroutine
; First fetch the bytes for the 1st digit
	pha		; save original BCD number
        and #$0F	; mask out the least significant digit
        sta Temp
        asl
        asl
        adc Temp	; multiply by 5
        tay		; -> Y
        lda #5
        sta Temp	; count down from 5
.loop1
        lda DigitsBitmap,y
        and #$0F	; mask out leftmost digit
        sta FontBuf,x	; store leftmost digit
        iny
        inx
        dec Temp
        bne .loop1
; Now do the 2nd digit
        pla		; restore original BCD number
        lsr
        lsr
        lsr
        lsr		; shift right by 4 (in BCD, divide by 10)
        sta Temp
        asl
        asl
        adc Temp	; multiply by 5
        tay		; -> Y
        dex
        dex
        dex
        dex
        dex		; subtract 5 from X (reset to original)
        lda #5
        sta Temp	; count down from 5
.loop2
        lda DigitsBitmap,y
        and #$F0	; mask out leftmost digit
        ora FontBuf,x	; combine left and right digits
        sta FontBuf,x	; store combined digits
        iny
        inx
        dec Temp
        bne .loop2
	rts

; SetHorizPos2 - Sets the horizontal position of an object.
; The X register contains the index of the desired object:
;  X=0: player 0
;  X=1: player 1
;  X=2: missile 0
;  X=3: missile 1
;  X=4: ball
; This routine does a WSYNC both before and after, followed by
; a HMOVE and HMCLR. So it takes two scanlines to complete.
SetHorizPos2
        sta WSYNC	; start a new line
        sec		; set carry flag
DivideLoop
	sbc #15		; subtract 15
	bcs DivideLoop	; branch until negative
	eor #7		; calculate fine offset
        asl
        asl
        asl
        asl     
        sta RESP0,x; fix coarse position
        sta HMP0,x	; set fine offset   
        sta RESP1,x; fix coarse position
        sta HMP1,x	; set fine offset  
        sta WSYNC
        sta HMOVE	; apply the previous fine position(s)
	sta HMCLR	; reset the old horizontal position(s)
        rts		; return to caller 

; Height of our sprite in lines
SpriteHeight equ 8

; Bitmap data "standing" position
;;{w:8,h:16,brev:1,flip:1};;
	
    
Frame0  .byte #0
        .byte #%00000000;--
        .byte #%10000000;--
        .byte #%11000000;--
        .byte #%11111110;--
        .byte #%11111111;--
        .byte #%00111100;--
        .byte #%01110000;--
        .byte #%00000000;--
;---End Graphics Data---
;---End Graphics Data---
      
;; Bitmap data "throwing" position
;;{w:8,h:16,brev:1,flip:1};;
Frame1
 	.byte #0
        .byte #%00000000;--
        .byte #%00011000;--
        .byte #%11111100;--
        .byte #%00011100;--
        .byte #%01111110;--
        .byte #%11010101;--
        .byte #%01101011;--
        .byte #%00111110;--
;---End Graphics Data---

ground_sprite  
		.byte #0
                .byte %0001111110
		.byte %0001111110
                .byte %1111111111
                .byte %0111111111
                .byte %1111000111
                .byte %1111000111
                .byte %1111000111
                .byte %1111000111
                .byte %1111000111
                
ground_sprite2  
		.byte #0
                .byte %0000000000
		.byte %0000000110
                .byte %1100000111
                .byte %0100000001
                .byte %1100000011
                .byte %1111000111
                .byte %1100000111
                .byte %1111000111
                .byte %1111000111
                
                
	
;; Color data for each line of sprite
;;{pal:"vcs"};;
ColorFrame0
	.byte #$FF	; ball color if not sharing line with player sprite
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
;; Playfield top border bitmap
;;{w:8,h:8,count:3,xtra:"vcspf"};;
TopBorder0
	.byte #%11111111
	.byte #%11100000
	.byte #%11100000
	.byte #%11111111
	.byte #%11110000
	.byte #%11001111
	.byte #%10000000
	.byte #%00000000
TopBorder1
        .byte #%11111111
        .byte #%11111111
        .byte #%11000111
        .byte #%11111111
        .byte #%11111000
        .byte #%11100000
        .byte #%11000000
        .byte #%10000000
TopBorder2
	.byte #%11111111
	.byte #%11100000
	.byte #%11111111
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
        
     ; org $FF00
        
;; Bitmap pattern for digits
DigitsBitmap ;;{w:8,h:5,count:10,brev:1};;
        .byte $EE,$AA,$AA,$AA,$EE
        .byte $22,$22,$22,$22,$22
        .byte $EE,$22,$EE,$88,$EE
        .byte $EE,$22,$66,$22,$EE
        .byte $AA,$AA,$EE,$22,$22
        .byte $EE,$88,$EE,$22,$EE
        .byte $EE,$88,$EE,$AA,$EE
        .byte $EE,$22,$22,$22,$22
        .byte $EE,$AA,$EE,$AA,$EE
        .byte $EE,$AA,$EE,$22,$EE
        
TargetPositions
	.byte #$88
        .byte #$68
        .byte #$48
        .byte #$28
;;end
;;
; Epilogue
	org $fffc
        .word Start
        .word Start
