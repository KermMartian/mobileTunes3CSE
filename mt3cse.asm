;----------------------------------------------
;
;		mobileTunes 3 CSE
;		by MateoConLechuga and Kerm Martian
;		February 2015
;
;----------------------------------------------
.nolist
;.binarymode TI8X
#include "ti84pcse.inc"
#include "dcse8.inc"
#include "keyval.inc"
.list

; From Mateo and benryves' codebase
#define safeMem plotsscreen
#define curSearchFile safeMem+0		; 2 bytes
#define numSongs safeMem+2			; 1 byte
#define selSong safeMem+3			; 1 byte
#define scrollOffset safeMem+4		; 1 byte
#define fullNumSongs safeMem+5		; 1 byte
#define abScrollPos safeMem+6		; 1 byte
#define saveLocations safeMem+7		;43 bytes
#define repeatCount safeMem+50		; 1 byte
#define prevSel  safeMem+51			; 1 byte

; From mobileTunes 3
#define FilePtr safeMem+52			; 2 bytes
#define FilePtrSong	safeMem+54		; 2 bytes
#define FileSize safeMem+56			; 2 bytes
#define OffsetTable safeMem+58		; 2 bytes
#define OffsetSection safeMem+60	; 2 bytes
#define SongNoteLength safeMem+62	; 2 bytes
#define SongNoteCurrent safeMem+64	; 2 bytes
#define tileSafe safeMem+66			; CHAR_SIZE (8) + 2 + 2 bytes
#define TextPalette_Temp safeMem+78 ; 4 bytes
#define prevSliderPos safeMem+82	; 2 bytes

#define songLocations safeMem+100

#define songDetails safeMem+200

#define MAX_LINES 9
#define CHAR_SIZE 8
#define HELP_SETTINGS_LINES 13

#define HELP_SETTINGS_WIDTH 280
#define HELP_SETTINGS_HEIGHT 168
#define HELP_SETTINGS_LEFT ((320-HELP_SETTINGS_WIDTH)/2)
#define HELP_SETTINGS_TOP ((240-HELP_SETTINGS_HEIGHT)/2)

#define NUM_COLOR_PALETTES (global_palettes_end - global_palettes) / 8

#define SLIDER_LEFT_MARGIN 65
#define SLIDER_TOP 193
#define SLIDER_WIDTH 8
#define SLIDER_HEIGHT 14

	.org UserMem
BinaryStart:
	.db $DE,$2A,"N",$BB,$B4,$BB,$B4,$BB,$B3,$BB,$C3,")D"
	.db $BB,$BF,$BB,$BF,$BB,$C2,$BB,$C3,")CSE",$2A,$3F
	.db $EF,$11     ;OpenLib(                                    2    2
	.db "D",$BB,$BF,$BB,$BF,$BB,$C2,$BB,$C3,"CSE",$11,$3F  ;    14   16
	.db $EF,$12,$3F ;ExecLib                                     3   19
	.db $D5,$3F     ;Return                                      2   21
	.db tExtTok,tAsm84CPrgm,$3F ;                                3   24 bytes total
HeaderStart:
	.dw ASMStart-HeaderStart ;offset to code
	
	.dw 10
	.db 3
	.db "DoorsCSE",8,1
	
	.dw Header_Author_End-Header_Author_Start
	.db 2
Header_Author_Start:
	.db "K.Martian+M.Waltz",0
Header_Author_End:

	.dw Header_Desc_End-Header_Desc_Start
	.db 0
Header_Desc_Start:
	.db "MobileTunes 3 CSE",0
Header_Desc_End:

	.dw Header_Icon_End-Header_Icon_Start
	.db 1
Header_Icon_Start:
	.db 0									;Icon type: 32x32, 2-bit color
	.db $ef,$9d,$9d,$56,$32,$f1,$11,$26
	.db 32,32								;Image dimensions, for the sprite routine
sprite:
	.db $00,$00,$00,$44,$44,$00,$00,$00
	.db $00,$00,$01,$66,$65,$50,$00,$00
	.db $00,$00,$1a,$aa,$aa,$99,$00,$00
	.db $00,$00,$6a,$aa,$aa,$aa,$40,$00
	.db $00,$06,$aa,$aa,$aa,$aa,$a4,$00
	.db $00,$1a,$aa,$aa,$aa,$aa,$a5,$00
	.db $00,$6a,$a8,$6a,$aa,$aa,$ee,$40
	.db $01,$aa,$a4,$06,$aa,$ab,$ab,$40
	.db $05,$aa,$a8,$00,$ea,$ee,$ee,$d0
	.db $06,$aa,$a4,$00,$2a,$bb,$bb,$a0
	.db $0a,$aa,$a8,$00,$06,$ef,$ff,$f4
	.db $1a,$aa,$a4,$00,$01,$bb,$bf,$b4
	.db $5a,$aa,$a8,$00,$45,$5f,$ff,$fc
	.db $2a,$aa,$a4,$00,$11,$12,$ff,$f9
	.db $5a,$aa,$a8,$04,$55,$44,$7f,$fd
	.db $2a,$aa,$a4,$01,$11,$11,$1f,$fd
	.db $6a,$ae,$e8,$55,$54,$44,$6f,$fd
	.db $2a,$aa,$b4,$11,$51,$11,$ff,$fd
	.db $6a,$ee,$e8,$55,$44,$4f,$ff,$fd
	.db $2b,$ab,$b5,$11,$10,$3f,$ff,$f8
	.db $1e,$ee,$f9,$54,$47,$ff,$ff,$fc
	.db $1b,$bb,$b5,$11,$1f,$ff,$ff,$f0
	.db $0a,$ef,$f9,$45,$ff,$ff,$ff,$e4
	.db $03,$bb,$b5,$07,$ff,$ff,$ff,$d0
	.db $01,$ff,$f8,$7f,$ff,$ff,$ff,$c0
	.db $01,$bf,$ff,$ff,$ff,$ff,$ff,$00
	.db $00,$5f,$ff,$ff,$ff,$ff,$fc,$00
	.db $00,$17,$ff,$ff,$ff,$ff,$f0,$00
	.db $00,$05,$ff,$ff,$ff,$ff,$c0,$00
	.db $00,$00,$6f,$ff,$ff,$f4,$00,$00
	.db $00,$00,$05,$ff,$fe,$80,$00,$00
	.db $00,$00,$00,$11,$10,$00,$00,$00
	
Header_Icon_End:

	.dw 0
	.db $ff
ASMStart:
	.relocate UserMem

	call setSpeedFast

	xor a
	ld (selSong),a
	ld (prevSel),a
	ld (scrollOffset),a	; Reset Stuff
	
	bcall(_DisableAPD)
	ld a,1				; 15 MHz
	out ($20),a
	bcall(_RunIndicOff)
	di					; Very important! We use iy!

Redraw_Interface:
	ld de,(global_palette)
	call ClearScreen

	ld bc,(global_palette + 2)			; Foreground color
	push bc
		ld hl,(0 * 256) + 0
		ld de,(160 * 256) + 14
		call ColorRectangle

		pop de
	ld hl,67
	call DrawHLine
	
	ld hl,68
	ld b,MAX_LINES
LOOPd:
	push bc
		push hl
			ld de,(global_palette + 2)			; Color
			call DrawHLine
		pop hl
	pop bc
	ld de,13
	add hl,de
	djnz LOOPd

	ld de,4
	ld hl,3
	ld ix,title_icon
	call DrawSprite_2bit
	
	set textInverse,(iy+textFlags)
	ld bc,3+(256*9)
	ld hl,Description
	call DrawString
	res textInverse,(iy+textFlags)

	xor a
	call RedrawPlayPauseButton

	ld bc,(global_palette + 2)
	ld hl,(126 * 256) + 204
	ld de,(34 * 256) + 36
	call ColorRectangle

	ld de,230
	ld hl,204
	ld ix,controls6
	call DrawSprite_1Bit

	ld de,56
	ld hl,196
	ld ix,controls2
	call DrawSprite_2Bit
	
	ld de,85
	ld hl,209
	ld ix,controls3
	call DrawSprite_2Bit

	ld de,147
	ld hl,209
	ld ix,controls4
	call DrawSprite_2Bit
	call RedrawShuffleIndicator

	ld de,209
	ld hl,209
	ld ix,controls5
	call DrawSprite_2Bit

	ld de,312
	ld hl,196
	ld ix,controls7
	call DrawSprite_2Bit
	
	ld b,31
	ld de,65-8
drawSliderBarLoop:
	push bc
		ld hl,8
		add hl,de
		ld de,196
		ex de,hl
		push de
			ld ix,controls8
			call DrawSprite_2Bit
			pop de
		pop bc
	djnz drawSliderBarLoop

RestartFindSongs:
drawSongsLoop:
	xor a
	ld (numSongs),a
	ld (fullNumSongs),a
	ld a,71-13
	ld (penRow),a

	ld hl,songLocations
	ld (saveLocations),hl

	ld hl,(progPtr)
	ld a,(scrollOffset)
	or a
	jr z,noScrollOffset

	ld b,a
scrollToFindStartProg:
	push bc
		ld ix,progSearchHeader
		call ionDetect
	pop bc
	jr nz,noScrollOffset

	ld a,(fullNumSongs)
	inc a
	ld (fullNumSongs),a
	ex de,hl
	djnz scrollToFindStartProg

noScrollOffset:
scanFindNextProg:
	ld ix,(saveLocations)
	ld (ix+1),h
	ld (ix+0),l
	inc ix
	inc ix
	ld (saveLocations),ix

	ld ix,progSearchHeader
	call ionDetect
	ld (curSearchFile),de
	jr nz,foundAllSongs; No more songs

	push hl
		ld hl,numSongs
		inc (hl)
		ld hl,fullNumSongs
		inc (hl)
		ld a,(numSongs)
	pop hl
	cp MAX_LINES
	jr nz,notHitMax
	jr foundAllSongs

notHitMax:
	inc hl

	ld a,(penRow)
	add a,13
	ld (penRow),a
	ld c,a
	ld b,14
	inc hl
	inc hl
	call DrawString
	ld hl,ExtraSpace	; Erase because scroll
	call DrawString
	
	ld hl,(curSearchFile)
	jr scanFindNextProg

foundAllSongs:
	ld hl,(curSearchFile)
countAllSongs:
	ld ix,progSearchHeader
	call ionDetect
	jr nz,countedAllSongs

	ld a,(fullNumSongs)
	inc a
	ld (fullNumSongs),a
	ex de,hl
	jr countAllSongs

countedAllSongs:
	ld a,(numSongs)
	or a
	jp z,FullExit
	
	ld a,(selSong)
	ld b,a
	ld a,(scrollOffset)
	add a,b
	inc a
	ld (abScrollPos),a
	
	ld a,(prevSel)
	ld ix,BulletPointErase
	ld b,2	
DrawBulletLoop:
	push bc
		or a
		jr z,First
		ld b,a
		xor a
Add:
		add a,13
		djnz Add
First:
		add a,71	; Offset
		ld l,a
		ld h,0
		ld de,4*2
		call DrawSprite_2Bit
		ld a,(selSong)
		ld ix,BulletPoint
	pop bc
	djnz DrawBulletLoop
	
ReloadCurrentSong
	ld hl,SLIDER_LEFT_MARGIN + SLIDER_WIDTH
	ld (prevSliderPos),hl

	call loadSong
	call playStop
	call ClearTop
	ld hl,(FilePtr)
	ld bc,21+256*4
	call DrawString
	ld bc,37+256*4
	call DrawString
	ld bc,53+256*4
	call DrawString
;------------------------------------------------------------------
; MainLoop
;------------------------------------------------------------------
MainLoop:
noKeyPressedGUI:
	bcall(_getCSC)
	or a
	jr z,noKeyPressedGUI
	cp skClear			; Clear
	jp z,FullExit
	cp skEnter			; Enter
	jp z,loadAndPlaySong
	cp sk2nd			; 2nd
	jp z,loadAndPlaySong
	cp skYequ
	jp z,loadAndPlaySong
	cp skWindow
	jp nz,noKeyPressedGUI_NotStop
	call playStop
	jr MainLoop
noKeyPressedGUI_NotStop:
	cp skZoom
	jr z,toggleShuffle
	cp skTrace
	jp z,HelpSettings
	cp skUp				; Up
	jr nz,notGUIUp
	ld a,(selSong)
	or a
	jr nz,notOffTop

	ld a,(scrollOffset)
	or a
	jr z,noKeyPressedGUI
	ld (prevSel),a
	dec a
	ld (scrollOffset),a
	jp drawSongsLoop

notOffTop:
	ld (prevSel),a
	dec a
	ld (selSong),a
	jp drawSongsLoop
notGUIUp:
	cp skDown			; Down
	jr nz,noKeyPressedGUI
	ld a,(numSongs)
	ld b,a
	ld a,(selSong)
	ld (prevSel),a
	inc a
	cp MAX_LINES-1
	jr nz,notOffBottomScreen

	ld a,(abScrollPos)
	ld b,a
	ld a,(fullNumSongs)
	cp b
	jr z,noKeyPressedGUI
	ld a,(scrollOffset)
	inc a
	ld (scrollOffset),a
	jp drawSongsLoop

notOffBottomScreen:
	cp b
	jr z,noKeyPressedGUI
	ld (selSong),a
	jp drawSongsLoop
	
toggleShuffle:
	ld a,(shuffle_mode)
	xor $01
	ld (shuffle_mode),a
	call RedrawShuffleIndicator
	jp RestartFindSongs

HelpSettings:
	; Erase background
	ld bc,(global_palette + 0)			; Background color
	ld hl,(HELP_SETTINGS_LEFT / 2) * 256 + HELP_SETTINGS_TOP + 14
	ld de,(HELP_SETTINGS_WIDTH / 2) * 256 + HELP_SETTINGS_HEIGHT - 14
	call ColorRectangle
	
	ld bc,(global_palette + 2)			; Foreground color
	ld hl,0 + (HELP_SETTINGS_LEFT / 2) * 256 + HELP_SETTINGS_TOP
	ld de,0 + (HELP_SETTINGS_WIDTH / 2) * 256 + 14
	call ColorRectangle

	; Draw top and left borders
	push iy
		ld de,HELP_SETTINGS_LEFT - 1
		ld bc,HELP_SETTINGS_TOP - 1
		push de
			push bc
				ld hl,HELP_SETTINGS_LEFT + HELP_SETTINGS_WIDTH
				push bc
					pop ix
				ld a,(global_palette + 4)			; Dark accent color
				ld iyh,a
				ld a,(global_palette + 5)
				ld iyl,a
				push iy
					call ColorLine
					pop iy
				pop bc
			pop de
		push de
			pop hl
		ld ix,HELP_SETTINGS_TOP + HELP_SETTINGS_HEIGHT
		push iy
			call ColorLine
			pop iy
		
		; Draw bottom and right borders
		ld hl,HELP_SETTINGS_LEFT + HELP_SETTINGS_WIDTH
		ld ix,HELP_SETTINGS_TOP + HELP_SETTINGS_HEIGHT
		push hl
			push ix
				ld de,HELP_SETTINGS_LEFT - 1
				push ix
					pop bc
				push iy
					call ColorLine
					pop iy
				pop ix
			pop hl
		push hl
			pop de
		ld bc,HELP_SETTINGS_TOP - 1
		call ColorLine
		pop iy
			
	ld de,HELP_SETTINGS_LEFT + 4
	ld hl,HELP_SETTINGS_TOP + 3
	ld ix,title_icon
	call DrawSprite_2bit
	
	set textInverse,(iy+textFlags)
	ld bc,(256 * (HELP_SETTINGS_LEFT + 18) / 2) + HELP_SETTINGS_TOP + 3
	ld hl,HelpSettingsTitle
	call DrawString
	res textInverse,(iy+textFlags)
	
	ld hl,HelpSettingsText
	ld bc,(256 * (HELP_SETTINGS_LEFT + 4) / 2) + HELP_SETTINGS_TOP + 3 + 14
	ld e,HELP_SETTINGS_LINES
HelpSettings_TextLoop:
	push de
		push bc
			call DrawString
			pop bc
		ld a,11
		add a,c
		ld c,a
		pop de
	dec e
	jr nz,HelpSettings_TextLoop

	ld b,0
	ld de,HELP_SETTINGS_LEFT + 2 + 80 + 8
HelpSettings_PaletteLoop:
	push bc
		push de
			ld a,b
			call DisplayPalette
			pop de
		ld hl,24
		add hl,de
		ex de,hl
		pop bc
	inc b
	ld a,b
	cp NUM_COLOR_PALETTES
	jr nz,HelpSettings_PaletteLoop

HelpSettings_KeyLoop:
	bcall(_getcsc)
	di					; Because we use iy (also interrupts make audio bad)
	or a
	jr z,HelpSettings_KeyLoop
	cp skClear
	jr z,HelpSettings_Finish
	cp sk2nd
	jr z,HelpSettings_Finish
	cp skEnter
	jr z,HelpSettings_Finish
	cp skLeft
	jr nz,HelpSettings_KeyLoop_NotLeft
	ld a,(palette_choice)
	dec a
	jp p,HelpSettings_KeyLoop_SetandRedisplay
	ld a,NUM_COLOR_PALETTES - 1
	jr HelpSettings_KeyLoop_SetandRedisplay
HelpSettings_KeyLoop_NotLeft:
	cp skRight
	jr nz,HelpSettings_KeyLoop
	ld a,(palette_choice)
	inc a
	cp NUM_COLOR_PALETTES
	jr nz,HelpSettings_KeyLoop_SetandRedisplay
	xor a
HelpSettings_KeyLoop_SetandRedisplay:
	ld (palette_choice),a
	call SetPalette
	jp HelpSettings
HelpSettings_Finish:
	ld a,(palette_choice)
	call SetPalette
	jp Redraw_Interface

; Copies a'th palette to global_palette
; Input: a = palette index
; Output: (global_palette + 0-7) set
; Destroyed: a, bc, hl
SetPalette:
	push de
		add a,a
		add a,a
		add a,a
		ld c,a
		ld b,0
		ld hl,global_palettes
		add hl,bc
		ld de,global_palette
		ld bc,8
		ldir
		pop de
	ret

DisplayPalette:
	push af
		call SetPalette
		ld ix,Palette_Select_Icon
		ld hl,HELP_SETTINGS_TOP + 14 + (10 * 11) - 2
		push de
			call DrawSprite_2Bit
			pop de
		pop bc
	ld a,(palette_choice)
	cp b
	ret nz
	ld a,e
	srl a
	add a,2
	ld b,a
	ld c,HELP_SETTINGS_TOP + 14 + (10 * 11) - 2 + 16 + 3
	ld hl,Palette_Pointer_String
	jp DrawString

RedrawShuffleIndicator:
	ld ix,ShuffleIndicator
	ld de,177
	ld hl,229
	call DrawSprite_2Bit
	ld a,(shuffle_mode)
	or a
	ret z
	ld bc,(global_palette + 6)			; Accent color 1
	ld a,b
	ld b,c
	ld c,a
	ld hl,0 + ((178 / 2) * 256) + 230
	ld de,0 + ((6 / 2) * 256) + 6
	jp ColorRectangle

loadSong:
	ld a,(selSong)
	ld l,a
	ld h,0
	add hl,hl
	ld de,songLocations
	add hl,de
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	; Now find the actual start of the song
	push de
		push bc
			ld ix,progSearchHeader
			call ionDetect
			push hl
				;ld de,-5				; Reverse to the beginning of the program?
				;add hl,de
				; Store the pointer and set up other important variables
				inc hl
				inc hl
				inc hl		; Get past header
				ld (FilePtr),hl
				ld bc,65000	; So cpir actually works right
				xor a
				ld (repeatCount),a
				cpir
				cpir
				cpir		; Get past info bytes
				ld (FilePtrSong),hl

				push hl
					ld hl,0
					ld (OffsetTable),hl
					ld (OffsetSection),hl
					ld (SongNoteCurrent),hl
					ld b,h
					ld c,l
					pop hl
				; Count the number of notes in the file for the slider
CountNotes:
				push hl
					ld a,(hl)
					inc hl
					ld h,(hl)
					ld l,a
					or h
					jr z,CountNotesDone
					ld de,(FilePtrSong)
					add hl,de
CountNotesInner:
					inc hl
					inc hl
					inc hl
					inc hl
					ld a,(hl)
					inc hl
					or (hl)
					jr z,CountNotesNext
					inc hl
					inc bc
					jr CountNotesInner
CountNotesNext:
					pop hl
				inc hl
				inc hl
				jr CountNotes
CountNotesDone:
					pop hl
				ld (SongNoteLength),bc
				call UpdateSlider

				pop hl
			pop bc
		pop de
	ret
	
loadAndPlaySong:
	ld a,4
	call RedrawPlayPauseButton
#ifdef COMMENTED_OUT
	call loadSong
#endif
	call playSong			; If we come back from here, the song is done
	call setSpeedFast
	xor a
	call RedrawPlayPauseButton
	jp ReloadCurrentSong

; a must be 0 (play button) or 4 (pause button)
RedrawPlayPauseButton:
	push af
		ld de,0
		ld hl,184
		ld ix,controls1
		call DrawSprite_2Bit
		pop af
	ld hl,ControlsPPLUT
	ld d,0
	ld e,a
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
		pop ix
	inc hl
	ld e,(hl)
	ld d,0
	inc hl
	ld l,(hl)
	ld h,0
	jp DrawSprite_2Bit

UpdateSlider:
	ld de,(SongNoteCurrent)
	ld a,320 - 8 - SLIDER_LEFT_MARGIN - SLIDER_WIDTH		;screen width - right margin - left margin - slider width
	call MultADE
	ld b,h
	ld c,l
	ld de,(SongNoteLength)
	call DivABCDE
	ld hl,SLIDER_LEFT_MARGIN			; left margin
	add hl,bc
	ld bc,(prevSliderPos)
	push hl
		pop de
	or a
	sbc hl,bc
	ret z				; same position
	push de				; save new slider pos
		ld de,SLIDER_WIDTH
		jr c,UpdateSlider_FullErase
		or a
		sbc hl,de		; Compare width to slider width
		add hl,de
		jr c,UpdateSlider_DoErase	; Less than width
UpdateSlider_FullErase:
		ex de,hl		; slider width -> hl
UpdateSlider_DoErase:
		push bc			; bc, to move into hl for slider pos
			push hl
				push bc
					ld a,$50
					ld hl,SLIDER_TOP
					call WriteLCD	;Ymin
					ld a,$20
					call WriteLCD	;Ystart
					ld a,$51
					ld hl,SLIDER_TOP + SLIDER_HEIGHT - 1
					call WriteLCD	;Ymax
					inc a
					pop hl
				call WriteLCD		;Xmin
				ld a,$21
				call WriteLCD		;Xstart
				pop de
			push de
				add hl,de
				dec hl
				ld a,$53
				call WriteLCD  		;Xmax
				ld a,$22			;write mode
				out ($10),a
				out ($10),a
				pop de
			ld b,e					; b  = number of columns to erase
			pop hl
		ld de,-64
		add hl,de
		ld a,l						; (h)l = current slider X pos to erase
		ld c,SLIDER_HEIGHT

		ld ix,UpdateSlider_RestoreMask
		ld de,SLIDER_HEIGHT
		cp 229 - SLIDER_LEFT_MARGIN
		jr c,UpdateSlider_Outer
		add ix,de
		cp 233 - SLIDER_LEFT_MARGIN
		jr c,UpdateSlider_Outer
		add ix,de
		cp 236 - SLIDER_LEFT_MARGIN
		jr c,UpdateSlider_Outer
		add ix,de
		;ld d,0		; d is already 0
UpdateSlider_Outer:
		push bc
			ld c,$11
			ld a,(ix)
			add a,a
			ld e,a
			ld hl,global_palette
			add hl,de
			ld a,(hl)
			inc hl
			ld l,(hl)
UpdateSlider_Inner:
			out (c),a
			out (c),l
			djnz UpdateSlider_Inner
			inc ix
			pop bc
		dec c
		jr nz,UpdateSlider_Outer
		pop hl
	ld (prevSliderPos),hl
	ld b,5
	ld de,SLIDER_TOP	;slider top
	ex de,hl
	ld ix,slider
	jp DrawSprite_2Bit

;------------------------------------------------------------------
; ClearScreen
; Copies whatever is at plotsscreen to the LCD bitwise
; A = color XlibC
;------------------------------------------------------------------
ClearScreen:
	push de
SetLcdWindowFull:
		ld a,$50
		ld hl,0
		call WriteLCD  ;Ymin
		ld a,$52
		call WriteLCD  ;Xmin
		dec a
		ld hl,239
		call WriteLCD  ;Ymax
		ld a,$53
		ld hl,319
		call WriteLCD  ;Xmax
		ld hl,0
		ld a,$20
		call WriteLCD  ;Y pos
		inc a
		call WriteLCD
		inc a     ;write mode
		out ($10),a
		out ($10),a
		ld de,(320*240)/2
		pop hl
FullScreenColorLoop:
	out (c),h
	out (c),l
	out (c),h
	out (c),l
	dec e
	jp nz,FullScreenColorLoop
	dec d
	jp nz,FullScreenColorLoop
	ret

WriteLCD:
	out ($10),a
	out ($10),a
	ld c,$11
	out (c),h
	out (c),l
	ret

;------------------------------------------------------------------
; FullExit
; Fixes up everything and exits
;------------------------------------------------------------------
FullExit:
	xor a
	out (0),a
	cpl
	out (1),a
	call ClearScreen
	bcall(_DrawStatusBar)		
	ld hl,cmdShadow
	ld (hl),' '
	ld de,cmdShadow+1
	ld bc,259
	ldir
	ld hl,textShadow
	ld (hl),' '
	ld de,textShadow+1
	ld bc,259
	ldir
	bcall(_ClrScrn)
	bcall(_HomeUp)
	ld a,$6A
	ld hl,0
	call WriteLCD
	bcall(_DelRes)
	bcall(_EnableAPD)
	ei
	ret
	
#ifdef COMMENTED_OUT
;SetBoundary---------------------------------------
; Sets the window bounds for chars and sprites
; l = min Y
; b = min X / 2
; (de) = width
; max Y is always 239
;--------------------------------------------------
SetBoundary:
	ld h,0
	ld a,$50		; min Y
	call	WriteLCD
	ld a,$20		; curr Y
	call	WriteLCD
	ld l,b		; min X
	add	hl,hl
	ld a,$52
	call	WriteLCD
	ld a,$21		; curr X
	call	WriteLCD
	ld a,(de)		; width
	dec	a
	ld c,a
	ld b,0
	add	hl,bc
	ld a,$53		; max X
	call	WriteLCD
	ld a,$22
	out	($10),a
	out	($10),a
	ret
#endif

;playSong------------------------------------------
; Plays a song
; IX -> Sond data
;--------------------------------------------------
playSong:
	ld ix,(FilePtrSong)
	ld de,(OffsetTable)
	add ix,de
	ld a,(ix+0)
	or (ix+1)
	jr nz,PlaySection
playStop:
	ld hl,0						;reset stuff here
	ld (OffsetTable),hl
	ld (OffsetSection),hl
	ld (SongNoteCurrent),hl
	jp UpdateSlider

#ifdef false
	ld a,(ix+0)
	or (ix+1)
	call z,checkCommand

	ld (songLocSave+2),ix
	ld h,(ix+1)
	ld l,(ix+0)
songLoc:
	ld de,0
	add hl,de
	push hl
		pop ix
	call playSection
songLocSave:
	ld ix,0
	inc ix
	inc ix
	jr playSong
#endif

; Play the section pointed to by IX:
playSection:
	ld d,(ix+1)
	ld e,(ix+0)
	ld ix,(FilePtrSong)
	add ix,de
	ld de,(OffsetSection)
	add ix,de
playNoteLoop:
	ld b,(ix+5)
	ld c,(ix+4)
	ld a,b
	or c
	jr z,playNextSection
	ld h,(ix+0)
	ld l,(ix+1)
	ld d,(ix+2)
	ld e,(ix+3)
	inc ix
	inc ix
	inc ix
	inc ix
	inc ix
	inc ix
	call setSpeedSlow
	call playTone
	call setSpeedFast
	; Update note tracking for the slider
	ld hl,(SongNoteCurrent)
	inc hl
	ld (SongNoteCurrent),hl
	push ix
		call UpdateSlider
		pop ix
	; Check if user pressed a key on the top row
	ld a,KeyRow_Top
	out (1),a
	nop \ nop
	in a,(1)
	cp dkWindow
	jr z,playStop
	ld hl,(OffsetSection)
	ld de,6
	add hl,de
	ld (OffsetSection),hl
	jr playNoteLoop

playNextSection:
	ld hl,(OffsetTable)
	inc hl
	inc hl
	ld (OffsetTable),hl
	ld hl,0
	ld (OffsetSection),hl
	jp PlaySong

; Play the tone dur=bc, period = h,l,d,e (4 channel sound - h,l = left speaker, d,e = right speaker)
playTone:
	di
	ld a,$D0
	ld (toneMask1+1),a
	ld (toneMask2+1),a
	inc b 
	
; INIT CHANNEL A
	ld a,h
	ld (toneAPitch+1),a
	or a
	jr z,isRestA
	ld a,1
	jr notRestA
isRestA:
	xor a
notRestA:
	ld (toneAChange+1),a

; INIT CHANNEL B
	ld a,l
	ld (toneBPitch+1),a
	or a
	jr z,isRestB
	ld a,1
	jr notRestB
isRestB:
	xor a
notRestB:
	ld (toneBChange+1),a

; INIT CHANNEL C
	ld a,d
	ld (toneCPitch+1),a
	or a
	jr z,isRestC
	ld a,2
	jr notRestC
isRestC:
	xor a
notRestC:
	ld (toneCChange+1),a

; INIT CHANNEL D
	ld a,e
	ld (toneDPitch+1),a
	or a
	jr z,isRestD
	ld a,2
	jr notRestD
isRestD:
	xor a
notRestD:
	ld (toneDChange+1),a

toneMaskPreserve:
	push bc
toneMask:
		and 1
		jr z,playPart2
		ld a,(toneMask1+1)
		jr playPart1
playPart2:
		ld a,(toneMask2+1)
playPart1:
		out ($00),a
pitchLoop:
		dec h
		jr nz,noPitchA
toneMask1:
		ld a,0
toneAChange:
		xor 0
		ld (toneMask1+1),a
toneAPitch:
		ld h,0
noPitchA:

		dec l
		jr nz,noPitchB
toneMask2:
		ld a,0
toneBChange:
		xor 0
		ld (toneMask2+1),a
toneBPitch:
		ld l,0
noPitchB:

		dec d
		jr nz,noPitchC
		ld a,(toneMask1+1)
toneCChange:
		xor 0
		ld (toneMask1+1),a
toneCPitch:
		ld d,0
noPitchC:

		dec e
		jr nz,noPitchD
		ld a,(toneMask2+1)
toneDChange:
		xor 0
		ld (toneMask2+1),a
toneDPitch:
		ld e,0
noPitchD:

extendDuration:
		ld a,0
		dec a
		ld (extendDuration+1),a
		jr nz,toneMask

		ld a,KeyRow_Top
		out (1),a
		pop bc					; Instead of nop \ nop
	in a,(1)
	cp dkY
	jr nz,notTimeToQuit

	pop hl						; End playTone
	pop hl						; End playSong
	call setSpeedFast
	xor a
	call RedrawPlayPauseButton
	jp MainLoop

notTimeToQuit:
	dec c
	jp nz,toneMaskPreserve
	dec b
	jp nz,toneMaskPreserve
	ret

#ifdef false
; MAGIC SYSTEM COMMANDS:
checkCommand:

		; We've just received a song command:

		ld a,(ix+2)
		cp 254
		jr nz,notValidCommand

		; You see, in an OLD SONG, the end of the song will be:
		; 0,0,NOTE1...
		; NOTE1 CANNOT BE 254 IF USING THE STANDARD TONES!
		; Therefore, use 254 as a command signifier.
		inc ix
		inc ix
		inc ix

		ld a,(ix+0)			
		or a
		jr nz,notValidCommand

	; It's a REPEAT INSTRUCTION

		ld a,(repeatCount)
		inc a
		ld (repeatCount),a
		ld b,a

		ld a,(ix+1)
		or a
		jr z,infiniteRepeat

		cp b
		jr z,notValidCommand	; Stop playing the song, we've done all the repeats

infiniteRepeat:

		ld h,(ix+3)
		ld l,(ix+2)
		ld de,(songLoc+1)
		add hl,de

		push hl
			pop ix

		pop af
	jp playSong
	
notValidCommand:
		pop af
	ret	; Stop playing the song!
#endif

;DrawString------------------------------------------
; Draws a string
; B = X
; C = Y
; HL -> String
;--------------------------------------------------
DrawString:
	push hl
		ld de,(global_palette + 2)		; foreground
		ld hl,(global_palette)			; background
		bit textInverse,(iy+textFlags)
		jr z,DrawString_NoInverse
		ex de,hl
DrawString_NoInverse:
		ld ix,TextPalette_Temp
		ld (ix),e
		ld (ix+1),d
		ld (ix+2),l
		ld (ix+3),h
		pop hl
	ld a,(hl)
	inc hl
	or a
	ret z
	push bc
		push hl
			call DrawChar
			pop hl
		pop bc
	ld a,b
	add a,5
	ld b,a
	cp 160
	ret nc			; Make sure we aren't out of bounds...
	jr DrawString

DrawChar:
	sub 32
	cp 62
	jr c,NoAdd
	sub $20			; Make lowercase letters uppercase to save space
NoAdd:
	ld hl,tileSafe
	push bc
		push hl
			ld de,TextPalette_Temp
			ld (hl),e		; Palette
			inc hl
			ld (hl),d
			inc hl
			ld (hl),CHAR_SIZE		; x
			inc hl
			ld (hl),CHAR_SIZE		; y
			inc hl
			push hl
				add a,a
				add a,a
				ld e,a
				ld d,0
				ld hl,tileData
				add hl,de
				add hl,de
				pop de
			ld bc,CHAR_SIZE
			ldir
			pop ix
		pop bc
	ld e,c
	ld d,0
	ld l,b
	ld h,d
	add hl,hl
	ex de,hl
	jp DrawSprite_1Bit
	
getNextString:
	ld a,(hl)
	inc hl
	or a
	jr nz,getNextString
	ret
	
iondetect:
	ld de,(ptemp)
	bcall(_cphlde)
	ld a,(hl)
	jr nz,detectContinue
	inc	a
	ret
detectContinue:
	push hl
		and $01
		jr nz,detectSkip

		dec	hl
		dec	hl
		dec	hl	; hl->lsb ptr
		ld e,(hl)
		dec	hl
		ld d,(hl)
		dec	hl	; hl->page
		ld a,(hl)
		or a
		push af
			ld h,d
			ld l,e	; hl & de->program
			jr z,detectNoMove
			push hl
				ld hl,64
				bcall(_enoughMem)
				ld bc,64
				pop	hl
			jr c,detectNotEnough
			ld de,(OPS-8)
			push ix
				push hl
					push de
						bcall(_flashToRam)
						pop	hl
					push hl
						pop	ix
					ld a,10
					add	a,(ix+9)
					ld e,a
					ld d,0	; de=flash offset
					add	hl,de
					ex (sp),hl
					add	hl,de
					pop	de
				ex de,hl	; hl-temp, de-perm
				pop	ix
detectNoMove:
			inc	de
			inc	de
			ld c,(hl)
			inc	hl
			ld b,(hl)
			inc	hl	; hl->data in ram
			push bc
				push ix
					pop	bc
detectCheck:
				ld a,(bc)
				or a
				jr z,detectFound
				cp	(hl)
				inc	bc
				inc	de
				inc	hl
				jr z,detectCheck
detectBad:
				pop	bc
detectNotEnough:
			pop	af
detectSkip:
		pop	hl
	ld bc,-6
	add	hl,bc
	ld b,(hl)
	dec	hl
detectNameLoop2:
	dec	hl
	djnz detectNameLoop2
	jr iondetect
detectFound:
	pop	hl
	; hl=size, de->data
	pop	af	; a=page, f=(or a)
	jr z,detectInRam
	push de	; data
		push af
			push hl
				bcall(_enoughMem)
				pop	bc
			jr c,detectBad
			pop	af
		pop	hl
	ld de,(OPS-8)	; tempMem
	push de
		bcall(_flashToRam)
		pop	de
detectInRam:	; de->data in RAM
		pop	hl	; hl->vat location
	ld bc,-6
	add	hl,bc
	ld b,(hl)
	inc	b
detectNameLoop1:
	dec	hl
	djnz detectNameLoop1
	ex de,hl
	xor	a
	ret
	

;ClearTop------------------------------------------
; Clears the song data at the top
;--------------------------------------------------
ClearTop:
	ld b,67-23
	ld hl,18
	ld de,(global_palette)			;Background Color
ClearLoop:
	push hl
		push bc
			call DrawHLine	; This could have been made a lot faster, but it works well enough
			pop bc
		pop hl
	inc hl
	djnz ClearLoop
	ret 
	
;DrawHLLine----------------------------------------
; Draws a Horizontal line
; L = Y
; de = color
;--------------------------------------------------
DrawHLine:
	push de
		ld a,$50
		push hl
			call WriteLCD  ;Ymin
			ld hl,0
			ld a,$52
			call WriteLCD  ;Xmin
			inc a
			ld hl,319
			call WriteLCD  ;Xmax
			pop hl
		ld a,$20
		call WriteLCD  ;Y pos
		ld hl,0
		inc a
		call WriteLCD
		inc a    ;write mode
		out ($10),a
		out ($10),a
		ld b,160
		pop hl
loopes:
	out (c),h
	out (c),l
	out (c),h
	out (c),l
	djnz loopes
	ex de,hl
	ret

SwapRAM4In:				; 16kb of Data for extraction... just have to hope songs don't go over that. :(
	push af
		ld a,$08
		out	($27),a
		ld a,$04
		out	($05),a
		pop	af
	ret

SwapVATIn:	
	push af; Just to make sure
		xor	a
		out	($05),a
	pop	af
	ret

;--------------------------------------------------------------------------------
; Math
;--------------------------------------------------------------------------------
MultADE:
	ld c,0
	ld h,c
	ld l,c
	add	a,a		; optimised 1st iteration
	jr nc,$+4
	ld h,d
	ld l,e
	ld b,7
MultADE1:
	add	hl,hl		; unroll 7 times
	rla			; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	adc	a,c		; ...
	djnz MultADE1
	ret
DivABCDE:
	ld hl,0
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	ret

SetSpeedFast:
	ld a,1
	out (20h), a
	ret
SetSpeedSlow:
	xor a
	out (20h), a
	ret

;--------------------------------------------------------------------------------
; Data
;--------------------------------------------------------------------------------
shuffle_mode:
	.db 0
palette_choice:
	.db 0

; Characters
; Exported using PixelFontEdit 8x8
tileData:
	.db	$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF	; ( )
	.db	$E7,$E7,$E7,$E7,$E7,$FF,$E7,$E7	; (!)
	.db	$99,$99,$99,$FF,$FF,$FF,$FF,$FF	; (")
	.db	$99,$00,$00,$99,$99,$00,$00,$99	; (#)
	.db	$FF,$E7,$E7,$81,$81,$E7,$E7,$FF	; ($)
	.db	$FF,$E7,$E7,$81,$81,$E7,$E7,$FF	; (%)
	.db	$FF,$E7,$E7,$81,$81,$E7,$E7,$FF	; (&)
	.db	$E7,$E7,$E7,$FF,$FF,$FF,$FF,$FF	; (')
	.db	$F1,$E3,$C7,$CF,$CF,$C7,$E3,$F1	; (()
	.db	$8F,$C7,$E3,$F3,$F3,$E3,$C7,$8F	; ())
	.db	$FF,$FF,$FF,$E7,$E7,$FF,$FF,$FF	; (*)
	.db	$FF,$E7,$E7,$81,$81,$E7,$E7,$FF	; (+)
	.db	$FF,$FF,$FF,$FF,$FF,$E7,$E7,$CF ; (,)
	.db	$FF,$FF,$FF,$81,$81,$FF,$FF,$FF	; (-)
	.db	$FF,$FF,$FF,$FF,$FF,$FF,$E7,$E7	; (.)
	.db	$F8,$F1,$E3,$C7,$8F,$1F,$3F,$7F	; (/)
	.db	$00,$00,$3C,$3C,$3C,$3C,$00,$00	; (0)
	.db	$F3,$C3,$C3,$F3,$F3,$F3,$F3,$F3	; (1)
	.db	$00,$00,$FC,$00,$00,$3F,$00,$00	; (2)
	.db	$00,$00,$FC,$C0,$C0,$FC,$00,$00	; (3)
	.db	$3C,$3C,$3C,$00,$00,$FC,$FC,$FC	; (4)
	.db	$00,$00,$3F,$00,$00,$FC,$00,$00	; (5)
	.db	$00,$00,$3F,$00,$00,$3C,$00,$00	; (6)
	.db	$00,$00,$FC,$FC,$FC,$FC,$FC,$FC	; (7)
	.db	$00,$00,$3C,$00,$00,$3C,$00,$00	; (8)
	.db	$00,$00,$3C,$00,$00,$FC,$00,$00	; (9)
	.db	$FF,$E7,$E7,$FF,$FF,$E7,$E7,$FF	; (:)
	.db	$FF,$E7,$E7,$FF,$FF,$E7,$E7,$E7	; (;)
	.db	$F1,$E3,$C7,$8F,$8F,$C7,$E3,$F1	; (<)
	.db	$FF,$81,$81,$FF,$FF,$81,$81,$FF	; (=)
	.db	$8F,$C7,$E3,$F1,$F1,$E3,$C7,$8F	; (>)
	.db	$C3,$C3,$F3,$E7,$E7,$FF,$E7,$E7	; (?)
	.db	$83,$01,$39,$21,$21,$3F,$81,$81	; (@)
	.db	$00,$00,$3C,$00,$00,$3C,$3C,$3C	; (A)
	.db	$03,$03,$3C,$00,$00,$3C,$03,$03	; (B)
	.db	$00,$00,$3F,$3F,$3F,$3F,$00,$00	; (C)
	.db	$03,$03,$3C,$3C,$3C,$3C,$03,$03	; (D)
	.db	$00,$00,$3F,$03,$03,$3F,$00,$00	; (E)
	.db	$00,$00,$3F,$03,$03,$3F,$3F,$3F	; (F)
	.db	$00,$00,$3F,$20,$20,$3C,$00,$00	; (G)
	.db	$3C,$3C,$3C,$00,$00,$3C,$3C,$3C	; (H)
	.db	$00,$00,$E7,$E7,$E7,$E7,$00,$00	; (I)
	.db	$00,$00,$E7,$E7,$E7,$E7,$07,$07	; (J)
	.db	$3C,$38,$31,$03,$03,$31,$38,$3C	; (K)
	.db	$3F,$3F,$3F,$3F,$3F,$3F,$00,$00	; (L)
	.db	$3C,$18,$00,$00,$24,$3C,$3C,$3C	; (M)
	.db	$3C,$1C,$0C,$04,$20,$30,$38,$3C	; (N)
	.db	$00,$00,$3C,$3C,$3C,$3C,$00,$00	; (O)
	.db	$00,$00,$3C,$00,$00,$3F,$3F,$3F	; (P)
	.db	$00,$00,$3C,$3C,$30,$30,$00,$00	; (Q)
	.db	$00,$00,$3C,$00,$00,$03,$3C,$3C	; (R)
	.db	$00,$00,$3F,$00,$00,$FC,$00,$00	; (S)
	.db	$00,$00,$E7,$E7,$E7,$E7,$E7,$E7	; (T)
	.db	$3C,$3C,$3C,$3C,$3C,$3C,$00,$00	; (U)
	.db	$3C,$3C,$3C,$3C,$3C,$3C,$C3,$C3	; (V)
	.db	$3C,$3C,$3C,$24,$00,$00,$18,$3C	; (W)
	.db	$3C,$18,$81,$C3,$C3,$81,$18,$3C	; (X)
	.db	$3C,$18,$81,$C3,$E7,$E7,$E7,$E7	; (Y)
	.db	$00,$00,$F1,$E3,$C7,$8F,$00,$00 ; (Z)
	.db $C3,$C3,$CF,$CF,$CF,$CF,$C3,$C3 ; ([)
	.db $C3,$C3,$F3,$F3,$F3,$F3,$C3,$C3 ; ([)
	.db $EF,$C7,$83,$01,$01,$FF,$FF,$FF ; (^)

global_palettes:
	.db $ff,$ff,$4a,$4a,$12,$b3,$7d,$bd					; WMP-style blues
	.db $ff,$ff,$00,$00,$98,$23,$e3,$cd					; Cemetech red
	.db $ff,$ff,$4a,$4a,$23,$84,$66,$4c					; TIFW green
	.db $ff,$ff,$4a,$4a,$69,$2d,$cb,$99					; Purples
	.db $00,$00,$ff,$ff,$b5,$b5,$4a,$4a					; Inverted grayscale
global_palettes_end:

global_palette:
	.db $ff,$ff,$4a,$4a,$12,$b3,$7d,$bd					; Start with WMP-style blues

BulletPoint:
	.dw global_palette
	.db 8,8
	.db $0a,$a0
	.db $2a,$a8
	.db $aa,$aa
	.db $aa,$aa
	.db $aa,$aa
	.db $aa,$aa
	.db $2a,$a8
	.db $0a,$a0
	
BulletPointErase:			; This is because I don't want to bother. 10 bytes; that's it
	.dw global_palette
	.db 8,8
	.db $00,$00
	.db $00,$00
	.db $00,$00
	.db $00,$00
	.db $00,$00
	.db $00,$00
	.db $00,$00
	.db $00,$00

slider:
	.dw global_palette
	.db SLIDER_WIDTH,SLIDER_HEIGHT
	.db $2a,$a8
	.db $80,$02
	.db $8f,$f2
	.db $7f,$fd
	.db $7a,$ad
	.db $6a,$a9
	.db $6a,$a9
	.db $6a,$a9
	.db $6a,$a9
	.db $6a,$a9
	.db $6a,$a9
	.db $6a,$a9
	.db $5a,$a5
	.db $5a,$a5
	.db $55,$55
	.db $15,$54
	
progSearchHeader:
	.db $BB,$6D,$C9,$31,$80,0	; Header? I wonder what the bytes after this mean...

;-------------------------------------------
;  Text	
;-------------------------------------------
Description:
	.db "mobileTunes 3 CSE",0
HelpSettingsTitle:
	.db "Help + Settings",0
;       "123456789012345678901234567"
#define cOB 'Z'+1			; Open square brace
#define cCB 'Z'+2			; Close square brace
HelpSettingsText:
	.db "Plays mobileTunes 3 music",0
	.db "from cemetech.net. Plug",0
	.db "2.5mm to 3.5mm adapter",0
	.db "into link port to listen.",0	
	.db cOB,"Y=",cCB,"/",cOB,"2nd",cCB," Play/Pause",0
	.db cOB,"WINDOW",cCB,"   Stop",0
	.db cOB,"ZOOM",cCB,"     Shuffle",0
	.db cOB,"TRACE",cCB,"    Help",0
	.db cOB,"CLEAR",cCB,"    Quit",0
	.db 0
	.db "Colors:",0
	.db 0
	.db "Press ",cOB,"CLEAR",cCB," to close",0
Palette_Pointer_String:
	.db ('Z'+3),0
ExtraSpace:
	.db "                        ",0

;-------------------------------------------
;  GUI Elements
;-------------------------------------------
controls1:
	.dw global_palette
	.db 56,52
	.db $00,$00,$00,$00,$00,$00,$55,$55,$55,$55,$00,$00,$00,$00
	.db $00,$00,$00,$00,$00,$15,$55,$55,$55,$55,$54,$00,$00,$00
	.db $00,$00,$00,$00,$01,$55,$40,$00,$00,$01,$55,$40,$00,$00
	.db $00,$00,$00,$00,$15,$50,$00,$00,$00,$00,$05,$54,$00,$00
	.db $00,$00,$00,$01,$55,$00,$00,$00,$00,$00,$00,$55,$40,$00
	.db $00,$00,$00,$05,$50,$00,$00,$00,$00,$00,$00,$05,$50,$00
	.db $00,$00,$00,$15,$00,$00,$3e,$aa,$aa,$bc,$00,$00,$54,$00
	.db $00,$00,$00,$54,$00,$0e,$a8,$00,$00,$2a,$b0,$00,$15,$00
	.db $00,$00,$01,$50,$00,$e8,$00,$00,$00,$00,$2b,$00,$05,$40
	.db $00,$00,$05,$40,$0e,$80,$03,$ff,$ff,$c0,$02,$b0,$01,$50
	.db $00,$00,$15,$00,$38,$03,$ff,$ff,$ff,$ff,$c0,$2c,$00,$54
	.db $00,$00,$14,$00,$e0,$ff,$ff,$ff,$ff,$ff,$ff,$0b,$00,$15
	.db $00,$00,$54,$03,$83,$ff,$ff,$ff,$ff,$ff,$ff,$c2,$c0,$15
	.db $00,$01,$50,$0e,$0f,$ff,$fe,$aa,$aa,$bf,$ff,$f0,$b0,$05
	.db $00,$01,$40,$38,$3f,$ff,$aa,$aa,$aa,$aa,$ff,$fc,$2c,$05
	.db $00,$05,$40,$28,$ff,$ea,$aa,$aa,$aa,$aa,$ab,$ff,$28,$01
	.db $00,$15,$00,$ef,$fe,$aa,$aa,$aa,$aa,$aa,$aa,$bf,$fb,$00
	.db $01,$54,$00,$bf,$fa,$aa,$aa,$aa,$aa,$aa,$aa,$af,$fe,$00
	.db $55,$50,$03,$bf,$ea,$aa,$aa,$aa,$aa,$aa,$aa,$ab,$fe,$c0
	.db $55,$00,$02,$ff,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$ff,$80
	.db $00,$00,$02,$fe,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$bf,$80
	.db $00,$00,$0e,$fe,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$bf,$b0
	.db $00,$00,$0b,$fa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$af,$e0
	.db $00,$00,$0b,$fa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$af,$e0
	.db $00,$00,$3b,$ea,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$ab,$ec
	.db $00,$00,$2b,$ea,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$ab,$e8
	.db $00,$00,$2b,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$e8
	.db $00,$00,$2a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a8
	.db $00,$00,$2a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a8
	.db $00,$00,$2a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a8
	.db $00,$00,$2a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a8
	.db $00,$00,$2a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a8
	.db $00,$00,$2a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a8
	.db $00,$00,$3a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$ac
	.db $00,$00,$0a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a0
	.db $00,$00,$0a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a0
	.db $00,$00,$0e,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$b0
	.db $00,$00,$02,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$80
	.db $00,$00,$02,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$80
	.db $00,$00,$00,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$00
	.db $00,$00,$00,$da,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$a7,$00
	.db $00,$00,$00,$16,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$94,$00
	.db $00,$00,$00,$35,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$5c,$00
	.db $00,$00,$00,$0d,$6a,$aa,$aa,$aa,$aa,$aa,$aa,$a9,$70,$00
	.db $00,$00,$00,$01,$5a,$aa,$aa,$aa,$aa,$aa,$aa,$a5,$40,$00
	.db $00,$00,$00,$00,$d5,$aa,$aa,$aa,$aa,$aa,$aa,$57,$00,$00
	.db $00,$00,$00,$00,$15,$5a,$aa,$aa,$aa,$aa,$a5,$54,$00,$00
	.db $00,$00,$00,$00,$0d,$55,$6a,$aa,$aa,$a9,$55,$70,$00,$00
	.db $00,$00,$00,$00,$03,$55,$55,$6a,$a9,$55,$55,$c0,$00,$00
	.db $00,$00,$00,$00,$00,$35,$55,$55,$55,$55,$5c,$00,$00,$00
	.db $00,$00,$00,$00,$00,$03,$55,$55,$55,$55,$c0,$00,$00,$00
	.db $00,$00,$00,$00,$00,$00,$39,$55,$55,$6c,$00,$00,$00,$00

controls2:
	.dw global_palette
	.db 12,8
	.db $55,$55,$40
	.db $55,$55,$40
	.db $55,$55,$00
	.db $55,$55,$00
	.db $55,$55,$c0
	.db $15,$55,$c0
	.db $01,$55,$40
	.db $00,$01,$40

controls3:
	.dw global_palette
	.db 28,28
	.db $00,$00,$03,$aa,$c0,$00,$00
	.db $00,$03,$aa,$00,$aa,$c0,$00
	.db $00,$3a,$00,$ff,$00,$ac,$00
	.db $00,$e0,$ff,$ff,$ff,$0b,$00
	.db $03,$8f,$ff,$ff,$ff,$f2,$c0
	.db $0e,$0f,$ff,$aa,$ff,$f0,$b0
	.db $08,$ff,$ea,$aa,$ab,$ff,$20
	.db $3b,$fe,$aa,$aa,$aa,$bf,$ec
	.db $2f,$fa,$aa,$aa,$aa,$af,$f8
	.db $2f,$ea,$bf,$ff,$fe,$ab,$f8
	.db $ef,$aa,$80,$00,$02,$aa,$fb
	.db $af,$aa,$80,$00,$02,$aa,$fa
	.db $ae,$aa,$80,$00,$02,$aa,$ba
	.db $aa,$aa,$80,$00,$02,$aa,$aa
	.db $aa,$aa,$80,$00,$02,$aa,$aa
	.db $aa,$aa,$80,$00,$02,$aa,$aa
	.db $6a,$aa,$80,$00,$02,$aa,$a9
	.db $6a,$aa,$80,$00,$02,$aa,$a9
	.db $1a,$aa,$aa,$aa,$aa,$aa,$a4
	.db $1a,$aa,$aa,$aa,$aa,$aa,$a4
	.db $35,$aa,$aa,$aa,$aa,$aa,$5c
	.db $0d,$6a,$aa,$aa,$aa,$a9,$70
	.db $03,$5a,$aa,$aa,$aa,$a5,$c0
	.db $00,$d6,$aa,$aa,$aa,$97,$00
	.db $00,$35,$6a,$aa,$a9,$5c,$00
	.db $00,$05,$55,$aa,$55,$50,$00
	.db $00,$03,$55,$55,$55,$c0,$00
	.db $00,$00,$02,$55,$80,$00,$00
	
controls4:
	.dw global_palette
	.db 28,28
	.db $00,$00,$03,$aa,$c0,$00,$00
	.db $00,$03,$aa,$00,$aa,$c0,$00
	.db $00,$3a,$00,$ff,$00,$ac,$00
	.db $00,$e0,$ff,$ff,$ff,$0b,$00
	.db $03,$8f,$ff,$ff,$ff,$f2,$c0
	.db $0e,$0f,$ff,$aa,$ff,$f0,$b0
	.db $08,$ff,$ea,$aa,$ab,$ff,$20
	.db $3b,$fe,$aa,$aa,$aa,$bf,$ec
	.db $2f,$fa,$aa,$aa,$ab,$af,$f8
	.db $2f,$ef,$fe,$aa,$fc,$eb,$f8
	.db $ef,$a0,$03,$ab,$00,$3a,$fb
	.db $af,$a0,$00,$ec,$00,$2a,$fa
	.db $ae,$aa,$a0,$30,$28,$aa,$ba
	.db $aa,$aa,$a8,$00,$aa,$aa,$aa
	.db $aa,$aa,$a8,$00,$aa,$aa,$aa
	.db $aa,$aa,$ac,$00,$eb,$aa,$aa
	.db $6a,$af,$f0,$20,$3c,$ea,$a9
	.db $6a,$a0,$00,$a8,$00,$3a,$a9
	.db $1a,$a0,$02,$aa,$00,$2a,$a4
	.db $1a,$aa,$aa,$aa,$a8,$aa,$a4
	.db $35,$aa,$aa,$aa,$aa,$aa,$5c
	.db $0d,$6a,$aa,$aa,$aa,$a9,$70
	.db $03,$5a,$aa,$aa,$aa,$a5,$c0
	.db $00,$d6,$aa,$aa,$aa,$97,$00
	.db $00,$35,$6a,$aa,$a9,$5c,$00
	.db $00,$05,$55,$aa,$55,$50,$00
	.db $00,$03,$55,$55,$55,$c0,$00
	.db $00,$00,$02,$55,$80,$00,$00
	
controls5:
	.dw global_palette
	.db 28,28
	.db $00,$00,$03,$aa,$c0,$00,$00
	.db $00,$03,$aa,$00,$aa,$c0,$00
	.db $00,$3a,$00,$ff,$00,$ac,$00
	.db $00,$e0,$ff,$ff,$ff,$0b,$00
	.db $03,$8f,$ff,$ff,$ff,$f2,$c0
	.db $0e,$0f,$ff,$aa,$ff,$f0,$b0
	.db $08,$ff,$ea,$aa,$ab,$ff,$20
	.db $3b,$fe,$aa,$aa,$aa,$bf,$ec
	.db $2f,$fa,$ac,$00,$3a,$af,$f8
	.db $2f,$ea,$b0,$00,$0e,$ab,$f8
	.db $ef,$aa,$80,$aa,$82,$aa,$fb
	.db $af,$aa,$82,$aa,$82,$aa,$fa
	.db $ae,$aa,$aa,$aa,$02,$aa,$ba
	.db $aa,$aa,$aa,$a8,$0a,$aa,$aa
	.db $aa,$aa,$aa,$ac,$2a,$aa,$aa
	.db $aa,$aa,$aa,$b0,$aa,$aa,$aa
	.db $6a,$aa,$aa,$82,$aa,$aa,$a9
	.db $6a,$aa,$aa,$82,$aa,$aa,$a9
	.db $1a,$aa,$aa,$aa,$aa,$aa,$a4
	.db $1a,$aa,$aa,$aa,$aa,$aa,$a4
	.db $35,$aa,$aa,$82,$aa,$aa,$5c
	.db $0d,$6a,$aa,$82,$aa,$a9,$70
	.db $03,$5a,$aa,$aa,$aa,$a5,$c0
	.db $00,$d6,$aa,$aa,$aa,$97,$00
	.db $00,$35,$6a,$aa,$a9,$5c,$00
	.db $00,$05,$55,$aa,$55,$50,$00
	.db $00,$03,$55,$55,$55,$c0,$00
	.db $00,$00,$02,$55,$80,$00,$00

controls6:
	.dw global_palette
	.db 54,36
	.db $ff,$ff,$ff,$ff,$ff,$ff,$fc
	.db $0f,$ff,$ff,$ff,$ff,$ff,$fc
	.db $01,$ff,$ff,$ff,$ff,$ff,$fc
	.db $00,$7f,$ff,$ff,$ff,$ff,$fc
	.db $00,$1f,$ff,$ff,$ff,$ff,$fc
	.db $00,$07,$ff,$ff,$ff,$ff,$fc
	.db $00,$01,$ff,$ff,$ff,$ff,$fc
	.db $00,$00,$ff,$ff,$ff,$ff,$fc
	.db $00,$00,$7f,$ff,$ff,$ff,$fc
	.db $00,$00,$3f,$ff,$ff,$ff,$fc
	.db $00,$00,$1f,$ff,$ff,$ff,$fc
	.db $00,$00,$0f,$ff,$ff,$ff,$fc
	.db $00,$00,$07,$ff,$ff,$ff,$fc
	.db $00,$00,$03,$ff,$ff,$ff,$fc
	.db $00,$00,$01,$ff,$ff,$ff,$fc
	.db $00,$00,$00,$ff,$ff,$ff,$fc
	.db $00,$00,$00,$7f,$ff,$ff,$fc
	.db $00,$00,$00,$3f,$ff,$ff,$fc
	.db $00,$00,$00,$1f,$ff,$ff,$fc
	.db $00,$00,$00,$0f,$ff,$ff,$fc
	.db $00,$00,$00,$07,$ff,$ff,$fc
	.db $00,$00,$00,$03,$ff,$ff,$fc
	.db $00,$00,$00,$01,$ff,$ff,$fc
	.db $00,$00,$00,$00,$ff,$ff,$fc
	.db $00,$00,$00,$00,$7f,$ff,$fc
	.db $00,$00,$00,$00,$3f,$ff,$fc
	.db $00,$00,$00,$00,$1f,$ff,$fc
	.db $00,$00,$00,$00,$0f,$ff,$fc
	.db $00,$00,$00,$00,$07,$ff,$fc
	.db $00,$00,$00,$00,$03,$ff,$fc
	.db $00,$00,$00,$00,$00,$ff,$fc
	.db $00,$00,$00,$00,$00,$3f,$fc
	.db $00,$00,$00,$00,$00,$0f,$fc
	.db $00,$00,$00,$00,$00,$03,$fc
	.db $00,$00,$00,$00,$00,$00,$7c
	.db $00,$00,$00,$00,$00,$00,$04

controls7:
	.dw global_palette
	.db 8,8
	.db $55,$55
	.db $55,$55
	.db $00,$55
	.db $fc,$55
	.db $ff,$55
	.db $ff,$55
	.db $55,$55
	.db $55,$55

controls8:
	.dw global_palette
	.db 8,8
	.db $55,$55
	.db $55,$55
	.db $00,$00
	.db $ff,$ff
	.db $ff,$ff
	.db $ff,$ff
	.db $55,$55
	.db $55,$55

controls_pp_pause:
	.dw global_palette
	.db 16,22
	.db $ff,$aa,$aa,$ff
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	.db $00,$aa,$aa,$00
	
controls_pp_play:
	.dw global_palette
	.db 20,17
	.db $fa,$aa,$aa,$aa,$aa
	.db $0f,$aa,$aa,$aa,$aa
	.db $00,$fa,$aa,$aa,$aa
	.db $00,$0f,$aa,$aa,$aa
	.db $00,$00,$fa,$aa,$aa
	.db $00,$00,$0f,$aa,$aa
	.db $00,$00,$00,$fa,$aa
	.db $00,$00,$00,$0f,$aa
	.db $00,$00,$00,$00,$fa
	.db $00,$00,$00,$00,$aa
	.db $00,$00,$00,$0a,$aa
	.db $00,$00,$00,$aa,$aa
	.db $00,$00,$0a,$aa,$aa
	.db $00,$00,$aa,$aa,$aa
	.db $00,$0a,$aa,$aa,$aa
	.db $00,$aa,$aa,$aa,$aa
	.db $0a,$aa,$aa,$aa,$aa

title_icon:
	.dw global_palette
	.db 8,8
	.db $5f,$ff
	.db $5f,$ff
	.db $5d,$57
	.db $5d,$57
	.db $5d,$57
	.db $fd,$7f
	.db $fd,$7f
	.db $f5,$7d

Palette_Select_Icon:
	.dw global_palette
	.db 16,16
	.db $00,$00,$55,$55
	.db $00,$00,$55,$55
	.db $00,$00,$55,$55
	.db $00,$00,$55,$55
	.db $00,$00,$55,$55
	.db $00,$00,$55,$55
	.db $00,$00,$55,$55
	.db $00,$00,$55,$55
	.db $aa,$aa,$ff,$ff
	.db $aa,$aa,$ff,$ff
	.db $aa,$aa,$ff,$ff
	.db $aa,$aa,$ff,$ff
	.db $aa,$aa,$ff,$ff
	.db $aa,$aa,$ff,$ff
	.db $aa,$aa,$ff,$ff
	.db $aa,$aa,$ff,$ff

ControlsPPLUT:
	.dw controls_pp_play
	.db 25,205			;x, y
	.dw controls_pp_pause
	.db 24,202

UpdateSlider_RestoreMask:
	.db 0,0,0,1,1,0,3,3,3,1,1,0,0,0
	.db 0,0,0,1,1,0,3,3,3,1,1,1,0,0
	.db 0,0,0,1,1,0,3,3,3,1,1,1,1,0
	.db 0,0,0,1,1,0,3,3,3,1,1,1,1,1

ShuffleIndicator:
	.dw global_palette
	.db 8,8
	.db $aa,$aa
	.db $95,$56
	.db $95,$56
	.db $55,$55
	.db $55,$55
	.db $55,$55
	.db $55,$55
	.db $55,$55
