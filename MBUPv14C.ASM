;-----------------------------------------------------------------------------------------------------------
; MBASUP v1.4C	(2015-06-28 by Pere Serrat)
; used to switch to MAP1 and copy 
; the BASIC from ROM into RAM,
; the patched DOS must loaded into low RAM at $4000
; the Extender should be loaded at $6000
; and finally reset the machine
;-----------------------------------------------------------------------------------------------------------
			ORG	$3FB0
			PUT	$3FB0
;-----------------------------------------------------------------------------------------------------------
			ORCC	#$50			; disable interrupts
			LDX	#$8000		; point to std BASIC ROM
LOOP1		STA	$FFDE			; switch to MAP0 (RAM-ROM)
			LDD	,X				; get a word
			STA	$FFDF			; switch to MAP1 (all 64k RAM)
			STD	,X++			; put at destination
			CMPX	#$BFFF		; copied full BASIC?
			BLO	LOOP1			; no, loopback
			LDU	#$4000		; point to beginning of DOS loaded in low RAM
loop2		ldd	,u++			; get a word
			std	,x++			; put at destination
			cmpu	#$7D00		; copied all 8k of DOS + 6k of extender? ($7D00-$4000 = $3D00 = 15.616 bytes)
			blo	loop2			; no, loopback
			LDA	#$0E			; byte to put instead of $10. Sets bits X=$FFC0 step 2, 14 times instead of 16 times
			STA	$BB72			; at I/O initialisation, so staying in map1
			LDX	#$B3CA		; initial code to detect RAM-ROM limit
			LDD	#$120C		; NOP opcode 12 times to prevent overwritting (now it will be all RAM)
LOOP3		STA   ,X+			; fill that part of initialisation
			DECB					; already finished?
			BNE	LOOP3			; no, loopback
			LDA	#$8E			; opcode for LDX immed
			STA	,X+			; put after the NOPs
			LDD	#$7FFF		; get RAM limit
			STD	,X				; put after LDX to force end of Basic, of RAM, etc to be that value
			CLR	<$71			; mark COLD start
			JMP	$B3B4			; perform reset
;-----------------------------------------------------------------------------------------------------------
			if (* < $4000)
		   		rzb	 $4000-*		; fill until $3FFF included
		 	endif
;********************************************************************************************************************************
