	org	$3fb0-9						; $3fa7
	put	$3fb0-9
	
	fcb	$55
	fcb	$02
	fdb	$3fb0
	fdb	final-inicio
	fdb	$3fb0
	fcb	$aa
inicio		
	includebin "BMUPv14C.rom"
	includebin "DP50NEW.rom"
	includebin "EXTENDER.rom"
	if ((*-$3fa7)%256 > 0)
		rzb	 256-((*-$3fa7)%256)		; make it DW4 compatible (length multiple of 256 bytes)
	endif
final	END

