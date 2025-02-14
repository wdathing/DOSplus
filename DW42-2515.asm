; -------------------------------------------------------------------------------------------------------------------------------
; This is the ROM-add-on that acts depending upon the drive number, via DW4 or via DOS as usual
; Verified 43 commands of the DosPlus49b, all of them worked flawlessly with drive / VDK of any size (180-360-360-720)
; I have left BOOT apart because it seems that only drive1 can be booted
; To BOOT an OS-9 or NitrOS-9 disk it should be mounted on DriveWire slot 0
;    and it has to be a special boot disk to communicate with the DW server via parallel adapter or Becker Port
; NOTE: The commands that modify directory sectors (T20), don't update backup T16 (by these DOS design)
; -------------------------------------------------------------------------------------------------------------------------------
; From $fc to $ff will contain a slot number for units 1-2-3-4
; if they are greater than 4, then they must be pointing to a reserved DW4 slot, else they are actual floppy drives (1 to 4)
; last modification: slot 0 is also considered to be Drivewire (for booting OS-9 or NitrOS-9)
; numbers from 1 to 4 are ONLY accepted at the UNIT with the SAME number and just for physical drives
; -------------------------------------------------------------------------------------------------------------------------------
; This extender is intended for DPlus50
; DW42-0.25.12 - 2021-05-28 by Pere Serrat (pser1)
; DW42-0.25.13 - 2021-05-29 Mike Miller: Added multiboot BOOT command to allow RSDOS boot track disks
;                                          Note - requires DOS repatch
; 									 Mike Miller: Update size output to display large filesizes in SDIR, SDRIVE
; DW42-0.25.14 - 2021-06-03 Pere Serrat: corrects RSboot upgrade
; DW42-0.25.15 - 2021-06-04 Pere Serrat: adds cpyMPIcfg (copy of MPI config byte $ff7f)
;													  and modifies tstMPI to be fully compatible with mega-mini-MPI
; -------------------------------------------------------------------------------------------------------------------------------

													; ROM routines
getNChr		equ	$9f							; get next char from input buffer
redLChr		equ	$a5							; get last read char from input buffer
basini 		equ	$8000							; basic rom beginning
syserr   	equ   $8344							; system error
okprmpt  	equ   $8371							; BASIC OK prompt, command loop
basvect2 	equ   $83ed							; initialize BASIC
basvect1 	equ   $841f							; reset BASIC vectors, reset stack etc
runbasic 	equ   $849f							; run_basic, enable IRQs etc
romcmd		equ	$84ed             		; rom entry to dispatch a tokenized command
ckComma		equ	$89aa							; check for comma routine
synerr 		equ   $89b4             		; rom entry to show syntax error
fcerr  		equ   $8b8d             		; entry to send message error
asc2Num		equ	$8e51							; convert ASCII to number (1 byte)
get16bN		equ   $8e83							; get a 16 bit number into x
sendCR		equ	$90a5							; send a CR to screen
outstr   	equ   $90e5							; print string at X+1 to DEVNUM
outnum 		equ   $957a			         	; print number in d to devnum
backspc  	equ   $9a89							; print a backspace to DEVNUM
resetRt		equ	$b3b4							; reset routine
wrmStrt		equ	$b44f							; warmStart routine
outchr   	equ   $b54a							; print char A to DEVNUM
getfnam  	equ   $b7aa							; read file name and length into $1d1
ioerror  	equ   $b84b							; IO_error
outRegA		equ	$bcab							; output reg A to screen as number
romini 		equ   $c000							; beginning of roms area 
gtbtsig 		equ	$c0f9							; get Boot signature
rdbttrk 		equ	$c14e							; load disk track
; -------------------------------------------------------------------------------------------------------------------------------

													; VARIABLES
ErasCtr  	equ   $03							; sector count in eraser
noexec   	equ   $10							; inhibit exec after loading
bootflag 	equ   $11							; zero if run from command line, $55 if autorun on boot
dnum   		equ   $12							; assigned drive number by dw4 server
tokens 		equ   #18               		; to be incremented each time words are added  =  NEW COMMANDS
newtok 		equ   $1a               		; this works with dos cartridge plugged in!
FPA0			equ	$50							; number of bytes to flash
oldSlot		equ	$52							; FPA2 (re-used)
oldBank		equ	$53							; FPA3 (re-used)

bufPtr		equ	$76							; (re-used) pointer to buffer end for new name
chksum 		equ   $76							; checksum of the sector to be sent to dw4 (2 bytes)
dsktyp		equ	$76							; (re-used) only for SDRIVE creating file. 0=DSK, 1=VDK
dSource		equ	$76							; 2 bytes (re-used)
endadr 		equ   $76							; last loaded address - probably no conflict but collides with 	chksum 	equ   $76	
hdwFlo		equ	$76							; slot number where floppies are found (only for tstHDW)

hdwSdc		equ	$77							; slot number where SDC is found (only for tstHDW)
TargBank 	equ   $86							; bank to be flashed
curlow 		equ   $89							; low byte of cursor position
usrptr 		equ   $b0               		; pointer to usr vector base
bigfil 		equ   $e6							; too big file flag - probably no conflict but collides with internal use of $e6 in format by dplus
retry			equ	$e7							; to catch 1st acess to DW4 drives error
DCDRV			equ	$eb							; bank number
DCBPT			equ	$ee							; pointer to subroutines

cpyPtr		equ	$f8							; (re-used) pointer to buffer end for SDIR
numUni		equ	$f8							; unit number for new Commands (link, mount, bank)
oriNum		equ	$f8							; number of requested slot-bank (re-used)

cpyWhat		equ	$f9							; command that identifies BANK - SLOT (re-used)
numDir		equ	$f9							; number of received dirs (re-used)
sdcSlot		equ	$f9							; 1 byte (re-used)
swMap1		equ	$f9							; flag to switch to map1 (re-used)
trknum		equ	$f9							; tracknumber in process for DSKINIT 4th patch

dDest			equ	$fa							; 2 bytes (re-used)
geodat		equ	$fa							; numTracks - numSect x track (2 bytes)
itsRead		equ	$fa							; number of received items in a dir page (re-used)
valPar		equ	$fa							; (re-used) value for parameter to send into B

cmdNum		equ	$fb							; (re-used) command code to be sent
itsDone		equ	$fb							; number of items shown of a page
secxtrk		equ	$fb							; numSect x track (low byte of previous word)

drvtab		equ	$fc							; system table for linked numbers to the 4 units (fc-fd-fe-ff)
floSDC		equ	$114							; bit 0 for unit 1, bit 1 for unit 2. If 0 use Floppy, if 1 use SDC
 														; bit 2 is 1 if Floppies present else 0
 														; bit 3 is 1 if SDC present else 0
 														; bit 5-4 show the slot where floppies are connected (0-3)
 														; bit 7-6 show the slot where SDC is connected (0-3)
idxHD			equ	$115							; index to a HD array (maximum 90 discs of 720+1 sectors DW4 VDK type) (values 1-90)
newstb 		equ   $134              		; base of new stub
hokini 		equ   $15e							; first system hook
hokend 		equ   $1a8							; last system hook
sstack   	equ   $1cd							; backup of original stack
sector   	equ   $1cf							; counter, used when loading file contents, just below namebuf/DECB header

nambuf 		equ   $1d1							; fixed by BASIC, read filename function which stores in $1d1
namebuf  	equ   $1d1							; fixed by BASIC read filename function

decbhdr  	equ   namebuf						; DECB headers get copied in here
startbuf 	equ   namebuf+8					; dw input data buffer, can reuse buffer after DECB header slot
endbuf   	equ   startbuf+256				; end of buffer area
fST16			equ	$3d9							; first sector of Track 16 (backup DIR)
newusr		equ	$3ea							; will use $3ea - $3fd = 20 bytes (New USR area)
cpyMPIcfg	equ	$03ff							; candidate to be a copy in RAM of $ff7f byte ($3EB - $3FF seem safe places ...)
; -------------------------------------------------------------------------------------------------------------------------------

													; RAM area
ramini 		equ   $3000							; where to copy the rom
rammax 		equ   $6eff							; ram limit to send to highram (16k - 256 bytes) as a rom program to be run
rmaxld 		equ   $6fff							; ram limit to load into low memory (for basic roms)
; -------------------------------------------------------------------------------------------------------------------------------

													; SAM area 
R1CLR			equ	$ffd8							; to clear fast poke $ffd9
RAMROM		equ	$ffde							; to go MAP0, resetting $ffdf (MAP1)
; -------------------------------------------------------------------------------------------------------------------------------

													; HDW related addresses
BBOUT			equ	$ff20							; used for DW4
PIA1Base 	equ   $ff20							; used by DWRead/DWWrite
BBIN			equ	$ff22							; used for DW4
CMDREG   	equ   $ff40 						; command register (write)
STATREG  	equ   $ff40 						; status register (read)
PREG1    	equ   $ff41 						; param register 1
PREG2    	equ   $ff42 						; param register 2
DATREGA  	equ	PREG2       			   ; first data register
PREG3    	equ   $ff43							; param register 3
DATREGB  	equ   PREG3       			   ; second data register
CTRLATCH 	equ   $ff48 						; controller latch (write)
FLSHREG  	equ   $ff4a							; Flash Register in SDC
BANKREG  	equ   $ff4b							; Bank select in SDC
;													HIGH nibble = ROM        (/CTS)
;													LOW  nibble = HARDWARE   (/SCS)
MPIcfg		equ	$ff7f							; value used in Tandy MultiPacks to save slots used (/CTS and /SCS)
; -------------------------------------------------------------------------------------------------------------------------------

													; CONSTANTS 
 													; Status Register Masks
BUSY     	equ   %00000001   			   ; set while a command is executing
READY    	equ   %00000010   			   ; set when ready for a data transfer
FAILsdc  	equ   %10000000   			   ; set on command failure
 													; Mode and Command Values
destPtr		equ	0								; ofsset to get to the pointer to destination buffer (2 bytes)
CMDMODE  	equ   $0b							; was $43 for CoCo - control latch value to enable command mode
dimask 		equ   $50							; disable IRQ and FIRQ
IntMasks 	equ   $50							; used by DWRead/DWWrite
CMDREAD  	equ   $80         			   ; read logical sector
CMDWRITE 	equ   $a0         			   ; write logical sector
eimask 		equ   $af							; enable interrupts
CMDEX    	equ   $c0         			   ; extended command
OP_READEX	equ   'R'+$80						; $D2 - Read one sector (DW4 opcode)
CMDEXD   	equ   $e0         			   ; extended command with data block
OP_REREADEX	equ   'r'+$80						; $F2 - Re-read one sector (DW4 opcode)
E_CRC    	equ   $f3            			; Same as NitrOS-9 E$CRC
; ===============================================================================================================================

												; CODE BEGINNING
      IF drgbin > 0							; to create Patcher - Extender binaries
         org   $e000-9						; to create the BIN header
			fcb	#$55							; header values for a Dragon binary file. magic 1st byte
			fcb	#$02							; file type
			fdb	#$6000						; load
			fdb	#absEnd-$e000				; length
			fdb	#$6002						; exec
			fcb	#$aa							; magic end byte
		ELSE
			org	$e000							; real program beginning
         put   $e000							; allocated at $e000
      ENDIF
; -------------------------------------------------------------------------------------------------------------------------------

												; PROGRAM INIT
mark		fcb	$45,$49						; "EI" mark to signal to the DOS that this extension wants to be called to initialize itself
			jmp	initext						; initialize extension
			lbra	reptch						; direct jump to DOSPlus patcher

; -------------------------------------------------------------------------------------------------------------------------------
; Entry point for DOS command dispatcher. 1st patch
; Data received:
; 	A = track number
; 	B = command code  (2=READ - 3=Unknown - 4=WRITE - 7=VERIFY(dummy read)
; 	X = address of system variable "number of sectors x track" for this drive 
; 	Y = LSN
; 	U = sector counter (if not directory track) 
; -------------------------------------------------------------------------------------------------------------------------------
entry    pshs  a								; save register 'A' (code overwritten. It's the first instruction in the DOS)
			pshs  cc								; save flags
			bsr	phyvir						; detect physical or virtual drive
			bcc	dw4							; if greater than 4 is a DriveWire slot number
			tsta									; is linked value equal 0? - demanat per KenH
			beq	dw4							; yes, so DriveWire slot 0

; -------------------------------------------------------------------------------------------------------------------------------
; The DOS part respects the original code
; -------------------------------------------------------------------------------------------------------------------------------
todos		puls	cc								; restore flags 
			lbsr	flOrSd						; control access to floppy or SDC
			lda   #$02     					; get value #2 (number of retries). It's the second instruction in the DPlus49b
			jmp	$c10a							; back to next sentence of DOS original code

; -------------------------------------------------------------------------------------------------------------------------------
; Detects if drive is physical or virtual (code in A)
; returns Carry set if DW4 slot number detected (now any value greater than 4)
; must use extended addressing because for DSKINIT, DP arrives here with $FF and if not virtual should return like that
; -------------------------------------------------------------------------------------------------------------------------------
phyvir	pshs	x								; save working register
			ldx	#drvtab-1					; point to 1 byte before the slots table
			lda	>$00eb						; get drive number
			lda	a,x							; get slot number for that drive
			sta	>dnum							; put it into DW4 variable
			cmpa	#5								; is a DW4 number? - if not, Carry will be set
			puls	x,pc							; restore register and return

; -------------------------------------------------------------------------------------------------------------------------------
; The dw4 part distinguishes between READ, VERIFY and WRITE opcodes
; -------------------------------------------------------------------------------------------------------------------------------
dw4   	puls 	cc								; get saved flags
												; to avoid problems created by SOUND and PLAY over DW4
			pshs	a								; save register
			lda	#2								; get value to mark output
			sta	BBOUT							; to DW4
			clra									; set counter
dw4L01	deca									; decrement counter
			bne	dw4L01						; not zero, loopback
			puls	a								; restore register
			pshs  b,x,y,u						; save rest of registers (A was already onto stack) before recalculatimg Y
			orcc  #dimask						; disable interrupts to work with dw4							
			bsr	calcy							; do not relay on Y. Recalculate it from $eb-ec-ed
			bsr	ctrlHD						; control index in HD if needed
		 	leax  1,y               		; X points now to the rigth dw4 sector (skipping the header) - [DW needs registers X-Y inverted!)
		 	ldy   <$ee    						; Y points to RAM buffer address
       	cmpb  #2								; is this a read command?
			beq   reddw4						; yes, read sector from dw4
       	cmpb  #7								; is this a verify command?
			bne   wrtdw4						; no, write sector to dw4
			ldy	#$fcff						; initial byte of 256 area to be overwritten while verifying
			bra	reddw4						; go read that sector
wrtdw4	jsr   dowrit						; write a sector from buffer to dw4
			bra   verify						; verify operation result

; -------------------------------------------------------------------------------------------------------------------------------
; This part asks dw4 a sector to be read and dw4 log reflects it accordingly. It is as simple as possible.
; -------------------------------------------------------------------------------------------------------------------------------
reddw4 	jsr   DoRead						; call dw4 to read one sector
			bcs	verify						; if operation error, give message
			bne	verify						; if incompleted, give message
			ldb	,s								; get pushed B (opcode / command)
			cmpb	#2								; is it read?
			bne	jstvfy						; no, adapt flags and exit
			jsr	correct						; correct LSN if needed
verify	puls  b,x,y,u					   ; restore received registers
			puls  a								; and first one pushed
	 		bcs   failed						; detect operation error
       	bne   failed						; detect incomplete read operation
			clrb									; operation succesful - no error code returned in B
outgo		andcc #eimask						; enable interrupts
			rts									; return to caller
failed   tst	<retry						; is flag retry ON ($01)?
			beq	fail01						; no, exit
			bmi	fail01						; if negative exit too
			neg	<retry						; to get value $FF
			bra	entry							; try it again
fail01	ldb   #$a6							; set error code (unknown = ??)
			orcc	#%00000001					; set Carry flag (error)
			bra   outgo							; return
jstvfy	orcc	#%00000100					; set flag Z=1		
			bra	verify						; exit via verify

; -------------------------------------------------------------------------------------------------------------------------------
; This is used to calculate the LSN number from system variables $ec (track) and $ed (sector)
; Controls the number of sides of the disks to change 18 s/t to 36
; returns the calculated LSN in register Y
; -------------------------------------------------------------------------------------------------------------------------------
calcy		pshs	d,x							; save working registers
			ldx	#$6a6							; point to byte before sectors x track table
			ldb	<$eb							; get drive number
			lda	b,x							; get number of sectors x track for this drive
calcy01	ldb	<$ec							; get track number
			mul									; calculate sectors amount
			tfr	d,x							; X gets this value
			lda	<$ed							; get sector number
			deca									; decrement
			leay	a,x							; add to X and pass result to Y
			puls	d,x,pc						; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Aternative entry for DSKINIT that comes without data in $6a7-8-9-a. So this one relies on system variable $f4 (as numSides)
; -------------------------------------------------------------------------------------------------------------------------------
caldi		pshs	d,x							; save working registers
			lda	#$12							; sectors x track (single side)
			tst	<$f4							; is disk single sided?
			beq	ecaldi						; yes, skip next
			asla									; double number of sectors x track
ecaldi	bra	calcy01						; jump to calculation routine

; -------------------------------------------------------------------------------------------------------------------------------
; to apply index to a DW4 HD on drive 4 if it is the case
; -------------------------------------------------------------------------------------------------------------------------------
ctrlHD	pshs	a,b							; save register
			ldb	<$eb							; get drive number
			cmpb	#4								; is it 4?
			bne	eCtrl							; no, exit
			ldb	idxHD							; get index of disc to be accesed
			cmpb	#1								; is index 0 or 1?
			bls	eCtrl							; yes, no offset
			decb									; else deduct one
			leay	b,y							; Y = LSN + index
			lda	#240							; maximum acceptable value that can be used to multiply the index
			mul									; D = 240 * Index  (if index=90, result 21360 -> $5370) always will be positive
			leay	d,y							; Y = LSN + 241*index
			leay	d,y							; Y = LSN + 481*index
			leay	d,y							; Y = LSN + 721*index  now we have calculated the real LSN to be used, no loops!
eCtrl		puls	a,b,pc						; restore register and return

; -------------------------------------------------------------------------------------------------------------------------------
; Process that substitutes the calls to "WriteTrack". It is used ONLY by DSKINIT
; Writes the 18 sectors of one side in a loop
; If sector number received is 0, will write to side 1, else ($ff) will do side 2 
; -------------------------------------------------------------------------------------------------------------------------------
inidsk	pshs	cc								; save flags
			jsr	phyvir						; is drive physical or virtual?
			bcc	virtual						; virtual, process it
			tsta									; is linked value 0?
			beq	virtual						; yes, so DW4 slot 0
			puls	cc								; get saved flags
			ldd   #$47f4						; physical, do nothing. This was overwritten by the patch
			jmp	$c328							; back to DOS
												; to avoid problems created by SOUND and PLAY over DW4
virtual	tst	<$ec							; is track 0?
			bne	iniD01						; no, skip section
			pshs	a								; save register
			lda	#2								; get value to mark output
			sta	BBOUT							; to DW4
			clra									; set counter
iniL01	deca									; decrement counter
			bne	iniL01						; not zero, loopback
			puls	a								; restore register
iniD01	puls	cc								; get saved flags
			orcc  #dimask						; disable interrupts to work with dw4	
			clra									; value 0
			tfr	a,dp							; set direct page to 0
			setdp	0								; notify compiler
			pshs	d,x,y,u						; save registers
			bsr	filbuf						; fill buffer
			ldb	<$ec							; get track number
			cmpb	#$14							; is this the directory (T20 track)?
			bne	wrt00							; no, init the track
			tst	<$f3							; yes, is it side 2? (0 means side 1)
			bne	wrt00							; init side 2!
												; yes, don't waste time, it will be filled later
			clra									; mark no error
			bra	eIni01						; exit
wrt00		inca									; 1st track of side 1
			tst	<$f3							; is it side 1?
			beq	wrt01							; yes, skip next one
			lda	#19							; get 1st track of side 2
wrt01		sta	<$ed							; update system variable
			bsr	caldi							; calculate first LSN to write to (into Y)
			bsr	ctrlHD						; control index of HD if needed
		 	leax  1,y               		; X points to the rigth dw4 sector (skipping the header) - [DW needs them inverted!)
		 	ldy   <$ee    						; Y points to RAM buffer address
			ldb	#18							; number of sectors to be initialized
			stb	,-s							; put counter in stack
wrtmor	jsr	dowrit						; write sector to DW4 
			bcs   failed2						; detect operation error
       	bne   failed2						; detect incomplete read operation
			leax	1,x							; point to next LSN
			dec	,s								; dec counter
			bne	wrtmor						; if not yet 0, do next one
			leas	1,s							; get rid of counter
eIni01	puls	d,x,y,u					   ; restore registers (0 will be pulled for A)
eIniDsk	tsta									; update flags 
			andcc #eimask						; enable interrupts
			rts									; return
failed2	leas	1,s							; get rid of counter
			puls	d,x,y,u					   ; restore registers
			lda	#$ff							; mark error for Dskinit function
		   orcc	#$01							; set Carry flag (error)
			bra   eIniDsk						; return to caller

; -------------------------------------------------------------------------------------------------------------------------------
; Fill the buffer pointed by $ee all with value $e5
; -------------------------------------------------------------------------------------------------------------------------------
filbuf	pshs	b,x,u							; save registers
			ldx	<$ee							; get buffer address
			ldu	#$e5e5						; value to put
			bsr	cpu2xb						; put 128 times U at X
			puls	b,x,u,pc						; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Fill the directory track T20 for DW4 and floppy as well
; Only used by DSKINIT, but to be sure, it controls a return address in the stack to work else returns
; -------------------------------------------------------------------------------------------------------------------------------
fildir	pshs	d,u							; save working registers
			ldd	#$c9ef						; return address when called to write directory tracks by DSKINIT
			cmpd	4,s							; is it the write track call?
			bne	efildir						; not the write track command, skip fill subroutines
			bsr	fil16s						; fill buffer for the 16 Dir sectors
			bsr	ffat1							; fill buffer for FAT 1st sector
			jsr	ffat2							; fill buffer for FAT 2nd sector
efildir	puls	d,u							; restore registers
			ldx	#$800							; overwritten line by interceptor
			jmp	$c9f6							; back to DOS

; -------------------------------------------------------------------------------------------------------------------------------
; Fill the buffer for the 16 directory sectors with 10 unused entries
; -------------------------------------------------------------------------------------------------------------------------------
fil16s	ldx	#$a00							; point to third buffer
			ldu	<$8a							; get 16 bits zero
			bsr	cpu2xb						; put 128 times U at X
			lda	#$89							; code to mark not used entry
			ldx	#$a00							; point to the beginning again
fild02	sta	,x								; mark an entry
			leax	25,x							; point to next one
			cmpx	#$ae8							; done 10 entries?
			blo	fild02						; no, loop back
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Put value in U in bytes pointed by X, B times (each time a word)
; -------------------------------------------------------------------------------------------------------------------------------
cpu2xb	ldb	#128							; number of words to write
cp01		stu	,x++							; write one
			decb									; decrement counter
			bne	cp01							; if not 0, loopback
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Fill the first FAT sector. Must mark as used only the 18 sectors of Tracks 20 and 16 if applies
; -------------------------------------------------------------------------------------------------------------------------------
ffat1		ldx	#$800							; point to first buffer
			lda	<$f2							; get number of tracks	
			ldb	#$12							; sectors x track (single side)
			tst	<$f4							; is disk single sided?
			beq	ffat100						; yes, skip next
			aslb									; double number of sectors x track
ffat100	std	<geodat						; save geometry data for later use
			ldb	#90							; there are 180 FAT bytes, so 90 words, but 180k discs need only HALF that!
			ldu	<geodat						; get geometry data
			cmpu	#$2812						; is it a Single Sided Single Density disk (180k)?
			bne	ffat101						; no, skip next
			rorb									; yes, divide by 2 to get 45 words
ffat101	ldu	#$ffff						; code to mark 16 free sectors
			bsr	cp01							; copy B times U at X
			leau	1,u							; create 0 value
ffat102	stu	,x++							; clear a word
			cmpx	#$900-4						; reached end of buffer minus 4 bytes to put geometry data there?
			blo	ffat102						; no, loopback
			ldd	<geodat						; get geometry data
ffat103	std	,x++							; save it in 1st FAT sector
			coma									; complement
			comb									; both bytes
			std	,x								; store again in FAT
			ldb	#36							; This the FAT byte where dir T16 begins for single sided discs
			lda	<secxtrk						; get sectors x track
			cmpa	#$12							; is it single?
			beq	ffat104						; yes, skip next
			aslb									; double to 72 (the same place for Double Side disks)
ffat104	pshs	b								; save value for later use
			lda	#$f0							; flag value for parameter '/' (leave T16 free for data storage)
			cmpa	<$e6							; has $e6 this value?
			beq	ffat105						; yes, don't mark sector T16 as used								
			bsr	markfat						; mark used sectors of T16
ffat105	ldb	<secxtrk						; get sectors x track
												; distance between T16 and T20 is 4 * tracks * N_Sxt / 8 bits = NSxt / 2
			rorb									; convert distance to FAT bytes to add to
			addb	,s+							; the previously pushed FAT byte for T16, clear stack
markfat	ldx	#$800							; point to the begining of the FAT
			leax	b,x							; add offset to 1st DIR sector byte of that track
			clra									; 16 bit zero
			clrb									; to mark 16
			std	,x++							; used sectors by DIR track
			lda	#%11111100					; and two zero bits more
			sta	,x								; to complete the 18 reserved sectors for any directory Track
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Fill the second FAT sector. This is only used for 720k discs
; -------------------------------------------------------------------------------------------------------------------------------
ffat2		ldx	#$900							; point to second buffer
			ldd	<geodat						; get geometry data
			cmpd	#$5024						; is it a 720K disc?
			bne	ffat201						; no, do not put $FFs
			ldu	#$ffff						; to mark free sectors
			ldb	#90							; must mark 180 bytes
			bsr	cp01							; copy B times U at X
ffat201	ldu	<$8a							; get 16 bits zero
ffat202	stu	,x++							; put 2 zero bytes
			cmpx	#$a00							; reached end of buffer?
			blo	ffat202						; no, loopback
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Keep the good track number to avoid strange sequences. Called by DSKINIT and
; called from other points, one of them the COPY routine and from SAVE as well ...
; Has a control to detect that it is called from DSKINIT, else do nothing at all!
; when applied to DW4 the track number suffers non lineal sequences. Here we correct that
; -------------------------------------------------------------------------------------------------------------------------------
modtrk	pshs	d								; save register
			ldd	#$c9e1						; the return address that identifies DSKINIT
			cmpd	7,s							; is the one corresponding to DSKINIT?
			bne	notdsk						; no, do nothing
			lda	<$ec							; get track number
			sta	<trknum						; save in variable
			jsr	$d36b 						; (buf_read_sector) - this line was overwritten by patch
			lda	<trknum						; get saved track number
			sta	<$ec							; back to its place
emodtrk	puls	d								; restore register
			jmp	$d1a7							; back to DOS
notdsk	jsr	$d36b							; (buf_read_sector) - this line was overwritten by patch
			bra	emodtrk						; clean stack and back to DOS

; -------------------------------------------------------------------------------------------------------------------------------
; Part to detect a bigger than 180k disc and update tables accordingly. The same if disc was changed
; If read sector has no geometry data, we go to the other possible localization for directory ($168 - $2D0) (+1 for DW4)
; X is updated to the new LSN to be read, if necessary
; must be avoided ONLY when working with an HD at unit 4 (idxHD $115 >1)
; -------------------------------------------------------------------------------------------------------------------------------
correct	pshs	d,y,u							; save registers
			lda	<$eb							; get unit number
			cmpa	#4								; is it #4?
			bne	cor00							; no, skip control
			tst	idxHD							; working on a HD in a disk with index > 0?
			bne	ecorok						; yes, exit
cor00		ldd	<$ec							; get track-sector numbers
			cmpd	#$1401						; is it 1st FAT sector?
			bne	ecorok						; no, exit
			ldd	#$c869						; get return address from BACKUP command
			cmpd	20,s							; is this the case?
			beq	ecorok						; yes, exit
			ldu	<$ee							; get read buffer address
			ldd	$fe,u							; get complemented geodata bytes
			coma									; complement both bytes
			comb									; A=tracks; B=sectors x track ... possible values: 2812 - 2824 - 5012 - 5024
													; their 1st FAT sector will equal that one:        0169 - 02d1 - 0169 - 02d1 (+1 due to DW4) 
			cmpd	$fc,u							; do they equal the not complemented ones?
			beq	ecorok1						; yes, we have a right FAT sector, update system tables and exit
													; Bad 1st FAT sector read, we must change single <--> double as required
			cmpx	#$0169						; was sector read $0169 (single sided)
			bne	cor01							; no, change to single sided
			ldx	#$02d1						; yes, change to double sided
			bra	cor02							; go save new 1st FAT LSN
cor01		ldx	#$0169						; get single sided value
cor02		bsr	updtab						; update system tables (in some cases they are not yet filled)
			ldy	<$ee							; get buffer address
			jsr	DoRead						; read newly calculated T20 1st FAT LSN
			bra	ecor							; exit
ecorok1	bsr	updtab						; update system tables
ecorok	orcc	#%00000100					; set flag Z=1, result OK
      	andcc	#%11111110					; clear Carry flag, no errors
ecor		puls	d,y,u,pc						; restore registers and return			

; -------------------------------------------------------------------------------------------------------------------------------
; Updates system tables using param X as LSN for 1st DIR track (should get here with DW4 value)
; Affects system tables at $61c (dir LSN) and $6a7 (sectors x track)
; -------------------------------------------------------------------------------------------------------------------------------
updtab	ldu	#$616							; point 6 bytes before drives info table			
			ldb	<$eb							; get drive number
			lda	#6								; register length
			mul									; calculate offset
			leay	-1,x							; system works with $0168 or $02d0 (DW4 adds one because of the header)
			sty	d,u							; put LSN at requested drive table
			lda	#$12							; default value for single sided
			cmpx	#$0169						; is new value single sided?
			beq	cor03							; yes, skip next
			asla									; double value
cor03		ldu	#$6a6							; point 1 byte before sectors x track table
			ldb	<$eb							; get drive number
			sta	b,u							; update value in table for that drive
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Updates table $6a6 if drive is virtual. Uses data from stack (only BACKUP)
; -------------------------------------------------------------------------------------------------------------------------------
patbck	pshs	d,x							; save registers
			ldx	#drvtab-1					; point to 1 byte before the slots table
			lda	,u								; get source drive from stack hole
			bsr	updsxt						; update sectors x track
			lda	9,u							; get destination drive from stack hole
			bsr	updsxt						; update sectors x track
			puls	d,x							; restore registers
			lda	8,u							; these 2 sentences were
			mul									; overwritten by patcher
			jmp	$c839							; back to DOS

; -------------------------------------------------------------------------------------------------------------------------------
; Updates sectors x track of that drive (comes in A)
; -------------------------------------------------------------------------------------------------------------------------------
updsxt	ldb	a,x							; get associated slot number in drives table
			beq	upds01						; if cero is a special DW4 case, process it
			cmpb	#4								; is it lower than 5?
			bls	eupd							; yes, is a physicall drive, do not process it
upds01	pshs	x								; save register
			ldx	#$6a6							; point 1 byte before sectors x track table
			ldb	8,u							; get number of sectors x track in stack hole
			stb	a,x							; into table for that drive
			puls	x								; restore register
eupd		rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; This part avoids calling a hardware routine when the drive is virtual
; and when floppies allow it, in order to avoid that the FDC floppy light stays on forever
; -------------------------------------------------------------------------------------------------------------------------------
noHdw		pshs	a,cc							; save A and flags
			jsr	phyvir						; detect drive type
			bcc	retHdw						; if virtual, return
			tsta									; is linked value 0?
			beq	retHdw						; yes, so DW4 slot 0
			puls	a,cc							; restore A and flags
												; so it goes for floppies or SDC
			lbsr	flOrSd						; will take care of destination hdw			
			orcc  #dimask						; first overwritten instruction
			ror	,s							   ; second overwritten instruction
			jmp	$c174							; back to DOS
retHdw	tst	<retry						; is flag off (0)?
			bne	ret01							; no, skip next
			inc	<retry						; turn flag ON ($01)
ret01		leas	2,s							; get rid of 2 pushed regs
			ror	,s								; get rid of pushed carry value
			puls	a,cc,dp,x,y,u,pc			; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; This intercepts the std DOS reset routine
; -------------------------------------------------------------------------------------------------------------------------------
dosReset lbsr	initask						; put initial values
			jmp	wrmStrt						; call basic warmstart

; -------------------------------------------------------------------------------------------------------------------------------
; This part sends sectors to dw4 to be written. It is a block very similar to DoRead() but must work a bit different:
; Must send three blocks of data: The dw4 command, the data and the checksum, all of them in just one chunk
; -------------------------------------------------------------------------------------------------------------------------------
dowrit 	lda   <dnum							; get drive number (real dw4 number)
       	clrb						         ; LSN bits 23-16
       	pshs  a,b,x,y			         ; save registers / data					STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum
       	ldx	#0								; zero to calculate Checksum for that sector
       	clra									; byte counter (256)
calc   	ldb	,Y+							; read a byte
       	abx									; add to checksum
       	deca									; decrement counter
       	bne   calc							; if not all 256, keep on
       	stx   <chksum						; save checksum
		 	lda   #$57                   	; dw4 OP_WRITE (write one sector)
rewrit 	pshs  a					         ; save command onto stack				STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum, OP_WRITE
       	leax  ,s				         	; point X to top of stack
       	ldy   #5      			      	; 5 bytes
       	jsr   DWWrite		         	; send 5 bytes to dw4:	OP_WRITE, dnum, $0, Sector(H-L)
       	puls  a					         ; discard command							STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum
       	ldx   4,s				         ; get sector buffer pointer = received Y
       	ldy   #$0100			         ; to write 256 bytes
       	jsr   DWWrite		         	; send sector (256 bytes)
		 	ldy   <chksum						; get calculated checksum
       	pshs  y					         ; save it onto stack			   		STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum, ChksumL, ChksumH
       	leax  ,s				         	; point X to top of stack
       	ldy   #2				         	; 2 bytes
       	jsr   DWWrite			      	; send checksum to dw4
									   			; get answer from dw4 for the whole pack
		 	leay  1,y				         ; after a DWRITE Y returns zeroed, so this way it becomes equal to one
       	jsr   DWRead		         	; read dw4 answer	- this overwrites dnum byte
       	leas  2,s				         ; discard checksum						STACK: RETADRS, YL, YH, SectorL, SectorH, $0, Returnedvalue
       	bcs   writex			         ; detected framing error
       	bne   writex			         ; detected not all bytes received
       	lda   ,s				         	; get server answer (in old dnum position)
       	beq   writex			         ; zero=OK, go out
       	cmpa  #$F3				         ; error type = #E_CRC?
       	bne   writer			         ; not this one, go out signaling error
		 	lda   <dnum        		      ; get drive number
       	sta   ,s				         	; put again in stack						STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum
       	lda   #$77							; dw4 code for OP_REWRITE
       	bra   rewrit			         ; go to rewrite same sector again
writer 	comb						         ; sets the carry flag (ERROR)
writex	puls  a,b,x,y,pc             	; restore received registers and return      STACK: - - -

; -------------------------------------------------------------------------------------------------------------------------------
psmsg		fcc	13,"DW4&SDC EXTENDER (V0.25.15)",13
			fcn	   "BY PERE SERRAT (2021)",13,13
; --------------------------------------------------------------------------------------------------------------------------------			
ldtextr 	fcn   / LOADING ROM/,13						; initial message to the user
endtxt 	fcn   $0d, /ROM LENGTH: /					; message to show rom length
bigrom 	fcn   $0d, /ROM TOO BIG/					; message for roms greater than 16k-256bytes
notdw4	fcn	/NOT A DW4 SLOT/,13					; message for lrom without params and with default drive <> dw4
low    	fcn   /TOKEN TOO LOW/,13					; message for not recognized token
high   	fcn   /TOKEN TOO HIGH/,13					; message for not recognized token
drverr	fcn	/DRIVE MUST EQUAL UNIT/,13			; message for drive <> unit for physical drives
alrput	fcn	/IS LINKED TO ANOTHER UNIT/,13	; message for trying to use an already linked disc
headlnk	fcn	/UNIT LINK/,13							; header for LLINK
spaces	fcn	/    /									; text for detail line in LLINK
ldtext   fcn   /LOADING BOOT TRACK/					; message for OS9boot
noostxt  fcn   $0d, /NOT AN OS BOOT TRACK/		; errro message for os9boot
xidxTxt	fcn	/ #/										; text that precedes HD index number			
Str2Long	fcn	/STRING TOO LONG/,13					; message for param string too long
NotAnMPI fcn	/NO MPI PRESENT/,13					; message for no NPI and  ... next message too
NotAnSDC	fcn	/NO SDC PRESENT/,13					; message for no SDC present
NotActSDC
			fcn	/NO ACTIVE SDC/,13					; mmesage for not selected SDC
BadFmtType
			fcn	/FORMAT VALUE NOT VALID/,13 		; message for parameter invalid
idxHigh	fcn	/INDEX TOO HIGH/,13					; index higher than max allowed
noCurDir	fcn   /CURRENT DIR IS ROOT/,13			; message to user
canNot	fcn	/NOT POSSIBLE/,13						; message for switching not possible
sdcNoDsc	fcn	/:--NO DISC--/,13						; message for no disk in sdc drive
totSect	fcn	/:TOTAL SECTORS: /					; text for detail in SDRIVE
datSDF	fcn	/SDF /									; text for SDRIVE
datDIR	fcn	/DIR /									; text for SDRIVE
datNUL	fcn	/    /									; text for SDRIVE
msgCont	fcn	/  cONT/									; message to user CONTinue
msgExit	fcn	/  eXIT/									; message to user EXIT
wrongPar	fcn	/INVALID PARAMETER VALUE/,13		; message
stackPrb	fcn	/STACK CONFLICTS/,13					; error message	
timeout 	fcn 	/TIMEOUT/,13							; error message for SDC access
targetInUse
			fcn 	/TARGET IN USE/,13					; error message for SDC
dirNotFound 		
			fcn 	/DIRECTORY NOT FOUND/,13			; error message for SDC
pathNameInvalid 	
			fcn 	/INVALID PATHNAME/,13				; error message for SDC
miscHardware 		
			fcn 	/MISC. HARDWARE ERROR/,13			; error message for SDC
unknown 	fcn 	/UNKNOWN ERROR/,13					; error message for SDC
targetNotFound 	
			fcn 	/TARGET NOT FOUND/,13				; error message for SDC
not64K	fcn	/NO 64K COMPUTER/,13					; error message for SDC

; -------------------------------------------------------------------------------------------------------------------------------
; install new routines
; Creates a new USR table and fills it with calls to 'FC ERROR'
; usess the old USR table space to create a new STUB for the
; new commands that will be added after the D.O.S. ones
; -------------------------------------------------------------------------------------------------------------------------------
initext	orcc	#dimask						; disable interrupts
			leax  newusr,pcr        		; new address table for usr's
       	stx   usrptr            		; save at memory variable
       	ldy   #fcerr            		; address for error message
       	lda   #10               		; number of usr's
nxtvec 	sty   ,X++              		; fill element table
       	deca                    		; decrement counter
       	bne   nxtvec            		; not finished, go back
       	lda   #tokens           		; get number of new tokens
       	sta   newstb            		; store at stub base byte
       	leax  newrds,pcr        		; base of new reserved words table
       	stx   newstb+1          		; save on stub
       	leax  newdsp,pcr        		; address of dispatching routine
       	stx   newstb+3          		; save on stub
			ldx	#psmsg-1						; point to message string
			jsr	outstr						; show to screen
			lda	MPIcfg						; get received MPI config value for mega-mini-MPI compatibility
			anda	#$33							; avoid using bits 7-6-3-2
			sta	cpyMPIcfg					; initialize copy
			sta	MPIcfg						; update config byte
			bsr	initask						; put initial values
			andcc	#eimask						; enable interrupts
			jmp	expert						; goto auto-start experiments

; -------------------------------------------------------------------------------------------------------------------------------
initask	clr	<retry						; NO retry mark
			ldd	#$0102						; default values for the first two physical drives
			ldx	#$fc							; point to config area
			std	,x++							; save these values
			ldd	#$f6f5						; default values for Units 3-4 pointing to DW4 slots (245-246)
			std	,x								; save these values
			ldd	<$8a							; get 16 bits zero
			std	floSDC						; clear $114 and $115
												; pre-test on MPI			
			lbsr	tstMPI						; is there an MPI?
			bne	initsk01						; no, go to individual checks
			lbsr	tstHARD						; yes, verify connected hardware
			rts									; return
												; else verify individual elements (only one active)
initsk01	lbsr	tstSDC						; is SDC active?
			beq	fndSDC						; yes, skip next test
			lbsr	tstFLO						; is FDC active instead?
			bne	fndNthg						; no, go out, no harware present at all
fndFDC	lda	#%00000100					; set FDC present and let both SDC drives OFF
			bra	savHdwF						; go save it
fndSDC	lda	#%00001011					; mark SDC present and put both SDC drives ON
savHdwF	sta	floSDC						; save result
fndNthg	rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Command dispatcher called from BASIC
; decodes the received token and passes it with the address
; of the new dispatch table to the ROM command execution
; -------------------------------------------------------------------------------------------------------------------------------
newdsp 	cmpa  #newtok           		; compare to first defined token
       	blo   lowerr            		; if lower show error
       	cmpa  #newtok+tokens    		; compare to last one
       	bhs   higher            		; if higher show error
       	suba  #newtok           		; first token converts to 0
       	leax  nwtb01,pcr        		; get new commands dispatch table base address
       	jmp   romcmd            		; pass to rom command execution
lowerr 	leax  low,pcr           		; point to message
       	bra   new1							; show error and return to interpreter
higher 	leax  high,pcr          		; point to message
new1   	jsr   outstr             		; show the selected error
       	jmp   synerr            		; rom will show syntax error message

; -------------------------------------------------------------------------------------------------------------------------------
; new reserved words
; -------------------------------------------------------------------------------------------------------------------------------
newrds 	fcc   /DWLOA/,$C4       		; char 'D' OR $80		dwload	= DLOAD (the new into this ROM)
       	fcc   /MOUN/,$D4        		; char 'T' OR $80		mounT		= Associate a VDK to a Drive Number
       	fcc	/LIN/,$CB					; char 'K' or $80		linK		= fill unit cell with received data
       	fcc	/LLIN/,$CB					; char 'K' or $80		llinK		= show units links
       	fcc	/DO/,$D3						; char 'S' or $80		doS		= Boot a NitrOS-9 disc
       	fcc	/LRO/,$CD               ; char 'M' or $80		lroM		= Load or Run a ROM image
       	fcc	/BAN/,$CB					; char 'K' or $80		banK		= change active SDC-bank
       	fcc	/SLO/,$D4					; char 'T' OR $80		sloT		= change to another MPI slot ($ef)
       	fcc	/SDRIV/,$C5					; char 'E' or $80		sdrivE	= multipurpose for SDC-card
       	fcc	/SDI/,$D2					; char 'R' or $80		sdiR		= multipurpose for SDC-card
       	fcc   /HDID/,$D8					; char 'X' or $80		hdidX		= select the index disc in an HD array (max 90)
       	fcc	/SMKDI/,$D2					; char 'R' or $80		smkdiR	= create a directory in the SDC-card
		   fcc	/SKIL/,$CC					; char 'L' or $80		skilL		= kill a file or an empty directory in a SDC-card
			fcc	/SRE/,$CE					; char 'N' or $80		sreN		= rename a file in a SDC-card
			fcc	/UNLOA/,$C4       		; char 'D' OR $80		unloaD	= token to be used just as parameter ($f6)
			fcc	/SCH/,$C4       			; char 'D' OR $80		schD		= to change ACTUAL Directory
			fcc	/WRIT/,$C5					; char 'E' or $80		writE		= write to a flash bank
			fcc	/SCOP/,$D9					; char 'Y' or $80    scopY    = copy ROM from bank or slot to RAM

; -------------------------------------------------------------------------------------------------------------------------------
; Dispatch table. This is an indirect table
; it contains the start address of every new command 
; -------------------------------------------------------------------------------------------------------------------------------
nwtb01 	fdb   dwload						; routine for DWLOAD command
		 	fdb   mount 						; routine for MOUNT command                
		 	fdb	link							; routine for LINK command
		 	fdb	llink							; routine for LLINK command
		 	fdb	os9boot						; routine to boot NitrOS-9
		 	fdb	xlrom							; routine to load - run a ROM image
		 	fdb	xbank							; routine to switch SDC active bank
		 	fdb	xslot							; routine to change of MPI slot
		 	fdb	xsdrive						; routine for SDC-card DRIVE functions
		 	fdb	xsdir							; routine for SDC-card DIR functions
		 	fdb	xHDidx						; routine to select index of HD
		 	fdb	xsmkdir						; routine for SMKDIR command
		 	fdb	xskill						; routine for SDELETE command
		 	fdb	xsren							; routine for SRENAME command
		 	fdb	snError						; simply show syntax error
		 	fdb	xschd							; routine for SCHD command
		 	fdb	xwrite						; routine for WRITE command
		 	fdb	xscopy						; routine for SCOPY command

; -------------------------------------------------------------------------------------------------------------------------------
; Dispatch routines for the new added commands
; -------------------------------------------------------------------------------------------------------------------------------
; Routine to mark as invalid the four I/O buffers
; -------------------------------------------------------------------------------------------------------------------------------
mrkmod	clra									; get a 0
			ldx	#$636							; point to 1st I/O buffer flags data
			ldb	#21							; offset to last I/O
mo01		sta	b,x							; mark as invalid
			subb	#7								; offset to previous I/O
			bpl	mo01							; if not negative, loopback
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Command to ask DW4 to mount a VDK file and get the slot number back
; -------------------------------------------------------------------------------------------------------------------------------
mount		lbsr	readunit						; read unit number
			stb	<numUni						; save it as number of Unit
			jsr	ckComma						; CkComma
			jsr	getfnam						; GetNam into $1d1 (nambuf)
       	ldx   #nambuf-1					; byte before packet buffer start
       	clr   ,x								; zero MSB to make a 16 bit value for Y
       	ldy   ,x++							; length of file name (16 bit)
       	beq   failvdk						; if len=0 no file entered
       	ldb   -1,x							; length of name (8 bit)
												; Adding .VDK if not termination entered, costs 31 bytes       
		 	pshs  b,x							; save pointer to beginning of name and name length
isdot  	lda   ,x+							; get a char from name
       	cmpa  #'.'							; is it a dot?
       	beq   alrdot						; yes, stop search, quit loop
       	decb									; decrement counter
       	bne   isdot							; not end of string, get next char name
       	ldd   #$2e56						; get chars ".V"
       	std   ,x++							; add to end of name
       	ldd   #$444b						; get chars "DK"
       	std   ,x++							; add to end of name
		 	ldb   ,s								; get old name length
		 	addb  #4								; add 4 (because of .VDK added)
		 	stb   ,s								; update new length in stack
       	stb   nambuf						; save new length in byte before string
       	leay  4,y							; correction to name length - DW parameter
alrdot 	pshs	a								; save register
			lda	#2								; get value to mark output
			sta	BBOUT							; to DW4
			clra									; set counter
mount01	deca									; decrement counter
			bne	mount01						; not zero, loopback
			puls	a								; restore register
			puls  b,x       					; restore pointer and length
		 	clr   b,x							; zero terminate name string (for error)
       	inc   ,--x							; 1 = DriveWire OP_NAMEOBJ_MOUNT command
       	leay  2,y							; length of DW packet = name length + 2
       	orcc	#dimask						; disable interrupts to work with DW4
       	jsr   DWWrite						; send file request to server
       	ldx   #dnum							; our drive number variable
       	clr   ,x								; clear it
       	leay  1,y							; read one byte back
       	jsr   DWRead						; get drive number
		 	andcc #eimask						; enable interrupts again 
       	tst   ,x								; verify received value
       	bne   vdkok							; successful mount                                       
       	ldb   #$19							; code for MO ERROR
		 	jmp   syserr						; show error message (syserr)
vdkok		ldb	<dnum							; pass slot to B
			bsr	stval							; verify / store new value
			rts									; return
failvdk	jmp   ioerror						; I/O error message

; -------------------------------------------------------------------------------------------------------------------------------
; Command to link a Unit with a drive number or a DW4 slot number
; -------------------------------------------------------------------------------------------------------------------------------
link		bsr	readunit						; read unit number
			stb	<numUni						; save it as number of Unit
			bsr	opt8bit						; search another param 8 bit number
			bne	link01						; if greater than 0, goto more tests
												; requested slot #0
			ldb	<numUni						; get unit number
			cmpb	#1								; is it 1?
			bne	notalw						; no, error. Only unit 1 is allowed to be linked to slot #0
			clr	<$fc							; put 0 value in first cell (unit 1, value 0)
			bra	link03						; exit
notalw	ldb	#$08							; code for FC error
			bra	basyserr						; show error message
link01	cmpb	#4								; is new value greater than 4 (DW4)?
			bhi	link02						; yes, skip test
			cmpb	<numUni						; equals new drive number the unit number? (mandatory!)
			bne	numError						; no, show unit error
link02	bsr	stval							; verify and store new value
link03	rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Verifies that the new value (B) is not linked to another unit. If good, saves it
; -------------------------------------------------------------------------------------------------------------------------------
stval		ldx	#$00fb						; point one byte before drive/slot table
			lda	#4								; counter (number of units)
stv02		cmpb	a,x							; equals new value the contents of that slot?
			beq	stv04							; yes, see if it is the same unit
			deca									; decrement counter
			bne	stv02							; if not 0, loopback
estval	ldx	#$00fb						; point to one byte before data table
			lda	<numUni						; get unit number
			cmpa	#4								; is unit 4?
			bne	stv03							; no, skip next
			clr	idxHD							; set index to HD to 0
stv03		stb	a,x							; store slot into table
			lbsr	mrkmod						; modify data to flag drive changes to system	
			rts									; everything alright, return
stv04		cmpa	<numUni						; is unit with this value the same as destination in LINK command?
			beq	estval						; yes, acceptable, return
			leas	2,s							; get rid of return address
													; show error. We do not admit the same slot number in two units!
;			bra	already					; *** Fall Thru *** not needed 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
already	ldx	#alrput-1					; point to error message
			fcb	#$10							; to jump over next one
numError ldx	#drverr-1					; point to error message
			jmp	outstr						; show message and return to interpreter

; -------------------------------------------------------------------------------------------------------------------------------
readunit	clr	<numUni						; clear unit number
			bsr	getunit						; get a one digit number
			beq	errNum						; if zero jump to device error
			cmpb	#4								; is it greater than 4?
			bls	OKnext						; no, goto OK
errNum	ldb	#$28							; device number error (bas_err_DN)
basyserr	jmp	syserr						; bas_system_error
OKnext	jmp	<redLChr						; bas_poll_char			

; -------------------------------------------------------------------------------------------------------------------------------
getunit	jsr	<redLChr						; get a char (bas_poll_char)
			bne	r8b01							; if there is something, skip next
snError	jmp	synerr						; show error message (BAS-SN-Error)
r8b01		jsr	get8bit						; get a number (8bits)
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
opt8bit	jsr	<redLChr						; get a char (bas_poll_char)
			beq	eo8b							; if 0, return
			jsr	ckComma						; check if there is a comma separator (CkComa)
			bsr	get8bit						; get a 8 bits number
eo8b		rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
get8bit	pshs	y,u							; save working registers
			jsr	asc2Num						; convert ASCII to number (1 byte)
			tstb									; update flags upon result
			puls	y,u,pc						; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Command to show the links of every unit
; -------------------------------------------------------------------------------------------------------------------------------
llink		ldx	#headlnk-1					; point to message header
			jsr	outstr						; show it
			ldx	#$00fb						; point one byte before first data byte
			ldb	#1								; unit counter
llink01	lda	b,x							; get linked value
			bsr	showLine						; show data
			incb									; increment counter
			cmpb	#4								; done all units?
			bls	llink01						; no, loopback
			rts									; return to caller

; -------------------------------------------------------------------------------------------------------------------------------
showLine	pshs	d,x							; save working registers
			ldx	#spaces+1					; point to print 2 spaces
			jsr	outstr						; print them
			ldb	1,s							; get unit number
			clra									; clear high byte
			jsr	outnum						; print number
			ldb	,s								; get linked value
			clra									; clear high byte
			cmpb	#10							; is value lower than 10?
			blo	sL01							; yes, goto print
			inca									; one space less
			cmpb	#100							; is value lower than 100?
			blo	sL01							; yes, goto print
			inca									; one space less
sL01		ldx	#spaces-1					; point to print 4 spaces
			leax	a,x							; skip the calculated ones upon the value
			jsr	outstr						; print required spaces
			ldb	,s								; get linked value
			clra									; clear high byte							
			jsr	outnum						; print number
			ldb	1,s							; get unit number
			cmpb	#4								; is it 4?
			bne	sL02							; no, stop printing
			ldb	idxHD							; get index for HD
			beq	sL02							; if zero, skip printing it
			ldx	#xidxTxt-1					; point to text
			jsr	outstr						; show it
			ldb	idxHD							; get unit number
			clra									; zero to high byte
			jsr	outnum						; call ROM to print integer in D
sL02		lda	#$0d							; enter
			jsr	outRegA						; print A 
			puls	d,x,pc						; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Command to Boot a NitrOS-9 disc
; -------------------------------------------------------------------------------------------------------------------------------
os9boot  clrb									; default drive 0
         jsr   <redLChr             	; peek at next char
         beq   defnum						; not found, use default
         jsr   asc2Num          			; Get 1 byte
defnum 	stb   <dnum							; save drive number
	 		ldx   #ldtext-1					; point to 'loading' message
         jsr   outstr						; show it
	 		orcc  #dimask						; disable interrupts
			jsr	trick							; make sure DW4 connection is on
													; DriveWire bootstrap code. Get sectors 612-629 to $2600
	      ldx   #612							; starting sector number to load
         ldy   #$2600						; destination in memory
DOSLoop  jsr   DoRead						; read sector (into Y)
         bcs   fail2							; start all over
         bne   fail2							; show error
         cmpx  #612							; our first sector?
         bne   not1st						; branch if not
         ldd   ,y								; else get 1st two bytes at $2600
         cmpd  #$4f53						; OS?
         bne   noos							; no, show error
not1st   lda   #'*'+$40						; print star
         leay  256,y							; move for next sector
         leax  1,x							; increment number of sector to be read
         sta   571,x							; print progress (1024+32*5-612-1)
         cmpx  #630							; are we at last sector?
         blt   DOSLoop						; branch if not
         lda   #'!'+$40						; print bang
         sta   635,x							; over last '*' (636-1)
         jmp   $2602							; jump into Boot code
noos     ldx   #noostxt-1					; point to message
         jsr   outstr						; print string at x+1
fail2    jmp   ioerror						; IO_error equ $b84b

; -------------------------------------------------------------------------------------------------------------------------------
; Patch to DosPlus Boot to allow RSDOS disks to boot too (Mike Miller 2021-05)
; -------------------------------------------------------------------------------------------------------------------------------
rsbtchk  cmpx	#$4F53						; Check for signature
		 	beq	rsbt01  						; Boot DragonDOS style disk
		 	ldd	#$2201  						; Look instead RSDOS location track 34 first sector
		 	jsr	gtbtsig					   ; Read boot sig (enter after track and sector set)
		 	bcc	rsbt00						; it's OK, skip next
;		 	jmp	$C166   						; Failed to read then exit
		 	rts		   						; Failed to read then exit
rsbt00 	ldx	$4F     						; Get first 2 bytes 
		 	cmpx	#$4F53   					; Check for signature 
			beq	rsbt01						; it is Bootable!
		 	jmp	$C164   					   ; Not bootable RSDOS way either, exit with ?BT error	
rsbt01 	jmp	$C14E   						; Otherwise load the track

; -------------------------------------------------------------------------------------------------------------------------------
; Routine that marks on the DW4 line for a while, to avoid fails at 1st access
; -------------------------------------------------------------------------------------------------------------------------------
trick    pshs	b,x							; save working registers
			ldx   #PIA1Base      			; address of PIA1
         ldb   #$02							; put value #2 (bit1 to high level)
         stb   ,x             			; make RS-232 output marking
													; Spin for a while so that the RS-232 bit stays hi for a time
waw		ldx   #$a000						; wait counter
waw1     leax  -1,x							; decrement counter
         bne   waw1							; if not 0, loopback
         puls	b,x,pc						; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Command to load - run a ROM image
; -------------------------------------------------------------------------------------------------------------------------------
xlrom   	orcc  #dimask	            	; disable firq, irq
			bsr	trick							; make sure dw4 connection is on
lromrty 	jsr   <redLChr						; peek at next character (after "rom")
       	suba  #'N'							; to detect if it was an 'n' - if so don't exec it after loading
       	sta   <noexec						; save difference. 0 = noexec
       	bne   savvec						; if not 'n' jump next instruccion
       	jsr   <getNChr						; jump over that char (the 'n')
savvec 	ldx   #ldtextr-1					; byte before message
		 	bsr   prtstr						; print string
	   	jsr   getfnam						; read rom file name and length into $1d1 = nambuf
       	tst	nambuf						; is filename length equal 0?
       	bne	rom01							; no, process it
       	ldb	$60a							; get default drive number
       	ldx	#$00fb						; point to table of links x unit
       	lda	b,x							; search slot number in table
       	bne	ver01							; if not 0, do more tests
       	cmpb	#1								; is slot 0 linked to unit 1? - to avoid tricks with poke unit's slot numbers
       	beq	ver02							; yes, accept it and go on. the only legal one!
ver01   	cmpa	#5								; is it a dw4 slot?
       	blo	errdw4						; no, show error
ver02   	sta	<dnum							; save slot number at variable
       	bra 	rdecb							; go process rom at default drive
errdw4	ldx	#notdw4-1					; point to message error
		 	jsr   outstr						; show message
			rts									; return
rom01   	ldx   #nambuf-1					; byte before packet buffer start
       	clr   ,x								; zero msb to make a 16bit value for y
       	ldy   ,x++							; length of file name (16 bit)
       	beq   failedr						; if len=0 no file entered
       	ldb   -1,x							; length of name (8 bit)
													; adding .rom if not termination entered      
		 	pshs  b,x							; save pointer to beginning of name and name length
isdotr  	lda   ,x+							; get a char from name
       	cmpa  #'.'							; is it a dot?
       	beq   alrdotr						; yes, stop search, quit loop
       	decb									; decrement counter
       	bne   isdotr						; not end of string, get next char name
       	ldd   #$2e52						; get chars ".r"
       	std   ,X++							; add to end of name
       	ldd   #$4f4d						; get chars "om"
       	std   ,x++							; add to end of name
		 	ldb   ,s								; get old name length
		 	addb  #4								; add 4 (because of .rom added)
		 	stb   ,s								; update new length in stack
       	stb   nambuf						; save new length in byte before string
       	leay  4,y                    	; correction to name length - dw parameter
alrdotr 	puls  b,x       					; restore pointer and length
       	clr   b,x							; zero terminate name string (for error)
       	inc   ,--x							; 1 = drivewire op_nameobj_mount command
       	leay  2,y							; length of dw packet = name length + 2
       	jsr   DWWrite						; send file request to server
       	ldx   #dnum							; our drive number variable
       	clr   ,x								; clear it
       	leay  1,y							; read one byte back
       	jsr   DWRead						; get drive number
       	tst   ,x								; verify received value
       	bne   rdecb							; successful mount. if z has not been set = error (coundn't mount). jump to readin
moError 	ldb   #$19							; errror code for mo error
       	andcc #eimask       				; enable interrupts
       	jmp   syserr						; show error message
failedr 	andcc #eimask       				; enable interrupts
		 	jmp   ioerror						; i/o errror message
prtstr 	ldb	<curlow 						; get low byte of cursor position
       	andb  #$1f							; is column zero?
       	bne   shwmsg   					; no, print whith leading enter
		 	leax  1,x      					; jump leading enter 
shwmsg 	jsr   outstr					   ; print string
		 	rts									; return
rdecb  	ldx   #0								; begin at sector zero
       	ldy   #ramini						; destination address
       	stx   <endadr						; set last load address to zero 
       	clr   <bigfil						; flag for long file to false
romlop 	lbsr  doreadr  					; read sector (into y) 
       	bcs   failedr						; detect frame error
       	bne   failedr						; detect incomplete read
       	blt   nomore                 	; detect end of file. finish
       	lda   #'*'							; get 'star' ascii code
		 	jsr   outchr						; show on screen, we have read a sector
       	leay  256,y							; point to next sector address
       	cmpy  #rmaxld						; got to the ram limit?
       	bhs   bigend						; no, keep on copying
       	leax  1,x							; increment sector number
       	bra   romlop						; go read next sector
bigend 	inc   <bigfil						; big file flag to true
nomore 	sty   <endadr						; save as last loaded address
       	lda   #$08							; get backspace ascii code
		 	jsr	 outchr						; print on screen (go back one char)
       	lda   #'!'							; get 'bang' ascii code
		 	jsr   outchr						; put it on screen
		 	tst   <bigfil						; is this a too long file?
		 	beq   final							; no, jump message
		 	ldx   #bigrom-1					; message for rom greater than 16k-256bytes
		 	bsr   prtstr						; print message
final  	tst   <noexec						; has to be run?
       	bne   runit							; yes, do it
       	ldx   #endtxt-1					; point to end text
		 	bsr   prtstr						; print message
		 	ldd   <endadr						; get last loaded ram address
		 	subd  #ramini						; subtract init ram address
		 	jsr   outnum		 				; print length 
	    	andcc #eimask						; enable interrupts
       	rts									; return to the caller
runit  	ldd   >romini						; get two bytes from rom area
		 	ldx   #$9669						; create a pattern 1001 0110 0110 1001
		 	stx   >romini						; store into two dos-rom bytes
		 	cmpx  >romini						; have been modified?
		 	bne   basicup						; no, must switch to map1 and copy basic to upper ram
		 	std   >romini						; put back two saved bytes
basicup  ldx	#basic						; point to beginning of code to be moved
			ldu	#$2a00						; destination in low ram
basup01	ldd	,x++							; get a word
			std	,u++							; put at destination
			cmpx	#doreadr						; got to the end?
			bls	basup01						; no, loopback
			jmp	$2a00							; transfer control to copied code in ram

; -------------------------------------------------------------------------------------------------------------------------------
basic  	ldx   #basini						; get basic initial address
bas1   	sta   $ffde							; go map0 (rom)
       	ldd   ,x								; get 2 bytes
       	sta   $ffdf							; go map1 (ram)
       	std   ,x++							; store 2 bytes, incr. pointer
       	cmpx  #romini						; reached end of basic?
       	blo   bas1							; no, keep on copying basic
map1	 	ldx	#ramini						; get rom origin address at ram area
       	ldu   #romini						; get rom destination address
       	sta   $ffdf							; go map1 (ram) and stay there
move   	ldd   ,x++							; get two bytes, inc pointer
       	std   ,u++							; store two bytes, inc pointer
       	cmpx  <endadr 						; reached last loaded address?
		 	bhs   nohook						; yes, quit loop
		 	cmpx  #rammax						; reached max rom possible (16k-256bytes)?
		 	blo	 move							; no, keep on copying rom
nohook 	lda   #$39							; opcode for rts
		 	ldx   #hokini						; get initial address of system hooks
hooks  	sta   ,x+							; overwrite them
       	cmpx  #hokend						; got hooks end?
       	blo   hooks							; no, keep on overwriting them
			ldd	#$444b						; sign for dos roms
			ldx	#romini						; beginning of the rom
			cmpd	,x								; is a dos what we have loaded?
			bne	bas2							; no, execute it
			leax	2,x							; skip over dos signature
bas2	 	jmp   ,x								; start loaded rom

; -------------------------------------------------------------------------------------------
doreadr 	lda   <dnum							; our drive number
       	clrb									; lsn bits 23-16
       	pshs  a,b,x,y						; save registers / data
       	lda   #$d2							; dw op_readex (read one sector)
rereadr 	pshs  a								; save command to stack
       	leax  ,s								; point x to top of stack
       	ldy   #5								; 5 bytes
       	jsr   DWWrite						; send 5 bytes to dw	  - readex, dnum, $0, sector
       	puls  a								; discard command
       	ldx   4,s							; get read buffer pointer (old y)
       	ldy   #256							; read 256 bytes
       	ldd   #133							; 1 second timeout
       	jsr   DWRead						; receive 256 bytes
       	bcs   readex2						; detect framing error
       	bne   readex2						; detect not all bytes received
       	pshs  y								; save checksum
       	leax  ,s								; point x to top of stack
       	ldy   #2								; 2 bytes
       	jsr   DWWrite						; send checksum
       	leay  1,y							; after a dwrite 'y' returns zeroed, so this way it becomes equal to one
       	ldd   #133							; 1 second timeout
       	jsr   DWRead						; read dw answer
       	leas  2,s							; discard checksum
       	bcs   readex2						; detect framing error
       	bne   readex2						; detect not all bytes received
       	lda   ,s								; get server answer
       	beq   readex2						; zero=ok, return
       	cmpa  #$f3							; error code is #e_crc?
       	bne   reader						; not this one, go verify eof condition
       	lda   #$f2							; command reread. #op_rereadex
       	clr   ,s								; clear received answer
       	bra   rereadr						; read again
reader 	cmpa  #$f4          				; error code is 'read past end' (eof)?
		 	bne   noteof						; no, mark read error
       	andcc #$fc                    ; unsets the 'c' and 'v' flags
       	orcc  #$0c							; sets 'n' and 'z' flags. both prepare the flags to be positive at 'blt' test
       	bra   readex2						; jump over next instruction, not a read error
noteof 	comb									; sets the carry flag (error)
readex2 	puls  a,b,x,y,pc					; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Command to start a SDC-bank
; -------------------------------------------------------------------------------------------------------------------------------
xbank		jsr	getunit						; read a number between 0-7
			cmpb	#7								; is it greater than 7?
			bls	OKbank						; no, OK
badnum	ldb	#$11							; SD error
			jmp	syserr						; bas_system_error
OKbank	orcc	#dimask						; disable interrupts
			stb	<numUni						; save received number in first sentence (B)
			clr	swMap1						; by default don't change map type
			jsr	<redLChr						; see next char after BANK
			beq	seeSlot						; no extra chars, skip extra param search
			cmpa	#','							; is it a comma?
			lbne	snError						; no, show error
			jsr	<getNChr						; get next char
			cmpa	#'R'							; is it R (for RAM mode = MAP1)?
			lbne	snError						; no, show error
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; copy and execute from RAM the 64k tester
			ldx	#tst64k						; point to beginning of data to be copied
			ldu	#$1da							; point to destination
xb001		ldd	,x++							; get a wrod
			std	,u++							; put at destination
			cmpx	#emod1B+2					; got to the end?
			blo	xb001							; no, loopback
			jsr	$1da							; jsr that copied code from RAM
			bne	xb002							; if not zero, we have 64K
		   ldx 	#not64K-1					; else show message NO 64K COMPUTER			
			jmp	outstr						; show message

; -------------------------------------------------------------------------------------------------------------------------------
												; validate if there are 64k - This must be executed from RAM
tst64k	bsr	mod1Byt						; can modify rom area?
			bne	etst64k						; yes, return result
			sta	$ffdf							; no, try switching to map1			
			bsr	mod1Byt						; can modify area?
			stb	$ffde							; back to map0 where it was
etst64k	rts									; return result
mod1Byt	clrb									; default 32K
			lda	$c000							; get first DOS ROM char 
			coma									; invert bits
			sta	$c000							; update value
			cmpa	$c000							; has been updated?
			bne	emod1B						; no, return
			coma									; invert bits again
			sta	$c000							; restore value
			incb									; found 64k
emod1B	tstb									; update flags
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
xb002		inc	swMap1						; mark flag to change to map1
												; see if we have a SDC
seeSlot	lda	floSDC						; get hdw control byte
			bita	#%00001000					; is there an SDC?
			lbeq	NotSDCPresent				; no, show error
			anda	#%11000000					; use bits 7-6 (slot where it is)
			lsra									; move them to
			lsra									; bits 5-4 (lower bits of high nibble)
			pshs	a								; save high nibble
			lsra									; low nibble   
			lsra									; gets high nibble
			lsra									; that will end  
			lsra									; as zero
			ora	,s+							; add high nibble (now they are the same value)
			tst	swMap1						; should it be run in MAP1?
			bne	swMbank						; yes, go to next section
swbank	pshs	a								; save SDC slot number
			ldx	#special						; point to the beginning of code to execute from RAM
			ldu	#$1da							; point at destination
swB01		ldd	,x++							; get a word
			std	,u++							; copy at destination
			cmpx	#exbank+3					; got to the end?
			blo	swB01							; no, loopback
			puls	a								; restore slot number
			jmp	$1da							; jumto to RAM copied code
swMbank	pshs	a								; save SDC slot number
			ldx	#allRAM						; point to the beginning of code to execute from RAM
			ldu	#$1da							; point at destination
swMB01	ldd	,x++							; get a word
			std	,u++							; copy at destination
			cmpx	#m1L03+3						; got to the end?
			blo	swMB01						; no, loopback
			puls	a								; restore slot number
			jmp	$1da							; jumto to RAM copied code
												; this TWO parts MUST be run from RAM because we are going
												; to lose control when switching the ROM slot and/or the bank!
												; code to start the bank in ROM mode (MAP0)
												; SDC slot# comes onto stack 
special	orcc	#dimask						; disable interrupts
			clr	$ffde							; switch to MAP0 (ROM-RAM mode)
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; switch to the slot where the SDC is
			ldb	<numUni						; get requested bank number
			stb	$ff4b							; point to it
			ldx	#$444b						; get mark for DOS system
			cmpx	romini						; is there a DOS ROM?
			beq	spec01						; yes, issue cold start
			ldx	#$7f37						; get std value for stack
			leas	,x								; assign it
			andb	#eimask						; enable interrupts
			jmp	romini						; no, execute ROMPACK
spec01	clr	<$71							; force a cold boot
exbank	jmp	resetRt						; execute the system reset routine
												; code to start the bank but having copied everything into MAP1
												; SDC slot# comes onto stack 
allRAM	orcc  #dimask         			; disable interrupts
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; switch to the slot where the SDC is
			ldb	<numUni						; get requested bank number
			stb	$ff4b							; point to it
												; copy ROM to RAM
			ldx   #basini          			; point to beginning of Basic
m1L01		sta   $ffde           			; switch to ROM map
			ldd   ,x              			; get a word
			sta   $ffdf           			; switch to RAM map
			std   ,x++            			; copy at RAM destination
			cmpx  #$feff          			; end of ROM?
			bcs   m1L01 		   			; no, loopback
			lda   #$39            			; get RTS opcode
			ldx   #$015e          			; point to beginning of User vectors (hooks)
m1L02		sta   ,x+             			; put an RTS in each one
			cmpx  #$01a8          			; end of user vectors?
			bcs   m1L02			   			; no, loopback
			jsr	$ba77							; clear screen
			ldd	#$444b						; get mark for O.S.
			cmpd	romini						; is there an O.S. in RAM now?
			beq	m1L03							; yes, start it
			jmp	romini						; else start ROM from the beginning
m1L03		jmp   romini+2         			; start ROM (copied into RAM)

; -------------------------------------------------------------------------------------------------------------------------------
													; MESSAGES. They all will return with carry set.
BadFormat
			ldx	#BadFmtType-1				; point to message
			fcb	#$10							; skip next ldx
TooLong	ldx	#Str2Long-1					; point to message
			fcb	#$10							; skip next ldx
NotMPIPresent
			ldx	#NotAnMPI-1					; point to message
			fcb	#$10							; skip next ldx
NotSDCPresent
			ldx	#NotAnSDC-1					; point to message
			jsr	outstr						; show it
			orcc	#%00000001					; set carry flag
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; This routine will copy the hardware tests routines at $1da and jump there 
; to avoid hanging the system while switching of MPI slot
; -------------------------------------------------------------------------------------------------------------------------------
tstHARD	ldx	#tstHDW						; point to beginning of program to be copied
			ldu	#$1da							; point to destination
tstL01	ldd	,x++							; get a word
			std	,u++							; put at destination
			cmpx	#xslot						; got to end of program?
			blo	tstL01						; no, loopback
			jsr	$1da							; execute copied program in low RAM
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; test for the presence of MPI, floppies, SDC and update $114 accordingly
; -------------------------------------------------------------------------------------------------------------------------------
tstHDW	clrb									; get zero value
			stb	floSDC						; control byte default value (no hdw found)
			lbsr	tstMPI						; is there an MPI present?
			lbne	etstHDW						; no, exit
			stb	hdwFlo						; no Floppies found
			stb	hdwSdc						; no SDC found
												; search for SDC and/or floppies
			lda	cpyMPIcfg					; get active MPI slot
			ldb	#4								; ready to check four slots
												; first look for SDC
tst01		tst	hdwSdc						; have we found it before?
			bne	tst02							; yes, go to floppy test
			lbsr	tstSDC						; is there an SDC in this MPI slot?		
			bne	tst02							; no, go to floppy test
			sta	hdwSdc						; save slot where SDC has been found
			lda	floSDC						; get hdw control byte
			ora	#%00001000					; add flag for SDC present
			sta	floSDC						; update it
			bra	tst03							; go process another slot
												; if not SDC, then for floppy
tst02		tst	hdwFlo						; have we found it before?
			bne	tst03							; go process another slot
			lbsr	tstFLO						; is there a floppy controller in this MPI slot?
			bne	tst03							; no, process another slot
			sta	hdwFlo						; save slot where floppy has been found
			lda	floSDC						; get hdw control byte
			ora	#%00000100					; add flag for Floppies present
			sta	floSDC						; update it
												; look at another slot
tst03		lda	cpyMPIcfg					; get analized slot		 																---	A = $33/$22/$11/$00
			suba	#$11							; calculate previous slot																---   A = $22/$11/$00/$EF
			bpl	tst04 						; if not negative, skip next
			lda	#$33							; get upper value																			---   A = $33
tst04		pshs	a								; save new calculated slot number												  	---   pushed $22/$11/$00/$33
			lda	cpyMPIcfg					; get last tested slot value															--- 	   A = $33/$22/$11/$00
			anda	#$cc							; clean bits 0-1 in both nibbles														--- 	   A = $00/$00/$00/$00
			ora	,s+							; add new calculated slot number ($00-$11-$22-$33)								---  	   A = $22/$11/$00/$33
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; change of I/O slot (not keeping the same ROM slot)							---  MPIcfg = $22/$11/$00/$33
			decb									; decrement slot counter																---      B =   3/  2/  1/  0
			bne	tst01							; if not zero, loopback
												; if zero: end of tests. MPIcfg must be restored to original value
tst05		lda	floSDC						; get hdw control byte
			bita	#%00001000					; is bit3 ON (SDC present)?
			beq	tst07							; no, look for floppies 
			lda	hdwSdc						; get SDC slot																				--- A = $00
			cmpa	cpyMPIcfg					; equals copy of MPIcfg? 
			bne	tst06							; no, don't modify floSDC
			ldb	floSDC						; get hdw control byte											
			orb	#%00000011					; set floSDC.bits1-0 to #%11 (SDC ON)
			stb	floSDC						; update control byte
tst06		anda	#%00000011					; use just slot number																	--- A = $00
			clrb									; result = 0																				--- B = 0
			lsra									; send bit 0 to carry																	--- A = 0, C = 0
			rorb									; receive bit0 from carry in bit7													--- B = 0
			lsra									; send bit 1 (now 0) to carry															--- A = 0, C = 0
			rorb									; receive bit1 (now 0) from carry in bit7 and bit6 goes to bit7			--- B = 0
			orb	floSDC						; update bits 7-6 of $114 with slot used by SDC									--- floSDC.bits5-4 = %00
			stb	floSDC						; save new value
tst07		lda	floSDC						; get hdw control byte
			bita	#%00000100					; is bit2 ON (floppies present)?
			beq	etstHDW						; no, exit
			lda	hdwFlo						; get floppies slot																		--- A = $33
			anda	#%00000011					; get just slot number																	--- A = $03
			lsla									; move bits 0-1																			--- A = $06
			lsla									; to bits 5-4																				--- A = $0C
			lsla									;																								--- A = $18
			lsla									;																								--- A = $30
			ora	floSDC						; update bits 5-4 of $114 with slot used by Floppies							--- floSDC.bits7-6 = %11
			sta	floSDC						; save new value
etstHDW	rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; Tests for the presence of an MPI
; returns zero flag Z=1 if present else Z=0
; by Darren Atkinson (2015)
; modified to support mega-mini-MPI by Pere Serrat (2021)
; -------------------------------------------------------------------------------------------------------------------------------
tstMPI	pshs	a								; save register
			lda	cpyMPIcfg   				; get MPI settings
			eora  #$03      					; toggle two low used bits in /SCS nibble
			sta	cpyMPIcfg					; update copy
			sta   MPIcfg     					; update MPI settings
			cmpa  MPIcfg     					; did it accept the change? MANDATORY. Do not CHANGE it!
			beq   yesMPI     					; branch if yes (MPI present), will return with Z=1
			lda	#1								; will return with Z=0
			bra	noMPI							; exit
yesMPI	lda	cpyMPIcfg					; get old value
			eora	#$03							; recover modified bits in /SCS
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; back to its old value
			clra									; make Z=1
noMPI		puls	a,pc							; restore register and return

; -------------------------------------------------------------------------------------------------------------------------------
; Tests for the presence of a CoCo SDC on active slot
; returns zero flag Z=1 if present else to zero
; by Darren Atkinson (2015)
; -------------------------------------------------------------------------------------------------------------------------------
tstSDC	pshs	a,x							; save registers
   		ldx   #$ff4a    					; point X at Flash registers (FF42 for CoCo)
   		lda   #$1a      					; test pattern
   		sta   ,x        					; store in data register
   		lda   1,x       					; get value from control register
   		clr   ,x        					; clear data register
   		eora  1,x       					; get changed bits from control register
   		cmpa  #$18      					; did the expected bits change?
   		beq   yesSDC						; branch if SDC present, will return with Z=1
			lda	#1								; will return with Z=0
yesSDC	puls	a,x,pc						; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Test for presence of FDC in active MPI slot.
; Returns with Z=1 if FDC has been found
; by Darren Atkinson (2015)
; -------------------------------------------------------------------------------------------------------------------------------
tstFLO   pshs  d,x             			; save registers
         ldx   #$ff40          			; address for Dragon DOS controller
         lda   #$d0            			; FORCE INTERRUPT command
         sta   ,x              			; send to FDC command register
         bsr   fdcDelay        			; delay time
         lda   ,x              			; read FDC status to clear INTRQ
         ldd   2,x             			; get FDC sector and data registers
         coma                  			; invert sector bits only
         std   2,x             			; put both registers back
         bsr   fdcDelay        			; delay time
         subd  2,x             			; set Z if read matches write
         puls  d,x,pc          			; restore registers and return
fdcDelay pshs  y,x,d,cc        			; push registers
         mul                   			; consume 11 CPU cycles
         puls  cc,d,x,y,pc     			; restore registers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Command to change of MPI-slot and start it 
; it could happen that the new slot has no FDC nor SDC
; but if it had any of them, then floSDC must be adjusted accordingly
; -------------------------------------------------------------------------------------------------------------------------------
xslot		jsr   <redLChr						; peek at next character after "SLOT"
       	suba  #'N'							; to detect if it was an 'N' - if so do not switch to MAP0
       	sta   <noexec						; save difference. 0 = do not switch
       	bne   xslot1						; if not 'N' skip next
       	jsr   <getNChr						; jump over char 'N'
xslot1 	jsr	getunit						; read a number between 1-4
			beq	nogood						; if 0 error
			cmpb	#4								; is it greater than 4?
			bls	OKslot						; no, OK
nogood	ldb	#$0d							; MU error
			jmp	syserr						; bas_system_error
OKslot	bsr	tstMPI						; is there an MPI present?
			beq	OKslot01						; yes, skip section
			lbsr	NotMPIPresent				; no, show message
			rts									; return
OKslot01	decb									; slots range 0-3. We use reg B read at function getunit
			stb	<numUni						; save low nibble --- $00 - $03 if command was SLOTN1 - SLOTN4
			lslb									; move it			--- $00 - $06
			lslb									; to the 			--- $00 - $0c
			lslb									; high				--- $00 - $18
			lslb									; nibble				--- $00 - $30
			orb	<numUni						; add low nibble ($nn)		--- $00 - $33
			stb	<numUni						; save calculated code		--- $00 - $33
												; now must compare the new slot with the one for FDC and SDC and modify floSDC accordingly
												; by definition, switching the slot MUST enable the drives of that slot, so $114 must vary
			andb	#%00110000					; get just low bits of high nibble = new slot number	--- $00 - $30
			pshs	b								; save value														--- $00 - $30
			lda	floSDC						; get hdw control byte											--- $3C - $3C if we are in the FDC
			anda	#%00110000					; get the slot where the Floppies are						--- $30 - $30
			cmpa	,s+							; is new slot where the floppies are?						--- NO  - YES
			bne	xslot2						; no, compare SDC													--- jmp - mod
			lda	floSDC						; get control value
			anda	#%11111100					; set both floppies ON
			bra 	xslot3						; skip section - no need to verify SDC
xslot2	lda	<numUni						; get new slot number											--- $00 - $33
			anda	#%00110000					; get just low bits of high nibble = new slot number	--- $00 - $30
			asla									; move 2 bits to 
			asla									; the left															--- $00 - $00
			pshs	a								; save value														--- $00 - $00
			lda	floSDC						; get control bte
			anda	#%11000000					; get the slot where SD is										--- $00 - $00
			cmpa	,s+							; is the select one?												--- YES - NO
			bne	xslot4						; no, skip two next												--- mod - jump
			lda	floSDC						; get control value
			ora	#%00000011					; set both floppies OFF
xslot3	sta	floSDC						; update hdw control byte
													; this part MUST be run from RAM because we are going
													; to lose control when switching the ROM slot!
xslot4	ldx	#specia2						; point to beginning of code to execute from RAM
			ldu	#$1da							; point at destination
xsL01		ldd	,x++							; get a word
			std	,u++							; put at destination
			cmpx	#strROM+3					; got to the end?
			blo	xsL01							; no, loopback
			jmp	$1da							; jump to RAM copied code
specia2	orcc	#dimask						; disable interrupts
			tst	<noexec						; should switch to MAP0?								--- noexec = 0
			beq	specia3						; no, skip next											--- apply jump
			clr	$ffde							; switch to MAP0 (ROM mode)							--- skipped
specia3	ldb	<numUni						; get calculated code									--- B = $cc
			stb	cpyMPIcfg					; update copy
			stb	MPIcfg						; switch to that slot									--- MPIcfg = $cc
			tst	<noexec						; called with param N?
			bne	eOKSlot						; no, start slot
			rts									; return
eOKSlot	ldd	#$444b						; signature for O.S.
			cmpd	romini						; is an O.S. at $C000?
			bne	strROM						; no, start RomPack
			clr	<$71							; force coldstart
strROM	jmp	resetRt						; reset machine

; -------------------------------------------------------------------------------------------------------------------------------
; to access floppies or SDC-card
; uses byte floSDC ($114) to determine what it must access
; byte structure: 
; 	bit 7-6	- slot number where is the CoCo-SDC  (0-3)
; 	bit 5-4	- slot number where are the floppies (0-3)
; 	bit 3		- 1 if SDC present else 0
; 	bit 2		- 1 if Floppy controller present else 0
; 	bit 1		- for Unit #2: if 0 use floppy, if 1 use SDC
; 	bit 0		- for Unit #1: if 0 use floppy, if 1 use SDC
; -------------------------------------------------------------------------------------------------------------------------------
flOrSd	pshs	cc								; save register
			lda	floSDC						; get hdw control byte
			anda	#%00001100					; test bits3-2 (SDC and Floppies present)
			cmpa	#%00001100					; are both present?
			bne	eflOr							; no, exit
			lda	<$eb							; get unit number
			cmpa	#2								; is greater than 2?  --- If 3-4 and SDC present, should change to FDC slot!
			bhi	flo2flo						; yes, switch to floppies
			lda	floSDC						; get hdw control byte
			anda	<$eb							; test corresponding bit 0-1 (affected drive)
			beq	flo2flo						; if zero, switch to floppies
												; has to switch to slot with SDC (bits 7-6 of floSDC)
flo2SDC	lda	floSDC						; get hdw control byte
			anda	#%11000000					; use bits 7-6 to get slot number
			lsra									; send them
			lsra									; to bits 5-4 (2 low bits of high nibble)
			pshs	a								; save high nibble
			lsra									; move it
			lsra									; 4 times to the right
			lsra									; to have it
			lsra									; as low nibble
			ora	,s+							; add high nibble and clean stack
			cmpa	cpyMPIcfg					; already in that slot?
			beq	eflOr							; yes, do nothing
			bra	flOr01						; go update value
												; has to switch to slot with floppies (bits 5-4- of floSDC)
flo2flo	lda	floSDC						; get hdw control byte
			anda	#%00110000					; use bits 5-4 (2 low bits of high nibble)
			pshs	a								; save high nibble
			lsra									; move it
			lsra									; 4 times to the right
			lsra									; to have it
			lsra									; as low nibble
			ora	,s+							; add high nibble and clean stack
			cmpa	cpyMPIcfg					; already in that slot?
			beq	eflOr							; yes, do nothing
flOr01	sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; change active MPI slot
eflOr		puls	cc,pc							; restore register and return

; -------------------------------------------------------------------------------------------------------------------------------
; accept a value to be used as index disc in a HD in unit4 (for DW4)
; -------------------------------------------------------------------------------------------------------------------------------
xHDidx	jsr	getunit						; read a number between 1-90
			cmpb	#1								; value under minimum accepted?
			lblo	badParam						; yes, show message
			cmpb	#90							; exceeds the max index number?
			bhi	idxBad						; yes, show message
			stb	idxHD							; save disc index number
			jsr	mrkmod						; mark as modified
exHD		rts									; return
idxBad	ldx	#idxHigh-1					; poit to message
			jmp	outstr						; show it

; -------------------------------------------------------------------------------------------------------------------------------
; Command to deal with SDC-card DRIVE commands (1,2)
; -------------------------------------------------------------------------------------------------------------------------------
xsdrive	jsr	<redLChr						; peek at next char after SDRIVE
			lbeq	sdcLST						; is the SDRIVE alone
												; validate syntax and drive number
			jsr	getunit						; read a number between 1-2
			lbeq	badnum						; 0 not accepted
			cmpb	#2								; is it greater than 4?
			lbhi	badnum						; no, ok
												; update table of linked DW4 slots back to the number of the drive!	
			ldx	#drvtab-1					; point to linked slots -1, because we will use 1-2 as index (instead 0-1)
			stb	b,x							; link unit to DW4 slot with its same number
			decb									; internally will work with 0-1
			stb	<numUni						; save received number
			clr	<valPar						; value param is 0
			ldb	#$e0							; more used value
			stb	<cmdNum						; store it
			jsr	ckComma						; ckcomma. if not comma found syntax error
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; decode token
			cmpa	#$c2							; is this the token for off?
			lbeq	flopON						; yes, go process it
			cmpa	#$88							; is this the token for on?
			lbeq	flopOF						; yes, go process it
			cmpa	#$b3							; is this the token for get?
			lbeq	sdcGET						; yes, go process it
			cmpa	#$8c							; is this the token for dim?
			lbeq	sdcDIM						; yes, go process it
			cmpa	#$f6							; is this the token for unload?
			bne	nxtParam						; no, go to params section
			jsr	<getNChr						; yes, it is the 'unload' command version. must skip the token to avoid a sn error when done
reuseUL	ldd	#$0100						; get 1 byte with value equal null
			std	nambuf						; put in nambuf area
dTok01	bsr	putCmd						; skip parameters section
			rts									; return
; -------------------------------------------------------------------------------------------------------------------------------

												; search for more parameters
nxtParam	jsr   getfnam						; read string and length into $1d1 = nambuf
       	ldb	nambuf						; is string length 0?
       	lbeq	snError						; yes, show error
       	cmpb	#253							; is string longer than maximum accepted? (253 + m: + final null)
			lbcc	TooLong						; show error message
			jsr	<redLChr						; get last read char
			cmpa	#$2c							; is it a comma?
			bne	dTok01						; no, end of command. this is 'mount' command version
			jsr	<getNChr						; get next token
			lbeq	snError						; if no token after comma, show error
			clrb									; bit 7 value 0 means dsk format
			cmpa	#$97							; is it the token for new?
			beq	crtDisc						; yes, create a dsk file
			orb	#$80							; bit 7 value 1 means sdf format
			cmpa	#$98							; is it the token for def?
			lbne	snError						; no, show error message
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; create disc
crtDisc	stb	<noexec						; save format flag (bit7)
			lbsr	addExt						; add extension to the file name
			jsr	<getNChr						; get next char
			cmpa	#$2c							; is it a comma?
			beq	crt01							; yes, test size parameter
			lda	#1								; by default type=1
			ldx	#180							; and will be 180k (720 sectors)
			bra	crtstep						; exit cheks
crt01		jsr	<getNChr						; advance one char. mandatory for next routine
			jsr	get16bN						; get a 16 bit number into x
			lda	#1								; 1 will mean 180k dragon-like disc (40 tracks 1 side)
			cmpx	#180							; equals read value 180?
			beq	crtstep						; yes, exit checks
			inca									; 2 will mean 360k dragon-like disc (40 tracks 2 sides)
			cmpx	#360							; equals read value 360?
			beq	crtstep						; yes, exit checks
			inca									; 3 will mean 720k dragon-like disc (80 tracks 2 sides)
			cmpx	#720							; equals read value 720?
			lbne	BadFormat					; not, bad format number, show error
crtstep	adda	<noexec						; add disc type to format flag
			sta	<noexec						; save calculated value ($01-02-03 or $81-82-83)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; entries to send command to the sdc
												; entry for create new disc comands
			ldd	#"N:"							; command to be sent to create a file
			bsr	putCmd2						; send command
noMsgEr	rts									; return
												; entry for mount - unmount like commands
putCmd  	clr	<noexec						; no params needed (no disk to be created)
			ldd	#"M:"							; command to be sent to mount or unmount a drive
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; detect if sdc is already active
putCmd2	clr	hdwSdc						; no need to go back to old active slot
			lbsr	tstSDC						; is there an active sdc?
			beq	xsd01							; yes, go on
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; if present but not active change to that slot, save old one			
												; if sdc present, active it and mark somewhere that we must go back to the active one!
			pshs	d								; save registers
			lda	floSDC						; get hdw control byte
			bita	#%00001000					; is a sdc in the system?
			bne	vfySD01						; yes, jump section
			leas	4,s							; get rid of d and 1 return address
			ldx	,s								; see if return address is $0900 (from sdir?)
			cmpx	#$0900						; comes from sdir?
			bne	vfySD00						; no, skip next
			leas	2,s							; clean stack									
vfySD00	lbra	NotSDCPresent				; no, show message
												; calculate sdc slot
vfySD01	lda	floSDC						; get hdw control byte
			anda	#%11000000					; use bits 7-6 (slot where sdc is)
			lsra									; move them to
			lsra									; bits 5-4 (lower bits of high nibble)
			pshs	a								; save high nibble
			lsra									; low nibble   
			lsra									; gets high nibble
			lsra									; that will end  
			lsra									; as zero
			ora	,s+							; add high nibble (now they are the same value)
												; save active slot in hdwsdc
			ldb	cpyMPIcfg					; get active slot
			stb	hdwSdc						; save it for later use						
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; activate sdc
			puls	d								; restore registers
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; pass string to data buffer
xsd01		ldx   #nambuf+1					; point to 1st char of the input string
			ldu	#$0800						; point to 1st dos buffer
			std	,u++							; put at the beginning of the command buffer
			ldb	nambuf						; get string length
str2buf	lda	,x+							; get a char from input string
			sta	,u+							; copy in the buffer
			decb									; decrement counter
			bne	str2buf						; not yet done, loopback
			clr	,u								; put a final zero
			tst	<noexec						; was sdf format?
			bmi	addParms						; yes, put params: cylinders and sides into b:x
													; the other varians send these registers with zero value			
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; params to be passed (b:x)
			ldb	<valPar						; get b parameter value
			ldx	<$8a							; 16 bits zero					
			bra	sendIt						; send the command

; -------------------------------------------------------------------------------------------------------------------------------
												; special parameters for command 'create disc' (b:x)
addParms	lda	<noexec						; get disc type
			anda	#$03							; use lowest nibble
			ldb	#$28							; minimum value 40 cylinders
			cmpa	#3								; was type 3 (2880 sectors)?
			bne	addP01						; no, skip next
			aslb									; double cylinders number
addP01	ldx	#$0100						; one side by default
			cmpa	#1								; was type 0 or 1 (630 - 720 sectors)?
			bls	sendIt						; yes, skip next
			ldx	#$0200						; set double sided
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; send command to the sdc
													; b:x have zeros or tracks:sides (at x-high byte)
sendIt	lda	<cmdNum						; code for extended command with data block
			adda	<numUni						; add drive number
			ldu	#$0800						; point to beginning of data block
			jsr	CommSDC						; send command to sdc-card with (a,b:x,u)
			bcc	send01						; if ok, skip section
												; if error $10 and sdir command, show message (not error)
			bitb	#$10							; is bit 4 set?
			beq	nxtCEr						; no, goto next control
			pshs	d								; save registers
			lda	<valPar						; get parameter used
			cmpa	#'C'							; was it a 'C' (sdir)?
			bne	send00						; no, exit
			bsr	currentDirIsRoot			; show message
			puls	d								; restore registers
			orcc	#%00000001					; set carry to mark error
			bra	send02						; exit
send00	puls	d								; clean stack
			bra	send01						; exit
												; if error $08 and get-dim command, show message (not error)
nxtCEr	bitb	#$08							; is bit 3 set?
			beq	send01						; no, skip section
			anda	#$f0							; use high nibble
			cmpa	#$c0							; was command $cn (get-dim)?
			bne	send01						; no, go on
			orcc	#%00000001					; set carry to mark error
			lbra	exsdrv1						; exit
currentDirIsRoot								; current dir = root!
			lda	<numDir						; get number of directories requested in a series
			cmpa	#1								; is first one?
			bne	curNoMsg						; no, do not show message
			pshs	x,u							; save registers
			ldx	#noCurDir-1					; point to message
			jsr	outstr						; show it
			puls	x,u							; restore registers
curNoMsg	rts									; return
send01	lbsr	vfyStatus					; verify operation result
send02	tst	<noexec						; test format type
			lbeq	exsdrv1						; if coco-like 160k, all done, exit. should never happen
			lbmi	exsdrv1						; if sdf type, all done, exit	
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; additional swrite command for dragon-like disks
												; these three need a swrite,39(79),18(36),"",""
												; x must have the lsn number: 719,1439,2879
			ldx	#719							; to have 720 sectors
			lda	<noexec						; get disc type
			anda	#3								; use lowest 2 bits
			cmpa	#1								; is it a 180k (720 sectors) disc? 
			beq	sendWrt						; yes, send write order
			ldx	#1439							; to force 1440 sectors
			cmpa	#2								; is it a 360k (1440 sectors) disc?
			beq	sendWrt						; yes, send write order
			ldx	#2879							; to force 2880 sectors
										; *** ADDED CODE TO SUPPORT VDK FORMAT CREATING FILES	
												; here we must test the file extension to be ".VDK"
sendWrt	clr	<dsktyp						; Now equals 0 = DSK
			ldu	#nambuf						; point to filename buffer
			ldb	,u								; get filename length
			subb	#3								; to point to the possible dot (.) if .VDK was entered
			leau	b,u							; point to it
			ldd	#".V"							; get dot and letter V
			cmpd	,u++							; have we received it?
			bne	sWrt01						; no, skip section
			ldd	#"DK"							; get string DK
			cmpd	,u								; have we received it?
			bne	sWrt01						; no, skip section
												; now we know the user wants to create a virtual VDK file
			leax	1,x							; so that the disk has an extra sector (for the header)									
			inc	<dsktyp						; now is 1 = VDK
										; *** END_ADDED CODE TO SUPPORT VDK FORMAT CREATING FILES	
sWrt01	ldu	#$0800						; point to beginning of buffer
			clra									; create a  
			clrb									; 16 bits zero
cleanBuf	std	,u++							; clean a word
			cmpu	#$0900						; reached buffers end?
			blo	cleanBuf						; no, loopback
			ldu	#$0800						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
										; *** ADDED CODE TO FORMAT ON THE FLY DSK and VDK FILES
												; as the buffer is clean, we just need to fill bytes 0-19
													; for a VDK file, we will write a suitable header to sector #0
													; it should contain:
													;  position		length contents					 value
			ldd	#"dk"							;	offset 0     2     signature               ('dk')
			std	,u++
			ldd	#$0001						;	offset 2     2     Header length           ($00, $01) = $100
			std	,u++
			ldd	#$1010						;	offset 4     1     VDK version - actual    ($10)
			std	,u++							;	offset 5     1     VDK version - compat    ($10)
			ldd	#$8800						;	offset 6     1     Source id               ($88)
			std	,u++							;	offset 7     1     Source version          ($00)
			ldy	#$2801						; 40 tracks 01 side (180k)
			lda	<noexec						; get disc type
			anda	#3								; use lowest 2 bits
			cmpa	#1								; is it a 180k disc? 
			beq	noMore2						; yes, send write order
			leay	1,y							; to force double sided ($2802)
			cmpa	#2								; is it a 360k disc?
			beq	noMore2						; yes, send write order
			ldy	#$5002						; it is a 720k disk
noMore2	tfr	y,d							; pass geodata to regD
			sta	<$f2							; number of tracks to DOS system variable
			decb									; because 0 means 1 side
			stb	<$f4							; number of sides to DOS system variable
													;	offset 8     1     Number of tracks        ($28)          40 or 80 --> ($28 or $50)
			sty	,u++							;	offset 9     1     Number of sides         ($01)          1 or 2
			ldd	#$0040						;	offset 10    1     Flags                   ($00)
			std	,u++							;	offset 11    1     Comp & Name length      ($40)          bits 0-2 = compression [0=off, >0=TBD]
													;			                                                    bits 3-7 = filename length [min 0, max 31]
			ldx	#nambuf						;	offset 12    0-31  Disk name               (...)          (OPTIONAL) filename in ASCII. Doesn't need a zero terminator
			ldb	,x+							; get filename length
			subb	#4								; subtract 4 to have the actual name without dot and extension
cpy2buf	lda	,x+							; get a char from name
			sta	,u+							; put into buffer
			decb									; decrement counter
			bne	cpy2buf						; if not done, loopback, regB ends with value Zero
			ldu	#$0800						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			ldx	#$0000						; to point to sector 0 (header)
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
												; Once we have the header, we must add 20 tracks of $e5 bytes
			ldx	#$0800						; to use this buffer
			stx	<$ee							; update system pointer
			jsr	filbuf						; will fill it with bytes value $E5
			ldd	#$1412						; get number of tracks to write (20) and numsectors x track
			tst	<$f4							; is single sided?
			beq	noDouble						; yes, do not double
			aslb									; double number of sectors x track
noDouble	mul									; calculate total number of sector to write
			tfr	d,y							; pass value to regY
			ldx	#1								; 1st sector (LSN) to be written for a VDK file
			tst	<dsktyp						; are we creating a VDK?
			bne	nSector						; yes, skip next
			leax	-1,x							; 1st LSN for a DSK file (and so rewrite the header!)
nSector	pshs	x,y							; save variables
			ldu	#$0800						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			clrb									; higher byte to zero
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
			puls	x,y							; restore variables
			leax	1,x							; point to next LSN
			leay	-1,y							; decrement counter
			bne	nSector						; if not done, loopback	
			pshs	x								; put next LSN on stack
												; now we must add 1st FAT sector
			jsr	ffat1							; buffer: $800. fills 1st FAT sector upon geodata
			ldu	#$0800						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			clrb									; higher byte to zero. X is the good number
			ldx	,s								; get LSN from stack
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
												; let's try the same for track 16 (backup DIR)
												; we have to calculate the first sector for that track
			lda	#16							; number of track to write on
			ldb	#18							; number of sectors x track if single sided disk
			tst	<$f4							; is double sided?
			beq	1f								; no, skip next
			aslb									; double sectors x track
1			mul									; to get sectors before sector 1 track 16
			tst	<dsktyp						; is it a VDK?
			beq	2f								; no, skip next
			addd	#1								; add a sector (header)
2			std	<fST16						; save that value for later use
												; now write 1st sector Track 16
			ldu	#$0800						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			clrb									; higher byte to zero. X is the good number
			ldx	<fST16						; get LSN from variable
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
												; now we must add 2nd FAT sector
			jsr	ffat2							; buffer: $900. fills 2nd FAT sector upon geodata
			ldu	#$0900						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			clrb									; higher byte to zero. X is the good number
			ldx	,s								; get previous LSN from stack
			leax	1,x							; increment number
			stx	,s								; update it
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
												; and do the same for Track 16
			ldu	#$0900						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			clrb									; higher byte to zero. X is the good number
			ldx	<fST16						; get previous LSN from variable
			leax	1,x							; increment number
			stx	<fST16						; update it
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
												; now we go for the 16 directory sectors with entries ...
			jsr	fil16s						; buffer: $a00. fills a sector to be used as diretory sector (3-18)
			ldb	#16							; number of sectors to write
			puls	x								; get previous LSN from stack
			leax	1,x							; increment LSN number
nDSect	pshs	b,x							; save variables
			ldu	#$0a00						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			clrb									; higher byte to zero. X is the good number
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
			puls	b,x							; restore variables
			leax	1,x							; increment LSN number
			decb									; decrement sector counter
			bne	nDSect						; not done, loop
			pshs	x								; save next LSN (rest of the disk)
												; and do the same for Track16 too
			ldb	#16							; number of sectors to write
			ldx	<fST16						; get previous LSN from variable
			leax	1,x							; increment LSN number
n16Sect	pshs	b,x							; save variables
			ldu	#$0a00						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			clrb									; higher byte to zero. X is the good number
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
			puls	b,x							; restore variables
			leax	1,x							; increment LSN number
			decb									; decrement sector counter
			bne	n16Sect						; not done, loop
												; finally we will write the rest of the sectors with $e5
												; X points to next LSN. end od disk might be: 720, 1440, 2880 (due to the header)
			ldx	#$0800						; to use this buffer again
			stx	<$ee							; update system pointer
			jsr	filbuf						; will fill it with bytes value $E5
			puls	x								; get LSN for rest of the disk from stack
			ldy	#721							; value to detect end of disk for 180k disk
			lda	<noexec						; get disc type
			anda	#3								; use lowest 2 bits
			cmpa	#1								; is it a 180k (720 sectors) disk? 
			beq	1f								; yes, send write order
			ldy	#1441							; to detect end of 360 disk
			cmpa	#2								; is it a 360k (1440 sectors) disk?
			beq	1f								; yes, send write order
			ldy	#2881							; to detect end of 720k disk
1			tst	<dsktyp						; are we creating a VDK file?
			bne	nRDisk						; yes, skip next
			leay	-1,y							; subtract 1 (no header for DSK)
nRDisk	pshs	x,y							; save registers (LSN, endOfFile)
			ldu	#$0800						; point to buffers beginning
			lda	#$a0							; command to be sent: SDWRITE
			adda	<numUni						; add drive number
			clrb									; higher byte to zero. X is the good number
			jsr	CommSDC						; send command to SDC-card	(LSN = B:X)
			lbsr	vfyStatus					; verify operation result
			puls	x								; restore LSN number
			leax	1,x							; increment LSN number
			cmpx	,s								; is X greater than Y (in the stack)
			puls	y								; to balance stack
			bne	nRDisk						; not done, loop
												; call SDRIVE n,UNLOAD
												; first we must save the filename from nambuf or it will be lost
												; we need from nambuf to nambuf + 12 (8 plus dot plus VDK)
			ldx	#$a00							; point to last DOS buffer
			ldu	#nambuf						; point to filename buffer
			ldb	#13							; number of bytes to copy
1			lda	,u+							; get a byte from filename buffer
			sta	,x+							; put into DOS buffer
			decb									; decrement counter
			bne	1b								; not done, loop
			clr	<valPar						; value param is 0
			ldb	#$e0							; more used value
			stb	<cmdNum						; store it
			jsr	reuseUL						; call final part of SDRIVEx,UNLOAD
			ldx	#$a00							; point to last DOS buffer
			ldu	#nambuf						; point to filename buffer
			ldb	#13							; number of bytes to copy
1			lda	,x+							; get a byte from DOS buffer
			sta	,u+							; put into filename buffer
			decb									; decrement counter
			bne	1b								; not done, loop
			clr	<valPar						; value param is 0
			ldb	#$e0							; more used value
			stb	<cmdNum						; store it
			jsr	dTok01					; call SDRIVE n,"received FILENAME.VDK"
			lda	hdwSdc						; is here the old slot?
			beq	1f								; no, exit
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; active it
1			rts									; return
							; *** END_ADDED CODE TO SUPPORT VDK AND DSK FORMAT WHILE CREATING FILES	

; -------------------------------------------------------------------------------------------------------------------------------
												; restore old slot if needed
exsdrv	lda	hdwSdc						; is here the old slot?
			beq	exsdrv1						; no, exit
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; active it
exsdrv1	rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; special command floppy off
flopOF	bsr	flopCom						; see if it could be done
			orb	floSDC						; add to hdw control byte (a bit to 1 disables floppy and enables sdc)
eflop		stb	floSDC						; update hdw control byte
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; special command floppy on
flopON	bsr	flopCom						; see if it could be done
			comb									; invert all bits: %00000001 - %00000010 convert to %11111110 - %11111101
			andb	floSDC						; a bit 0 enables floppy and disables sdc
			bra   eflop							; update and return

; -------------------------------------------------------------------------------------------------------------------------------
												; common part for floppy on-off
flopCom	jsr	<getNChr						; skip token to avoid sn error when returning
			ldb	floSDC						; get hdw control byte
			andb	#%00001100					; see if switching is possible
			cmpb	#%00001100					; are both floppies and sdc present?
			bne	notAllow						; no, show error message
			ldb	<numUni						; get drive number (0-1)
			incb									; convert to 1-2
			rts									; return
notAllow leas	2,s							; clean stack (is inside one subroutine)
			ldx	#canNot-1					; point to message
			jmp	outstr						; show it and return

; -------------------------------------------------------------------------------------------------------------------------------
												; special command dim
sdcDIM	lda	#'Q'							; param to send into b
			bsr	sdcDAT						; process it
			bcs	noDiscEr						; if got "not disc" error, explain it
			lbsr	shwDIM						; show received data
rstSlot	bsr	exsdrv						; restore slot if needed
			andcc	#%11111110					; clear carry flag
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; special command get
sdcGET	lda	#'I'							; param to send into b
			bsr	sdcDAT						; process it
			bcs	noDiscEr						; if got "not disc" error, explain it
			bsr	shwGET						; show received data
			bra	rstSlot						; restore slot if needed

; -------------------------------------------------------------------------------------------------------------------------------
												; common part for get-dim
sdcDAT	sta	<valPar						; save it
			jsr	<getNChr						; skip read token
			lda	#$c0							; command to send
			sta	<cmdNum						; save it
			ldd	#$0100						; get 1 byte with value equal null
			std	nambuf						; put in nambuf area (no name)
			clr	<noexec						; no additional command needed
			ldd	<$8a							; get 16 bits zero
			jmp	putCmd2						; send command

; -------------------------------------------------------------------------------------------------------------------------------
												; message no disc in drive
noDiscEr	lda	<numUni						; gets 0-1
			adda	#$31							; converts to '1'-'2'
			jsr	outchr						; show drive number
			ldx	#sdcNoDsc-1					; point to message
			jmp	outstr						; show it

; -------------------------------------------------------------------------------------------------------------------------------
												; show received 'get' info
shwGET	lda	<numUni						; get drive number
			adda	#$31							; converts to '1'-'2'
			jsr	outchr						; show drive number
			lda	#':'							; get char ':'
			jsr	outchr						; show it
												; show drive status
			lda	#'o'							; get fixed 1st letter
			jsr	outchr						; show it
			lda	#'n'							; get on as default
			ldb	<numUni						; get drive unit
			incb									; convert  0-1 into 1-2
			andb	floSDC						; test corresponding bit of hdw control byte
			bne	sdcS00						; if not zero, skip next
			lda	#'f'							; get value for off
			jsr	outchr						; put first 'f'
			bra	sdcS01						; go put 2nd one instead of space
sdcS00	jsr	outchr						; show result
			lda	#32							; get space as inverted block to poke it
			ldx	<$88							; get cursor address
			sta	,x+							; put inverse space there
			stx	<$88							; update cusor address
			bra	sdcS02						; skip next
sdcS01	jsr	outchr						; show it
sdcS02	bsr	shwFdat						; show filename and type
			leau	17,u							; jump to length bytes
			bsr	shwFL							; show file length
			lda	#$0d							; get enter value
			jsr	outchr						; print it
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; show file length - structure of the fac
												;	$50 highest byte 	--- always $00
												;	$51 high byte		--- high byte of u
												;	$52 mid  byte		--- low  byte of u
												;	$53 lower byte		--- reg. a ($00 = multiple of 256)
shwFL		lda 2,u								; get highest byte in A
			ldb 1,u								; get high byte in B
			pshs d								; hold on
			lda ,u								; now get mid and low bytes
			ldb -1,u
			puls u
; Revised shwNum to take U and D
shwNum	cmpu #$0010							; Check if high word shows >1M
			blo less1M							; if not display size in bytes
			tfr u,d                    	; Otherwise take high word
			andb #$F0							; Clear low 4 bits
			asra									; Divide by 16 asr / ror
			rorb
			asra
			rorb
			asra
			rorb
			asra
			rorb
			pshs d								; Put B and then A onto stack
			clrb									; zero B
			pshs b								; stack zero top byte as well
			puls u								; get to U
			puls a								; and bottom byte in A
			incb									; flag for Megabytes
			pshs b								; remember flag	
			bra coreNum
less1M	pshs a								; stash mid byte
			pshs u								; and top two
			leas 1,s								; fudge frame drop high byte 
			puls u								; get u
			tfr b,a								; put low byte in a
			clrb									; flag for bytes
			pshs b								; remember
coreNum 	jsr	$dd7d							; integer to fac and normalize it
			jsr	$9587							; fac to ascii (x points to string ended with $00
													; calculate length and add spaces to have total length = 7
			leay	1,x							; point to first digit
			ldb	#8								; desired length + final null
			subb ,s								; leave a space if we need M indicator
shwN01	decb									; decrement counter
			lda	,y+							; get a digit
			bne	shwN01						; if not 0, loopback
			tstb									; needs extra spaces?
			beq	shwN02						; no, skip section
			lda	#32							; get space
shwN03	jsr	outchr						; print it
			decb									; decrement counter
			bne	shwN03						; if not zero, loopback
shwN02	jsr	outstr						; print lengths' string
			ldb 	,s+							; Are we printing Megabytes?
			beq 	skipM	 						; No, bytes 
			lda 	#77							; M character
			jsr	outchr						; print it
skipM		rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; show filename and extension			
shwFdat	ldu	#$800							; point to beginning of data
shwFd01	ldb	#8								; 8 chars for name
sdc01		lda	,u+							; get a char
			jsr	outchr						; show it
			decb									; decrement counter
			bne	sdc01							; not all, loopback
			lda	#'.'							; get a dot
			jsr	outchr						; show it
			ldb	#3								; 3 chars for extension
sdc02		lda	,u+							; get a char
			jsr	outchr						; show it
			decb									; decrement counter
			bne	sdc02							; not all, loopback
												; show file type and extra info
			ldb	,u+							; get byte 11 (type)
			lda	#32							; get space
			cmpb	#$02							; is byte hidden?
			bne	sdc05							; no, skip next	
			lda	#'h'							; get h char
sdc05		jsr	outchr						; show it
			lda	#32							; get space
			cmpb	#$01							; is it locked?
			bne	sdc06							; no, skip next	
			lda	#'l'							; get l char
sdc06		jsr	outchr						; show it
			cmpb	#$10							; is it a directory?
			beq	sdc04							; yes, get that text
			cmpb	#$04							; is it sdf type?
			beq	sdc03							; yes, get that text
			ldx	#datNUL-1					; else get spaces
			fcb	#$10							; to skip next
sdc03		ldx	#datSDF-1					; point to message
			fcb	#$10							; to skip next
sdc04		ldx	#datDIR-1					; point to message
			pshs	u								; save pointer
			jsr	outstr						; put the three chars
			puls	u								; restore pointer
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; command sdrive alone
sdcLST	clr	<numUni						; set drive 1
			lbsr	sdcGET						; get and show info for that drive
;			bcs	esdcLST						; if carry set, exit (no sdc present)
			brn	esdcLST						; if carry set, exit (no sdc present)
			ldx	<$a6							; get input pointer
			leax	-2,x							; move it back (will be moved forward later)
			stx	<$a6							; update it
			inc	<numUni						; det drive 2			
			lbsr	sdcGET						; get and show info for that drive
esdcLST	rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; show received 'dim' info
shwDIM	lda	<numUni						; get drive number
			adda	#$31							; converts to '1'-'2'
			jsr	outchr						; show drive number
			ldx	#totSect-1					; point to message
			jsr	outstr						; show it
			clra                   			; now get - Mike Miller 2021-05
			ldb $ff41              			; four - Mike Miller 2021-05
			tfr d,u                			; bytes - Mike Miller 2021-05
			ldd $ff42              			; in regs UD - Mike Miller 2021-05
			lbsr	shwNum						; process and show it
			jmp	sendCR						; send cr

; -------------------------------------------------------------------------------------------------------------------------------
												; command smkdir
xsmkdir	bsr	others						; read filename, get params
			ldd	#"K:"							; command to send
xsmk01	jsr	putCmd2						; send command
			lbra	rstSlot						; restore slot if needed

; -------------------------------------------------------------------------------------------------------------------------------
												; read filename and set parameters			
others	jsr   getfnam						; read string and length into $1d1 = nambuf
       	ldb	nambuf						; is string length 0?
       	lbeq	snError						; yes, show error
       	cmpb	#253							; is string longer than maximum accepted? (253 + m: + final null)
			bcs	other01						; no, skip section
			leas	2,s							; clean stack (we are inside a subroutine)
			lbra	TooLong						; show error message
other01	clrb									; set to zero
			stb	<valPar						; parameter for b
			stb	<noexec						; no more parmas
			stb	<numUni						; drive 0
			lda	#$e0							; command to send
			sta	<cmdNum						; save it
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; command skill
xskill	bsr	others						; read filename, get params
			ldd	#"X:"							; command to send
			bra	xsmk01						; go send command

; -------------------------------------------------------------------------------------------------------------------------------
												; command sren
xsren		jsr	getfnam						; get old name
       	ldb	nambuf						; is string length 0?
       	lbeq	snError						; yes, show error
       	cmpb	#125							; is string longer than maximum accepted? (125 for each one)
       	lbcc	TooLong						; show error message
												; send it to $800 ended with $00
			ldx   #nambuf+1					; point to 1st char of the input string
			ldu	#$0800						; point to 1st dos buffer
			ldb	nambuf						; get string length
xsren01	lda	,x+							; get a char from input string
			sta	,u+							; copy in the buffer
			decb									; decrement counter
			bne	xsren01						; not yet done, loopback
			clr	,u+							; put a final zero
			stu	<bufPtr						;save pointer
			jsr	getfnam						; get new name
       	ldb	nambuf						; is string length 0?
       	lbeq	snError						; yes, show error
       	cmpb	#125							; is string longer than maximum accepted? (125 for each one)
       	lbcc	TooLong						; show error message
												; add after old name
			ldx   #nambuf+1					; point to 1st char of the input string
			ldu	<bufPtr						; get old pointer
			ldb	nambuf						; get newname string length
xsren02	lda	,x+							; get a char from input string
			sta	,u+							; copy in the buffer
			decb									; decrement counter
			bne	xsren02						; not yet done, loopback
			stu	<bufPtr						; save last byte received
												; send it back to nambuf+1 - put length in nambuf 
			ldx	#nambuf+1					; point to 1st char in input buffer										
			ldu	#$800							; point to 1st byte of dos buffer
			clrb									; counter to zero
xsren03	lda	,u+							; get a byte
			sta	,x+							; put on the input buffer
			incb									; increment counter
			cmpu	<bufPtr						; got to the end?
			blo	xsren03						; no, loopback
			stb	nambuf						; save total string length 
			bsr	other01						; set parameters
			ldd	#"R:"							; command to send
			lbra	xsmk01						; go send command

; -------------------------------------------------------------------------------------------------------------------------------
; routine to add extension to the file name if not present
; param: noexec=$00 to add .DSK  --- noexec=$80 to add .SDF
; -------------------------------------------------------------------------------------------------------------------------------
addExt	pshs	d,x							; save pointers
			ldx	#nambuf						; poit to name buffer
			ldb   ,x+							; length of name
												; Check for existing extension
isadot  	lda   ,x+							; get a char from name
       	cmpa  #'.'							; is it a dot?
       	beq   eAddExt						; yes, exit
       	decb									; decrement counter
       	bne   isadot						; not end of string, get next char name
												; so, we must add an extension
       	tst	<noexec						; is for a SDF file?
       	bmi	addSDF						; yes, jump section
addDSK  	ldd   #$2e44						; get chars ".D"
       	std   ,x++							; add to end of name
       	ldd   #$534b						; get chars "SK"
       	std   ,x++							; add to end of name
			bra	add01							; skip next section
addSDF  	ldd   #$2e53						; get chars ".S"
       	std   ,x++							; add to end of name
       	ldd   #$4446						; get chars "DF"
       	std   ,x++							; add to end of name
add01		clr	,x								; put a null at next byte
			ldb   nambuf						; get old name length
		 	addb  #4								; add 4 (because of dot and extension added)
       	stb   nambuf						; save new length in byte before string
eAddExt	puls  d,x,pc     					; restore pointers and return

; -------------------------------------------------------------------------------------------------------------------------------
; Section to deal with SDC-card SCHD commands
; -------------------------------------------------------------------------------------------------------------------------------
xschd		jsr	<redLChr						; peek at next char after SCHD
			lbeq	snError						; if null, syntax error
			cmpa	#$22							; is this the double quote?  = new directory
			lbeq	setCD01						; do the same as SDIR = (by now!)
			cmpa	#$c6							; is it the token for /?
			beq	chgRoot						; process it
			cmpa	#'.'							; is it a dot '.'?
			lbne	snError						; no, show syntax error
			jsr	<getNChr						; get next char
			cmpa	#'.'							; is it another dot '.'?
			lbne	snError						; no, show syntax error
												; this is to set parent dir as actual
chgParD	jsr	<getNChr						; skip token
chwPD01	lda	#$02							; only one char in nambuf
			ldb	#'.'							; char to design root directory (twice)
			std	nambuf						; put signs
			stb	nambuf+2						; into input buffer
			bra	chgR01						; process it
												; this is to set root as actual dir
chgRoot	jsr	<getNChr						; skip token
			lda	#$01							; only one char in nambuf
			ldb	#'/'							; char to design root directory
			std	nambuf						; put data in input buffer area
chgR01	lbsr	other01						; set parameters
			lbra	setCD02						; go to process the requested directory

; -------------------------------------------------------------------------------------------------------------------------------
; Section to deal with SDC-card SDIR commands
; -------------------------------------------------------------------------------------------------------------------------------
xsdir		jsr	<redLChr						; peek at next char after sdir
			lbeq	shwCAct						; is the sdir alone = show current dir contents
												; decode token - sign
			cmpa	#$b3							; is the token for get?
			lbeq	shwCDir						; yes, show current dir name
			cmpa	#$87							; is it the token for print (question mark)?
			lbeq	shwFull						; yes, show the full path name of actual dir (maybe substitute sdir get!)
			cmpa	#$c4							; is this the token for ('-')? = show contents of root dir
			lbeq	shwRoot						; yes, go process it
			cmpa	#$cb							; is this the token for ('=')? = set current dir
			lbeq	setCDir						; yes, go process it
			cmpa	#$22							; is this the sign ('"')?  = show contents of the requested dir
			lbne	snError						; no, show syntax error
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; show contents of requested dir
												; send a initiate directory listing command
shwRDir	lbsr	others						; read filename, get params
shwEDir	ldd	#"L:"							; command to send
			jsr	putCmd2						; send command
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; ask for directory pages to the sdc
shwRD00	lbsr	other01						; set some parameters
			ldb	#'>'							; special parameter
			stb	<valPar						; save it
			lda	#$c0							; command to send
			sta	<cmdNum						; save it
			ldd	#$0100						; mark 1 byte with value equal null
			std	nambuf						; put in nambuf area (no name)
			jsr	putCmd2						; send command
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			ldx	#$0800						; point to buffer beginning
			clrb									; counter to zero
shwRD01	lda	,x								; get first char (1st name character)
			beq	shwRD02						; if zero, no more items
			incb									; increment received items counter
			leax	16,x							; point to next item
			cmpx	#$0900						; got to end of buffer?
			blo	shwRD01						; no, loopback
shwRD02	tstb									; have we read any item?
			beq	shwRD09						; no, print ok and exit
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			stb	<itsRead						; save number of items we received
			ldu	#$0800						; point to beginning of received data
			stu	<cpyPtr						; save it
			clr	<itsDone						; for each item (clear counter)
shwRD03	inc	<itsDone						; add 1 to counter
			lbsr	shwFd01						; show item name and type
			leax	,u								; point to msb highest byte of item length (not used)
			ldu	,x								; get 2 next bytes (Mike Miller)
			ldd	2,x							; get lowest byte (Mike Miller)
			lbsr	shwNum						; show item Length (U:A)
			ldx	<cpyPtr						; get beginning of this record
			leax	16,x							; point to next one
			stx	<cpyPtr						; update pointer
			ldb	<itsRead						; get number of items read
			cmpb	#16							; was 16?								
			bne	shwRD07						; NO, so partial page. Send CR and go for next one
			lda	<itsDone						; YES, it was a full page. Get item number just printed
			cmpa	#15							; was penultimate row?
			bne	shwRD04						; no, see if it was the last one
			ldx	#msgCont-1					; yes point to message CONT
			jsr	outstr						; show message			
			bra	shwRD07						; and send CR and go for next item	
shwRD04	cmpa 	#16							; is the last one?
			bne	shwRD07						; no, send CR and go for next item
			ldx	#msgExit-1					; yes, point to message EXIT
			jsr	outstr						; show message			
shwRD05	jsr	$8006							; wait for user keypress
			beq	shwRD05						; none, loopback
			cmpa	#$43							; is 'C' for Continue?
			bne	shwRD06						; no, verify if is 'E'
			jsr	sendCR						; send a CR (will scroll 1 line up)
			bra   shwRD00						; BREAK, ask for a new page to the SDC
shwRD06	cmpa	#$45							; is 'E' for eXIT?
			bne	shwRD05						; no, loopback and wait for a correct key
			bra	shwRD08						; yes, send CR, show OK and exit
shwRD07	jsr	sendCR						; send CR
			lda	<itsDone						; get number of items shown
			cmpa	<itsRead						; work done?
			beq	shwRD09						; yes, send OK and exit
			ldu	<cpyPtr						; no, get saved buffer pointer
			bra	shwRD03						; loopback for next item
shwRD08	lda	#$0d							; get enter value
			jsr	outchr						; print it
shwRD09	lbsr	exsdrv						; restore slot if needed
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
												; show name of current dir
shwCDir	jsr	<getNChr						; skip token
			lda	#1								; put value 1
			sta	<numDir						; as first level requested
			lbsr	other01						; put some parameters
			lda	#$c0							; command to be sent
			sta	<cmdNum						; save it
			ldb	#'C'							; param for b
			stb	<valPar						; save it
			ldd	#$0100						; get 1 byte with value equal null
			std	nambuf						; put in nambuf area (no name)
			jsr	putCmd2						; send command
			bcs	eshwCDir						; if error, don't show garbage
			lbsr	shwFdat						; show dirname and type
eshwCDir	lbra	exsdrv						; restore slot if needed

; -------------------------------------------------------------------------------------------------------------------------------
												; show contents of root dir
shwRoot	jsr	<getNChr						; skip token
			lda	#$01							; only one char in nambuf
			ldb	#'/'							; char to design root directory
shwRt01	std	nambuf						; put data in input buffer area
			lbsr	other01						; set parameters
			lbra	shwEDir						; go to process the requested directory

; -------------------------------------------------------------------------------------------------------------------------------
												; show contents of current dir
shwCAct	lda	#$01							; only one char in nambuf
			ldb	#'*'							; char to design current directory
			bra	shwRt01						; complete data and process command

; -------------------------------------------------------------------------------------------------------------------------------
												; set current dir to requested name
setCDir	jsr	<getNChr						; get next entered char
setCD01	lbsr	others						; read filename, get params
setCD02	ldd	#"D:"							; command to send
			lbra	xsmk01						; go send command

; -------------------------------------------------------------------------------------------------------------------------------
												; show actual dir full path
shwFull	jsr	<getNChr						; skip token
			leas	-2,s							; create a 3 bytes hole for variables
			clr	numDir						; set items received to 0
			ldx	#$0900						; get 2nd dos buffer address
			stx	destPtr,s					; to receive results
shwF01	lbsr	other01						; put some parameters
			lda	#$c0							; command to be sent
			sta	<cmdNum						; save it
			ldb	#'C'							; param for b
			stb	<valPar						; save it
			ldd	#$0100						; get 1 byte with value equal null
			std	nambuf						; put in nambuf area (no name)
			inc	numDir						; increment received items counter
			jsr	putCmd2						; send command to get actual directory
												; processing single answer
			bcs	shwF80						; if error (got to root) skip section  
												; save received data to $900 buffer
			ldx	#$800							; point to beginning of received data
			ldu	destPtr,s					; point ot destination record
			ldb	#8								; max name length 
shwF02	lda	,x+							; get a received char
			cmpa	#$20							; is it space
			beq	shwF03						; yes, stop copying name
			sta	,u+							; pass char to record
			decb									; decrement counter
			bne	shwF02						; if not zero, loopback
shwF03	ldb	,x+							; get 1st termination char
			cmpb	#$20							; is it a space?
			beq	shwF04						; yes, goto next dir element
			lda	#'.'							; get dot char
			std	,u++							; add dot and 1st ext char
			lda	,x+							; get 2nd extension char
			cmpa	#$20							; is it a space?
			beq	shwF04						; yes, goto next dir element
			sta	,u+							; add 2nd extension char
			lda	,x+							; get 3rd extension char
			cmpa	#$20							; is it a space?
			beq	shwF04						; yes, skip next
			sta	,u+							; add 3rd extension char
shwF04	ldd	#$2f00"						; get a slash and zero terminator
			std	,u								; add to destination buffer
			ldu	destPtr,s					; get beginning just used
			leau	16,u							; add 16 to buffer pointer
			stu	destPtr,s					; update it for next path directory
			lbsr	chwPD01						; ask SDC to go to parent dir (SCHD"..")
			bra	shwF01						; go back to ask for new actual directory
												; got to the root, NO MORE data to request. 
shwF80	lda	<numDir						; get number of received items
			cmpa	#1								; only one item received?
			beq	eshwFull						; yes, it was ROOT, so exit
												; process received data to show full path
			ldu	#nambuf+1					; destination for full path name (input buffer)
			lda	#$2f							; get a slash to denote root
			sta	,u+							; put into buffer as 1st char
												; add all the received dirs in reverse order
			ldx	destPtr,s					; get last record address (not used ... it was root)
shwF81	leax	-16,x							; point to previous used entry
			cmpx	#$0900						; gone back past the beginning?
			bcs	shwF90						; yes, end of putting items together
			pshs	x								; save register
shwF82	lda	,x+							; get a char from origin
			beq	shwF83						; if null, end of string
			sta	,u+							; else put at destination
			bra	shwF82						; go get next char
shwF83	puls	x								; restore pointer
			bra	shwF81						; back to get next directory
shwF90	clr	,u								; mark end of string
			tfr	u,d							; send poiter to D
			subd	#nambuf+1					; to calculate length of string
			stb	nambuf						; save it
			ldx	#nambuf						; point to whole path name
			jsr	outstr						; show the full mounted directory path
			lda	#$0d							; code for Enter
			jsr	outchr						; print it
												; issue a SCHD"" with the whole path to restore situation!!
			leas	2,s							; clean stack
			lbsr	other01						; set some parameters
			ldd	#"D:"							; command to send	
			lbra	xsmk01						; go back to the actual directory we had before
eshwFull	leas	2,s							; clean stack
			lbra	exsdrv						; exit

; -------------------------------------------------------------------------------------------------------------------------------
; process for command WRITE MEM @bank, start, destination, length
; this code is made based on the FLASHER.BIN program by Darren Atkinson
; -------------------------------------------------------------------------------------------------------------------------------
xwrite	jsr	<getNChr						; get next char
			cmpa	#$9b							; is the token for MEM?
			lbne	snError						; no, show error message
			jsr	<getNChr						; get next byte
			cmpa	#64							; is it sign '@'?
			lbne	snError						; no, show error message
			jsr	<getNChr						; get number into A
			cmpa	#$30							; is greater or equal 0?
			bcs	xwrt01						; no, show error
			cmpa	#$37							; is less or equal 7?
			bls	xwrt02						; yes, goto next parameters
xwrt01	ldb	#$11							; SD error
			jmp	syserr						; bas_system_error
xwrt02	anda	#$07							; use only valid bits
			sta	<DCDRV 						; save requested bank number
			jsr	<getNChr						; advance one char
			bne	morParms						; if more chars, read them
												; default parameters for writting a ROMPACK
			ldx	#$3000						; get origin of data
			stx	<dSource						; save start into variable
			ldx	#$c000						; get destination in bank
			stx	<dDest						; save destination into variable
			ldx	#$3f00						; get max. length (16k-256bytes)
			bra	savLen						; skip section
morParms	jsr	ckComma						; CkComma
			jsr	get16bN						; get a 16 bit number into X
			cmpx	#basini-1					; is it on the upper 32k?
			bhi	badParam						; yes, bad format number, show error
			stx	<dSource						; save start into variable
			jsr	<redLChr						; advance one char
			jsr	ckComma						; CkComma
			jsr	get16bN						; get a 16 bit number into X
			cmpx	#romini						; is it out of SDC-ROM area?
			blo	badParam						; yes, bad format number, show error
			cmpx	#$f000						; is it higher than the beginning of 4th sector?
			bhi	badParam						; yes, bad format number, show error
			stx	<dDest						; save destination into variable
			jsr	<redLChr						; advance one char
			jsr	ckComma						; CkComma
			jsr	get16bN						; get a 16 bit number into X
			tfr	x,d							; pass value to D
			addd	<dDest						; add destination value
			bcs	badParam						; yes, sum exceeds flash capacity
			cmpd	#$ff00						; uses result the reserved 256 bytes?
			bhi	badParam						; yes, bad format number, show error
			tfr	x,d							; get length again
			addd	<dSource						; add source beginning
			cmpd	#basini-1					; exceeds RAM area?
			bhi	badParam						; yes, bad format number, show error
savLen	stx	<FPA0							; save byte count
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; search slot where SDC is
			lbsr	tstSDC						; is SDC already active?
			beq	xwrt03						; yes, save slot number (if no MPI this will be $00)
			lda	floSDC						; no, get hdw control byte
			bita	#%00001000					; is a SDC in the system?
			lbeq	NotSDCPresent				; no, show message
												; yes, calculate occupied slot
			lda	floSDC						; get hdw control byte
			anda	#%11000000					; use bits 7-6 (slot where SDC is)
			lsra									; move them to
			lsra									; bits 5-4 (lower bits of high nibble)
			pshs	a								; save high nibble
			lsra									; low nibble   
			lsra									; gets high nibble
			lsra									; that will end  
			lsra									; as zero
			ora	,s+							; add high nibble (now they are the same value)
			bra	xwrt04						; save slot number
xwrt03	lda	cpyMPIcfg					; get active slot number
xwrt04	sta	<sdcSlot						; save slot where SDC is
												; copy the flasher routine to $1da. Should not exceed $2d2
cpyFlsh	ldx	#xflash						; point to beginning of program to be copied
			ldu	#$1da							; point to destination
cpyFl01	ldd	,x++							; get a word
			std	,u++							; put at destination
			cmpx	#erasEnd+2					; got to end of program?
			blo	cpyFl01						; no, loopback
			jmp	$1da							; execute copied program in low RAM

; -------------------------------------------------------------------------------------------------------------------------------
badParam	ldx	#wrongPar-1					; point to message
			jsr	outstr						; show it
			jsr	<redLChr						; read last char
			beq	ebadPrm						; if null, exit
bP001		jsr	<getNChr						; skip an unused byte
			bne	bP001							; if not null, loopback
ebadPrm	rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; This code manages the process of flashing a bank
; -------------------------------------------------------------------------------------------------------------------------------
xflash	orcc  #$50                    ; mask interrupts
         lda	<sdcSlot						; get the slot where SDC is
												; this is to avoid starting the just flashed bank!
         ldb	cpyMPIcfg					; get current active slot
			stb	<sdcSlot						; save it to go back to it at the end
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; switch to the SDC slot
		   sta   R1CLR                   ; normal speed for CoCo 3 (0,89MHz) - $ffd8 (resets $ffd7)
         sta   RAMROM                  ; activate RAM/ROM map mode = MAP0 - resets $ffdf
         ldb   <DCDRV                  ; get target bank number (parameter at $eb)
         bsr   FlashKill               ; erase target bank
         leax  burner,pcr	            ; point X at burner subroutine
         stx   <DCBPT                  ; store subroutine address: $ee
			ldx	<dSource						; point X at source data
			ldu	<dDest                  ; point U at flash destination
         bsr   callX                   ; write data to the flash
         ldb	<sdcSlot						; get original active slot
         stb	cpyMPIcfg					; update copy
         stb	MPIcfg						; set it again
         clr   <$71                    ; force a cold boot
         orcc  #$50                    ; mask interrupts
         jmp   [$fffe]                 ; execute the system reset routine

; -------------------------------------------------------------------------------------------------------------------------------
;  Erase flash bank specified in ACCB
; -------------------------------------------------------------------------------------------------------------------------------
FlashKill 
			stb   <TargBank               ; store bank number: $86
			lda	<FPA0							; get byte count																			; 2B - 20
			anda	#$f0							; use only high nibble																	; 20 - 20
			pshs	a								; push value (number of sectors of 4K)												; 20 - 20
			ldd	<FPA0							; get full byte count																	; 2B00 - 2000
			anda	#$0f							; mask high nibble of high byte														; 0B00 - 0000
			cmpd	#$0000						; is masked D equal 0?																	; NO   - YES
			beq	flK01							; yes, don't correct values															; corr - jump
			lda	#$10							; add one sector (x16)
			adda	,s								; to pushed value																			; 30
			sta	,s								; update it in the stack																; 30
flK01		puls	a								; get number of sectors to kill (x16 'cause in high nibble)					; 30 - 20
			cmpa	#$40							; number of sectors greater than 4?
			bls	flK02							; no, go on
			lda	#$40							; yes, limit to 4
flK02		tfr	a,b							; copy to B																					; 30 - 20
			lsrb									; move
			lsrb									; 4 times to
			lsrb									; the right to
			lsrb									; divide by 16 to get the actual number of sectors	to kill					; 03 - 02
			lda	<dDest						; get destination ini high byte														; D3 - C0	($D300 - $C000)
			anda	#$f0							; use high nibble																			; D0 - C0
												; now D should contain: 
												;     A = High byte of address of 1st sector to kill ($C0-$D0-$E0-$F0)
												;     B = number of sectors to be killed
												; save parameter for later use
         sta   <FPA0+2                 ; store MSB of default sector address: $52
         stb   <ErasCtr                ; store sector count: $03
         clr   <FPA0+3                 ; clear LSB of sector address: $53
         leax  eraser,pcr              ; point X at erasure routine
         stx   <DCBPT						; save it: $ee
callX    lda   <TargBank      			; get current flash bank selection: $86
         anda  #$07                    ; mask out undefined bits
         pshs  a,cc                    ; save current bank and IRQ mask
         jsr   [DCBPT]                 ; call utility routine: $ee -> eraser (routine)
         lda   $ff22                   ; read PIA data reg to clear any CART interrupt
         puls  cc,a                    ; restore old IRQ mask and pop saved bank number
         clr   FLSHREG                 ; clear the Flash byte register: $ff4a
         rts                           ; return

; -------------------------------------------------------------------------------------------------------------------------------
; Write Data to Flash
; Entry:
;   X = source data address
;   U = destination flash address
;   High word of FPA0 = byte count ($50)
; -------------------------------------------------------------------------------------------------------------------------------
burner   ldd   <FPA0                   ; D = byte count: $50
         leay  d,u                     ; Y = destination end address
         sty   <FPA0+2                 ; save end address in FPA0: $52
         leay  ,x                      ; point Y at the source data
         leax  ,u                      ; point X at the destination
         leau  burnPrfx,PCR            ; point U at the prefix data for Byte Program
         lda   <TargBank               ; get target bank number: $86
         sta   BANKREG                 ; activate target bank: ff4b
         ora   #$80                    ; setup bank number combined with PGM Enable
burn010  ldb   ,y+                     ; get next data byte from source
         cmpb  ,x                      ; does flash already contain this value?
         bne   burn020                 ; branch if needs to change
         leax  1,x                     ; increment flash address
         bra   burn030                 ; skip ahead to end of loop
burn020  pshs  u,x,b,a                 ; save registers
         bsr   flshPrefx               ; send the unlock sequence
         puls  a,b,x,u                 ; restore target bank and destination address
         bsr   flshWrCyc               ; write the byte to the Flash destination
burn030  cmpx  <FPA0+2                 ; have we reached the final address: $52
         blo   burn010                 ; loop if more
         bra   flshDone                ; restore original bank selection and return
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; - - -  Prefix Data for Byte-Program  - - -
burnPrfx fdb   $81aa                   ; Bank = 1, Data = $AA...
         fdb   $d555                   ; ...Address = $D555 ($5555)
         fdb   $8055                   ; Bank 0, Data = $55...
         fdb   $eaaa                   ; ...Address = $EAAA ($2AAA)
         fdb   $81a0                   ; Bank 1, Data = $A0...
         fdb   $d555                   ; ...Address = $D555 ($5555)
         fcb   $0		                  ; end of sequence

; -------------------------------------------------------------------------------------------------------------------------------
; Send a Programming Prefix Sequence to the Flash.
; Enter with U pointing to Prefix data.
; -------------------------------------------------------------------------------------------------------------------------------
flshPrefx
			pulu  a,b,x                   ; get next bank, data byte and address
         tsta                          ; if reached end-of-sequence..
         beq   flshRts                 ; ..then return
         bsr   flshWrCyc               ; write byte to flash
         bra   flshPrefx               ; loop for next byte of sequence

; -------------------------------------------------------------------------------------------------------------------------------
												; Restores old flash bank and returns upon completion of Flash operation
flshDone lda   3,s                     ; get orignal bank number from stack
         anda  #$7f                    ; make sure the PGM Enable bit is cleared
												;  *** Fall Thru ***
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Perform a Write Cycle to the Flash.
; Enter with Address in X, Bank+PGM Enable in ACCA and the data byte in ACCB.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
flshWrCyc
			sta   BANKREG                 ; select Flash bank / PGM Enable: $ff4b
         stb   FLSHREG                 ; store data byte in Flash register: $ff4a
         bitb  ,x+                     ; perform Flash Write by reading target address
flshRts  rts                           ; return

; -------------------------------------------------------------------------------------------------------------------------------
; Erase Flash Sector
; -------------------------------------------------------------------------------------------------------------------------------
eraser   leau  erasPrfx,PCR            ; point U to the prefix data for Sector Erase
         bsr   flshPrefx               ; write the prefix bytes
         ldd   #$8030                  ; ACCA = PGM Enable;  ACCB = final prefix byte
         ora   <TargBank               ; combine target bank number with PGM Enable: $86
         ldx   <FPA0+2                 ; X = sector address: $52 ($C0xx)
         bsr   flshWrCyc               ; write final prefix byte at sector address
         anda  #$07                    ; turn off the..
         sta   BANKREG                 ; ..PGM Enable bit: $ff4b
         clra                          ; clear ACCD for use..
         clrb                          ; ..as timeout counter
eras025  tst   -1,x                    ; poll the sector address: $C0xx-1
         bmi   eras030                 ; branch if bit 7 is set (erasure complete)
         subd  #1                      ; decrement timeout counter
         bne   eras025                 ; loop if not yet timed out
eras030  leax  $0fff,x                 ; increment X to point at the next sector
         stx   <FPA0+2                 ; save sector address in FPA0: $52 ($D0xx)
         dec   <ErasCtr                ; decrement erase sector counter: $03
         bne   eraser                  ; loop if erasing another sector
         bra   flshDone                ; restore original bank selection and return
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
												; - - -  Programming Prefix for Sector Erase  - - -
erasPrfx fdb   $81aa                   ; Bank = 1, Data = $AA...
         fdb   $d555                   ; ...Address = $D555 ($5555)
         fdb   $8055                   ; Bank 0, Data = $55...
         fdb   $eaaa                   ; ...Address = $EAAA ($2AAA)
         fdb   $8180                   ; Bank 1, Data = $80...
         fdb   $d555                   ; ...Address = $D555 ($5555)
         fdb   $81aa                   ; Bank = 1, Data = $AA...
         fdb   $d555                   ; ...Address = $D555 ($5555)
         fdb   $8055                   ; Bank 0, Data = $55...
         fdb   $eaaa                   ; ...Address = $EAAA ($2AAA)
         fdb   $0000                   ; end of sequence
erasEnd  fdb   $0000							; ...a record is 4 bytes long ...

; -------------------------------------------------------------------------------------------------------------------------------
; process for command SCOPY MEM @number   ,&HC000,&H4000,&H3F00
; or alternate        SCOPY MEM SLOTnumber,&HC000,&H4000,&H3F00
; -------------------------------------------------------------------------------------------------------------------------------
xscopy	jsr	<getNChr						; get next char
			cmpa	#$9b							; is the token for MEM?
			lbne	snError						; no, show error message
			jsr	<getNChr						; get next byte
			sta	<cpyWhat						; save received command
			cmpa	#64							; is it sign '@' for BANK?
			beq	xsbnk01						; yes, go get bank number
			cmpa	#$ef							; is the token for SLOT?
			lbne	snError						; no, show error
												; processing slot command
			jsr	<getNChr						; get number into A
			cmpa	#$31							; is greater or equal 1?
			bcs	xscerr1						; no, show error
			cmpa	#$34							; is less or equal 4?
			bhi   xscerr1						; no, show error 
			deca									; internally MUST work with 0-1-2-3
			bra	xsParam						; yes, goto next parameters
xscerr1	ldb	#$0d							; MU error
			bra	xscerr0						; goto bas_system_error
												; processing bank command
xsbnk01	jsr	<getNChr						; get number into A
			cmpa	#$30							; is greater or equal 0?
			bcs	xscerr2						; no, show error
			cmpa	#$37							; is less or equal 7?
			bls	xsParam						; yes, goto next parameters
xscerr2 	ldb	#$11							; SD error
xscerr0	jmp	syserr						; bas_system_error
												; look for more parameters
xsParam	suba	#$30							; convert ASCII to integer (0-3 or 0-7)
			sta	<oriNum 						; save requested bank/slot number
			jsr	<getNChr						; advance one char
			bne	otherPar						; if more chars, read them
												; default parameters for writting a ROMPACK
			ldx	#$c000						; get origin of data
			stx	<dSource						; save start into variable
			ldx	#$3000						; get destination in bank
			stx	<dDest						; save destination into variable
			ldx	#$3f00						; get max. length (16k-256bytes)
			bra	storeLen						; skip section			
otherPar	jsr	ckComma						; CkComma
			jsr	get16bN						; get a 16 bit number into X
			cmpx	#romini						; is it on the upper 32k?
			lblo	badParam						; no, bad format number, show error
			stx	<dSource						; save start into variable
			jsr	<redLChr						; advance one char
			jsr	ckComma						; CkComma
			jsr	get16bN						; get a 16 bit number into X
			cmpx	#$4000						; is it leaving space for a 16k ROM?
			lbhi	badParam						; yes, bad format number, show error
			stx	<dDest						; save destination into variable
			jsr	<redLChr						; advance one char
			jsr	ckComma						; CkComma
			jsr	get16bN						; get a 16 bit number into X
storeLen	tfr	x,d							; pass value to D
			addd	<dDest						; add destination value to calculate end value
			cmpd	#basini-1					; does result exceed the RAM area?
			lbhi	badParam						; yes, bad format number, show error
			std	<FPA0							; save last byte to be written
			tfr	x,d							; get byte count again
			addd	<dSource						; add first one to copy
			lbcs	badParam						; if carry it goes past $FFFF, show message
												; stack control
			ldd	<FPA0							; get last byte to write
			addd	#50							; 50 security bytes (stack depth)
			cmpd	<$21							; is last calculated address lower than stack pointer?
			blo	stkOK							; yes, so it's sure to continue
			ldd	<dDest						; get first destination byte in RAM
			cmpd	<$23							; is higher than strings beginning area?
			bhi	stkOK							; yes, so it's sure to continue
			ldx	#stackPrb-1					; point to message
			jmp	outstr						; show it
												; HDW verification
stkOK		lda	cpyWhat						; get kind of origin to copy
			cmpa	#$ef							; is SLOT requested?
			bne	seeBank						; no, go to bank control
			lbsr	tstMPI						; yes, is there an active MPI?
			beq	xsProces						; yes go on	
			lbsr	NotMPIPresent				; no, show message
			rts									; return
seeBank	lda	floSDC						; get hdw control byte
			bita	#%00001000					; is there an SDC?
			lbeq	NotSDCPresent				; no, show error
												; section to copy the code to be executed into $1da
xsProces	ldx	#xsMovCod					; point to origin of code
			ldu	#$1da							; point to destination
xsMov01	ldd	,x++							; get a word
			std	,u++							; put at destination
			cmpx	#xsLoEnd+1					; got to the end?
			blo	xsMov01						; no, loopback
			jsr	$1da							; call code at $1da
			rts									; return
												; code to be executed
xsMovCod	orcc	#dimask						; disable interrupts
			clra									; $00 is code for NO old value
			sta	oldSlot						; for old slot
			deca									; $ff is code for NO old value
			sta	oldBank						; for old bank
			lda	<cpyWhat						; get kind of element to copy
			cmpa	#64							; was command a copy BANK?
			beq	xsCopBnk						; yes, go there
												; change to the requested slot if not already there and save old slot
			lda	cpyMPIcfg					; get active slot number
			sta	<oldSlot						; save it
			anda	#3								; to have just the number
			cmpa	<oriNum						; is the requested one?
			beq	startCpy						; yes, go start the copy
			ldb	<oriNum						; get requested slot number
			pshs	b								; save it
			lslb									; move to
			lslb									; the high
			lslb									; nibble
			lslb									; so 
			orb	,s+							; now both are equal
			stb	cpyMPIcfg					; update copy
			stb	MPIcfg						; change of slot
			bra	startCpy						; done, go start the copy
xsCopBnk	lda	floSDC						; get hdw control byte
			anda	#%11000000					; use bits 7-6 (slot where SDC is)
			lsra									; move them to
			lsra									; bits 5-4 (lower bits of high nibble)
			pshs	a								; save high nibble
			lsra									; low nibble   
			lsra									; gets high nibble
			lsra									; that will end  
			lsra									; as zero
			ora	,s+							; add high nibble (now they are the same value)
			cmpa	cpyMPIcfg					; is this slot active right now?
			beq	cmpBank						; yes, go see if bank differs
			ldb	cpyMPIcfg					; get actual slot
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; change of slot
			stb	<oldSlot						; save old slot
cmpBank	lda	<oriNum						; compare requested bank with actually selected one 
			cmpa	$ff4b							; is active now?
			beq   savBank						; yes, save bank
			ldb	$ff4b							; no, get active one
			sta	$ff4b							; change to requested
			tfr	b,a							; pass B to A
savBank	sta	<oldBank						; save old bank number
startCpy	clrb									; 0 = not in MAP1
			lda	romini						; get value in $C000
			coma									; reverse bits
			sta	romini						; update ram cell
			cmpa	romini						; retains the new value?
			bne	noMap1						; no, we are in MAP0
			coma									; reverse again
			sta	romini						; restore value
			incb									; 1 = we are in MAP1
			sta	$ffde							; switch to MAP0 to copy the requested ROM
noMap1	stb	<oriNum						; re-use variable to save that value
			ldx	<dSource						; point to source ROM
			ldu	<dDest						; point to destination RAM
xsLoop01	ldd	,x++							; get a word
			std	,u++							; put at destination
			cmpu	<FPA0							; got to the end?
			bls	xsLoop01						; no, loopback
												; restore old bank-slot numbers and Map Type
			lda	<oldSlot						; get old slot
			beq	getBank						; if $00, do nothing
			sta	cpyMPIcfg					; update copy
			sta	MPIcfg						; change to old slot number
getBank	lda	<oldBank						; get old bank number
			bmi	exscopy						; if $ff, do nothing
			sta	$ff4b							; switch to old bank number
exscopy	tst	<oriNum						; were we initially in MAP1?
			beq	noMapChg						; no, skip next
			sta	$ffdf							; back to MAP1
noMapChg	andcc	#eimask						; enable interrupts
			clr	$ff48							; to unsure no led stays lit on FDC
xsLoEnd	rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; routine to verify the status returned by the SDC to a command
;         Bit 7 ($80) set on any failure.
;         Bit 5 ($20) set if target file is already in use.
;         Bit 4 ($10) set if target file not found.
;         Bit 3 ($08) set on miscellaneous hardware errors.
;         Bit 2 ($04) set if path name is invalid.			
; -------------------------------------------------------------------------------------------------------------------------------
vfyStatus
			bcc 	evfy							; if no error return
         tstb									; update flags upon result status 
         beq 	timeOutError				; if zero, timeout
         bitb 	#$20							; detected file in use?
         bne 	targetInUseError			; yes, message
         bitb 	#$10							; was file not found?
         bne 	targetNotFoundError		; no, message
		   bitb 	#$08							; was a hdw error?
         bne 	miscHardwareError			; yes, message
         bitb 	#$04							; was invalid pathname?
         bne 	pathNameInvalidError		; yes, message
unknownError									; this is for unknown errors. Should not get here!
			ldx 	#unknown-1					; point to message
         fcb 	#$10							; skip next ldx 
targetInUseError								; 
         ldx 	#targetInUse-1				; point to message
         fcb 	#$10							; skip next ldx 
targetNotFoundError							; 
         ldx 	#targetNotFound-1			; point to message 
         fcb 	#$10							; skip next ldx 
miscHardwareError								; 
			ldx 	#miscHardware-1			; point to message 
         fcb 	#$10							; skip next ldx 
pathNameInvalidError							; 
			ldx 	#pathNameInvalid-1		; point to message 
         fcb 	#$10							; skip next ldx 
timeOutError 									; 
			ldx 	#timeout-1					; point to message 
Exit		jmp   outstr						; show message
evfy		rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; CoCo SDC Low-level interface routine
; -------------------------------------------------------------------------------------------------------------------------------
; CommSDC by Darren Atkinson
;
;    This is the core routine used for all
;    transactions with the SDC controller.
;
; Entry:
;    A = Command code
;    B = LSN hi byte   or  First parameter byte
;    X = LSN lo word   or  2nd and third parameter bytes
;    U = Address of 256 byte I/O buffer ($ffff = none)
;
; Exit:
;    Carry set on error.
;    B = controller status code.
;    A, X, Y and U are preserved.
;--------------------------------------------------------------------------
CommSDC  pshs  u,y,x,a,cc  			  	; preserve resgisters
         lsr   ,s          			  	; shift carry flag out of saved cc
												; put controller in command mode
         ldy   #DATREGA    			  	; setup y for hardware addressing (ff42 - ff4a for coco)
         lda   #CMDMODE    			  	; the magic number
         sta   +6,y        			  	; was -10,y for coco - send to control latch (ff48 - ff40 for coco)
												; put input parameters into the hardware registers.
												; it does no harm to put random data in the
												; registers for commands which dont use them.
         stb   -1,y        			  	; high byte to param reg 1 (ff41 - ff49 for coco)
         stx   ,y          			  	; low word to param regs 2 and 3 (ff42-ff43 - ff4a-ff4b for coco)
												; wait for not busy.
         bsr   waitForIt   			   ; run polling loop
         bcs   cmdExit     			   ; exit if error or timeout
												; send command to controller
         lda   1,s         			   ; get preserved command code from stack
         sta   -2,y        			   ; send to command register (ff40 - ff48 for coco)
												; determine if a data block needs to be sent.
												; any command which requires a data block will
												; have bit 5 set in the command code.
         bita  #$20        			  	; test the "send block" command bit
         beq   rxBlock     			  	; branch if no block to send
													; wait for ready to send
         bsr   waitForIt   			   ; run polling loop
         bcs   cmdExit     			   ; exit if error or timeout
         leax  ,u          			   ; move data address to x
													; send 256 bytes of data
         ldd   #32*256+8   			  	; 32 chunks of 8 bytes
txChunk  ldu   ,x          			  	; send one chunk...
         stu   ,y							; ff42-ff43 - ff4a-ff4b for coco
         ldu   2,x
         stu   ,y
         ldu   4,x
         stu   ,y
         ldu   6,x
         stu   ,y
         abx               			  	; point x at next chunk
         deca              			  	; decrement chunk counter
         bne   txChunk     			  	; loop until all 256 bytes sent
												; wait for command completion
         lda   #5          			  	; timeout retries
waitCmplt
			bsr   waitForIt   			  	; run polling loop
         bitb  #BUSY       			  	; test busy bit
         beq   cmdExit     			  	; exit if completed
         deca              			  	; decrement retry counter
         bne   waitCmplt   			  	; repeat until 0
         coma              			  	; set carry for timeout error
         bra   cmdExit     			  	; exit
												; for commands which return a 256 byte response block the
												; controller will set the ready bit in the status register
												; when it has the data ready for transfer.   for commands
												; which do not return a response block the busy bit will
												; be cleared to indicate that the command has completed.
rxBlock  bsr   longWait    			  	; run long status polling loop
         bls   cmdExit     			  	; exit if error, timeout or completed
         leax  1,u         			  	; test the provided buffer address
         beq   cmdExit     			  	; exit if "no buffer" ($ffff)
         leax  ,u          			  	; move data address to x
												; read 256 bytes of data
         ldd   #32*256+8   			  	; 32 chunks of 8 bytes
rxChunk  ldu   ,y          			  	; read one chunk... (ff42-ff43 - ff4a-ff4b for coco)
         stu   ,x
         ldu   ,y
         stu   2,x
         ldu   ,y
         stu   4,x
         ldu   ,y
         stu   6,x
         abx               			  	; update x for next chunk
         deca              			  	; decrement chunk counter
         bne   rxChunk     			  	; loop until all 256 bytes transferred
         clrb              			  	; status code for success, clear carry
												; exit
cmdExit  rol   ,s            				; rotate carry into saved cc on stack
         clr   +6,y          				; was -10,y for coco - end command mode (ff48 - ff40 for coco)
         puls  cc,a,x,y,u,pc 				; restore irq masks, update carry and return

; -------------------------------------------------------------------------------------------------------------------------------
; Wait for controller status to indicate either "Not Busy" or "Ready".
; Will time out if neither condition satisfied within a suitable period.
;
; Exit:
;    CC.C set on error or time out.
;    CC.Z set on "Not Busy" status (if carry cleared).
;    B = status
;    X is clobbered.  A, Y and U are preserved.
; -------------------------------------------------------------------------------------------------------------------------------
longWait bsr   waitForIt     				; enter here for doubled timeout
         bcc   waitRet       				; return if cleared in 1st pass
waitForIt
			ldx   #0            				; setup timeout counter
waitLp   comb                				; set carry for assumed FAIL
         ldb   -2,y          				; read status (FF40 - FF48 for CoCo)
         bmi   waitRet       				; return if FAILED
         lsrb                				; BUSY --> Carry
         bcc   waitDone      				; branch if not busy
         bitb  #READY/2      				; test READY (shifted)
         bne   waitRdy       				; branch if ready for transfer
         bsr   waitRet       				; consume some time
         ldb   #$81          				; status = timeout
         leax  ,-x           				; decrement timeout counter
         beq   waitRet       				; return if timed out
         bra   waitLp        				; try again
waitDone clrb                				; Not Busy: status = 0, set Z
waitRdy  rolb                				; On Ready: clear C and Z
waitRet  rts                 				; return

; -------------------------------------------------------------------------------------------------------------------------------
; EXPERimenTs area
; to help Boot any 'bootable' disk in drive 1 if SPACE IS pressed
; or start AUTOLOAD.DWL if SHIFT is NOT pressed
; -------------------------------------------------------------------------------------------------------------------------------
expert	ldb	#1								; set drive number
			stb	<$eb							; in system variable
			stb	>$60a							; and in DOS area
			lda   $ff02							; save PIA value
         ldb   #$7f							; value to test for Space & Shift
         stb   $ff02							; put it in the PIA
         ldb   $ff00							; read PIA output
         sta   $ff02							; restore old value in PIA
         comb									; invert read bits
         andb  #$20							; is bit5 set?
	 		bne   doBoot						; yes, SPACE is pressed, BOOT
noBoot	jsr   $c16a							; attempt to read a disk sector
			bra	doDWAuto						; simulate it has failed
doBoot	lds	#$7f36						; set new stackpointer
         bsr	rebReset						; rebuild Reset
			andcc	#$af							; enable interrupts
			jmp	$c13d							; call BOOT drive 1
doDWAuto clr	$ff48							; ensure motors off
			andcc #$af							; enable interrupts 
         ldx   #$b4b2						; get Basic Intro message 
         jsr   outstr						; show it
         ldx   #$c34c						; get DOS intro message
         jsr   outstr						; show it
			bsr	rebReset						; rebuild Reset
			lbsr	chkshift						; check for shift to call AUTOLOAD.DWL
retFDwl	
			andcc	#$af							; enable interrupts
			jmp	okprmpt						; goto Basic Iterpreter

; -------------------------------------------------------------------------------------------------------------------------------
rebReset	ldx   #$c6b4						; get new reset routine
			stx	<$72							; put in variables area
			lda	#$55							; flag value for warmreset
			sta	<$71							; update variable
			rts									; return

; -------------------------------------------------------------------------------------------------------------------------------
; DWLOAD implementation for patched Dragon 32 ROM (upper 8KB) 2014 Tormod Volden
; -------------------------------------------------------------------------------------------------------------------------------
												; Entry point from command line (DWLOAD or EXEC)
												; for later to see that we came from DWLOAD command
dwload   clr   <bootflag					; clear our autoboot flag
         ldx   #dwtext-1					; 
         jsr   outstr						; print string
Start		orcc  #IntMasks					; disable FIRQ, IRQ
         ldx   #PIA1Base      			; PIA1
         ldb   #$02
         stb   ,x             			; make RS-232 output marking
												; Spin for a while so that the RS-232 bit stays hi for a time
Reset		ldx   #$a000
Spin     leax  -1,x
         bne    Spin
												; Request named object from DW server
         lda   <bootflag					; coming from autoboot?
         bne   autoreq						; then use default name
         jsr   <redLChr						; peek at next character
         suba  #'N'							; DLOADN ?
         sta   <noexec						; 0 = noexec
         bne   getn
         jsr   <getNChr						; read next char (the N)
getn     jsr   getfnam						; read file name and length into $1d1 = namebuf
         ldx   #namebuf-1					; packet buffer start
         clr   ,x								; zero MSB for Y
         ldy   ,x++							; length of file name (16 bit)
         beq   noname						; no file name given?
         ldb   -1,x							; length of name (8 bit)
         clr   b,x							; zero terminate name string (for error)
         inc   ,--x							; 1 = DriveWire OP_NAMEOBJ_MOUNT
         leay  2,y							; length of DW packet, name length + 2
reqobj	jsr   DWWrite
         ldx   #dnum							; Our drive number variable
         clr   ,x
         leay  1,y							; read one byte back (Y is 0 after DWWrite)
         jsr   DWRead						; get drive number
         bcc	reqOK							; if open has not failed go on
			rts									; else return
reqOK    tst   ,x
         bne   ReadDECB						; successful mount
         tst   <bootflag
         lbne  stealth						; silent failure, BASIC OK prompt
         ldb   #$19							; MO ERROR
         jmp   syserr						; system error
autoreq  sta   <noexec        			; a is non null, autoexec
noname   ldx   #autoname
         ldy   #12+2							; length of DW packet
         bra   reqobj
												; named object has been mounted, proceed to read in file
ReadDECB	ldx   #0000
         stx   sector
         ldu   #endbuf    					; start with empty buffer
			cmpx	#nextseg
			brn	nextseg
			sts	sstack
												; read DECB header
nextseg  ldy   #5
         ldx   #decbhdr   					; copy DECB header here
         jsr   copybuf    					; moves x ahead
         lda   -5,x       					; DECB segment header starts with zero
         beq   ml         					; normal data segment
         cmpa  #$55       					; Dragon DOS file?
         bne   noddd
         leay  4,y        					; remaining header bytes (y is 0 here)
         jsr   copybuf
         dec   -8,x       					; Dragon DOS BASIC = 1
         bne   ddbin
         leax  -1,x       					; length at x-5
         bra   ldbas
ddbin    ldy   -5,x       					; length
         ldx   -7,x       					; load address
         jsr   copybuf    					; read whole file
         ldx   #decbhdr+8 					; exec address ptr + 2
         bra   oldsp
noddd    inca
         lbne  ErrDWL     					; must be $FF otherwise
         leay  1,y        					; Y is 0 after copybuf
         cmpy  sector     					; only first sector can be BASIC header
         bne   endseg     					; otherwise end flag
         cmpu  #startbuf+5					; must be first segment also
         bne   endseg
												; loading DECB BASIC file
												; bytes 4 and 5 are actually part of the program
         leau  -2,u       					; u was 5 bytes into read buffer here
												; at this point x is past 5-byte header, u is 3 bytes into first 256-bytes block
												; for Dragon DOS x is 8 bytes into header, u is 9 bytes into first 256-bytes block
ldbas    ldy   -4,x       					; read whole BASIC program
         ldx   <$19
         jsr   copybuf
         jsr   prbang
												; set up BASIC pointers and finish
         stx   <$1b       					; end of BASIC program
         stx   <$1d
         stx   <$1f
         tst   <noexec
	 		lbeq  $b72d      					; print OK, run basvect1, basvect2, readline
         jsr   basvect1   					; BasVect1 reset BASIC stack etc
         jsr   basvect2   					; BasVect2 initialize BASIC
         jmp   runbasic   					; run_basic
ml       ldy   -4,x       					; DECB segment length
         ldx   -2,x       					; DECB segment loading address
         jsr   copybuf
         bra   nextseg
prbang   jsr   backspc    					; print backspace to devnum
         lda   #'!'       					; print bang
         jmp   outchr     					; print to devnum
endseg	ldu   -4,x       					; new stack pointer specified?
         bne   setsp
oldsp    ldu   sstack     					; otherwise restore original SP
setsp    tfr   u,s
         bsr   prbang
         andcc #~IntMasks 					; enable interrupts
         ldx   -2,x       					; exec address
         tfr   x,d
         inca
         beq   retbas     					; return to basic if exec address $FFxx
         stx   <$9d       					; save BASIC EXEC address
         tst   <noexec
         beq   retbas
         jsr   ,x         					; and run loaded program
retbas   rts
												; vector table for chainloaders, etc
dwrvec   fdb   DWRead
dwwvec   fdb   DWWrite
dwdvec   fdb   DoRead
         fdb   0
dwtext   fcc   /DWLOAD/
         fcb   $0d,0
fromboot ldu   ,s++							; check return address (and drop it)
         cmpu  #$b469						; ROM code location (not a copy)
         bne   gocmdl						; return if run from a copied code segment
         cmpx  #wrmStrt						; coldstart sets this, warmstart doesn't
         bne   gocmdl						; return if warmstart
												; check for SHIFT key
chkshift lda   $ff02							; save PIA
         ldb   #$7f
         stb   $ff02
         ldb   $ff00
         sta   $ff02							; restore PIA
         comb
         andb  #$40
	 		bne   ste01							; exit but enabling interrupts
         incb									; B was 0 before
         stb   <bootflag					; our autoboot flag = 1
         jsr   Start
			bra	ste01							; exit but enabling interrupts
autoname fcb   $01							; OP_NAMEOBJ_MOUNT
         fcb   12								; length of name string
         fcn   /AUTOLOAD.DWL/
ErrDWL   lbra  ioerror						; BASIC IO ERROR

stealth  ldd	#retFDwl						; get return address from DWLOAD
			cmpd	2,s							; is this on stack as 2nd retAdrs?
			bne	ste01							; no, skip next one
			leas	2,s							; get rid of return address (from checkshift)
			rts									; return

ste01		andcc #~IntMasks					; enable interrupts
gocmdl   jmp   okprmpt						; BASIC OK prompt
												; copy y chars from read buffer to x, updates u
copybuf
copyl    cmpu  #endbuf
         bne   copym
												; fill up buffer via DW - resets buffer pointer u
         pshs  x,y
         ldx   sector
         ldy   #startbuf
         tfr   y,u
         bsr   DoRead
         bcs   ErrDWL
         bne   ErrDWL
         leax  1,x
         stx   sector
         lda   #'.'							; print dot
         jsr   outchr
         puls  x,y
copym    lda   ,u+
         sta   ,x+
         leay  -1,y
         bne   copyl
         rts

; -------------------------------------------------------------------------------------------------------------------------------
													; below code is taken from toolshed/dwdos
DoRead	lda   <dnum							; our drive number
         clrb									; LSN bits 23-16
         pshs  d,x,y
         lda   #OP_READEX
ReRead   pshs  a
         leax  ,s
		 	ldy   #$0005
		 	lbsr  DWWrite
		 	puls  a
		 	ldx   4,s							; get read buffer pointer
		 	ldy   #256							; read 256 bytes
		 	ldd   #133*1						; 1 second timeout
		 	bsr   DWRead
         bcs   ReadEx
         bne   ReadEx
												; Send 2 byte checksum
		 	pshs  y
		 	leax  ,s
		 	ldy   #2
		 	lbsr  DWWrite
		 	ldy   #1
		 	ldd   #133*1
		 	bsr   DWRead
		 	leas  2,s
		 	bcs   ReadEx
       	bne   ReadEx
		 	lda   ,s
		 	beq   ReadEx
		 	cmpa  #E_CRC
		 	bne   ReadErr
		 	lda   #OP_REREADEX
		 	clr   ,s
		 	bra   ReRead  
ReadErr  comb
ReadEx	puls  d,x,y,pc

; -------------------------------------------------------------------------------------------------------------------------------
; DWRead
;    Receive a response from the DriveWire server.
;    Times out if serial port goes idle for more than 1.4 (0.7) seconds.
;    Serial data format:  1-8-N-1
;    4/12/2009 by Darren Atkinson
; -------------------------------------------------------------------------------------------------------------------------------
;    Added a timeout control to the Becker part to avoid infinite loops
;    11-Mars-2016 by Pere Serrat
; -------------------------------------------------------------------------------------------------------------------------------
; Entry:
;    X  = starting address where data is to be stored
;    Y  = number of bytes expected
; Exit:
;    CC = carry set on framing error, Z set if all bytes received
;    X  = starting address of data received
;    Y  = checksum
;    U is preserved.  All accumulators are clobbered
; -------------------------------------------------------------------------------------------------------------------------------
			IF becker > 0
DWRead   clra                      		; clear Carry (no framing error)
         deca                    		; clear Z flag, A = timeout msb ($ff)
         tfr	cc,b
         pshs  u,x,dp,b,a        		; preserve registers, push flags and timeout msb
         leau  ,x								; U points to received data buffer
         ldx   #$0000						; checksum to zero
         leas	-2,s							; create a word hole for a 16 bits counter
loop0    ldd	#$F000						; reset counter to a fixed value
			std	,s								; to count 16 times 256
loop1    dec	1,s							; decrement low byte counter
			bne	loop2							; if not zero, skip next byte control
			inc	,s								; increment high byte
			beq	notReady						; if zero, exit via notReady
loop2		ldb   $ff49							; pol if DW is ready
         bitb  #$02							; is ready?
         beq   loop1							; no, loop
         ldb   $ff4a							; get data byte
         stb   ,u+							; put at destination buffer
         abx									; accumulate in checksum word
         leay  ,-y							; decrement byte counter
			bne	loop0							; not all received, loopback
			leas	2,s							; discard word counter off the stack
         tfr   x,y							; pass checksum to Y
         ldb   #0
         lda   #3
         leas  1,s               		; remove timeout msb from stack
outLoop  inca                    		; A = status to be returned in C and Z (bit2-Z; bit0-C), now A=4 (Z=1 C=0)
         ora   ,s                		; place status information into the..
         sta   ,s                		; ..C and Z bits of the preserved CC
         leay  ,x                		; get checksum into Y
         puls  cc,dp,x,u,pc      		; restore registers and return

notReady leas	3,s							; to remove word counter and timeout
			clra									; to get a #$01 in next opcode (to set Carry flag)
			bra	outLoop						; update flags and exit

			ELSE
DWRead   clra                    		; clear Carry (no framing error)
         deca                    		; clear Z flag, A = timeout msb ($ff)
         tfr   cc,b
         pshs  u,x,dp,b,a        		; preserve registers, push timeout msb
         tfr   a,dp              		; set direct page to $FFxx
         setdp $ff
         leau  ,x                		; U = storage ptr
         ldx   #0                		; initialize checksum
         lda   #$01              		; A = serial in mask
         bra   rx0030            		; go wait for start bit
												; Read a byte
rxByte   leau  1,u               		; bump storage ptr
         leay  ,-y               		; decrement request count
         lda   <BBIN             		; read bit 0
         lsra                    		; move bit 0 into Carry
         ldd   #$ff20            		; A = timeout msb, B = shift counter
         sta   ,s                		; reset timeout msb for next byte
         rorb                    		; rotate bit 0 into byte accumulator
rx0010   lda   <BBIN             		; read bit (d1, d3, d5)
         lsra
         rorb
         bita  1,s               		; 5 cycle delay
         bcs   rx0020            		; exit loop after reading bit 5
         lda   <BBIN             		; read bit (d2, d4)
         lsra
         rorb
         leau  ,u
         bra   rx0010
rx0020   lda   <BBIN             		; read bit 6
         lsra
         rorb
         leay  ,y                		; test request count
         beq   rx0050            		; branch if final byte of request
         lda   <BBIN             		; read bit 7
         lsra
         rorb                    		; byte is now complete
         stb   -1,u              		; store received byte to memory
         abx                     		; update checksum
         lda   <BBIN             		; read stop bit
         anda  #$01              		; mask out other bits
         beq   rxExit            		; exit if framing error
												; Wait for a start bit or timeout
rx0030   bita  <BBIN             		; check for start bit
         beq   rxByte            		; branch if start bit detected
         bita  <BBIN             		; again
         beq   rxByte
         ldb   #$ff              		; init timeout lsb
rx0040   bita  <BBIN
         beq   rxByte
         subb  #1                		; decrement timeout lsb
         bita  <BBIN
         beq   rxByte
         bcc   rx0040            		; loop until timeout lsb rolls under
         bita  <BBIN
         beq   rxByte
         addb  ,s                		; B = timeout msb - 1
         bita  <BBIN
         beq   rxByte
         stb   ,s                		; store decremented timeout msb
         bita  <BBIN
         beq   rxByte
         bcs   rx0030            		; loop if timeout hasn't expired
         bra   rxExit            		; exit due to timeout
rx0050   lda   <BBIN             		; read bit 7 of final byte
         lsra
         rorb                    		; byte is now complete
         stb   -1,u              		; store received byte to memory
         abx                     		; calculate final checksum
         lda   <BBIN             		; read stop bit
         anda  #$01              		; mask out other bits
         ora   #$02              		; return SUCCESS if no framing error
												; Clean up, set status and return
rxExit   leas  1,s               		; remove timeout msb from stack
         inca                    		; A = status to be returned in C and Z
         ora   ,s                		; place status information into the..
         sta   ,s                		; ..C and Z bits of the preserved CC
         leay  ,x                		; return checksum in Y
         puls  cc,dp,x,u,pc      		; restore registers and return
         setdp $00
			ENDIF

; -------------------------------------------------------------------------------------------------------------------------------
; DWWrite
;    Send a packet to the DriveWire server.
;    Serial data format:  1-8-N-1
;    4/12/2009 by Darren Atkinson
; Entry:
;    X  = starting address of data to send
;    Y  = number of bytes to send
; Exit:
;    X  = address of last byte sent + 1
;    Y  = 0
;    All others preserved
; -------------------------------------------------------------------------------------------------------------------------------
			IF becker > 0
DWWrite  pshs  d,cc              		; preserve registers
txByte	lda   ,x+
         sta   $ff4a
         leay  -1,y          	      	; decrement byte counter
         bne   txByte        	      	; loop if more to send
         puls  cc,d,pc       	      	; restore registers and return

			ELSE							   
DWWrite  pshs  dp,d,cc       	      	; preserve registers
         ldd   #$04ff        	      	; A = loop counter, B = $ff
         tfr   b,dp          	      	; set direct page to $FFxx
         setdp $ff		   			
         ldb   <$ff23        	      	; read PIA 1-B control register
         andb  #$f7          	      	; clear sound enable bit
         stb   <$ff23        	      	; disable sound output
         fcb   $8c           	      	; skip next instruction
txByte   stb   <BBOUT        	      	; send stop bit
         ldb   ,x+           	      	; get a byte to transmit
         nop				   			
         lslb                	      	; left rotate the byte two positions..
         rolb                	      	; ..placing a zero (start bit) in bit 1
tx0020   stb   <BBOUT        	      	; send bit (start bit, d1, d3, d5)
         rorb                	      	; move next bit into position
         exg   a,a		   			
         nop				   			
         stb   <BBOUT        	      	; send bit (d0, d2, d4, d6)
         rorb                	      	; move next bit into position
         leau  ,u			   			
         deca                	      	; decrement loop counter
         bne   tx0020        	      	; loop until 7th data bit has been sent
         stb   <BBOUT        	      	; send bit 7
         ldd   #$0402        	      	; A = loop counter, B = MARK value
         leay  ,-y           	      	; decrement byte counter
         bne   txByte        	      	; loop if more to send
         stb   <BBOUT        	      	; leave bit banger output at MARK
         puls  cc,d,dp,pc    	      	; restore registers and return
         setdp $00
			ENDIF

; ===============================================================================================================================
												; THIS PART WILL ONLY BE NEEDED IF WE MODIFY THE PATCHES-CODE
												; IN THIS CASE THE DOSPLUS HAS TO BE RE-PATCHED TO UPDATE LINKS/HOOKS
												; No problem as long as we just work from the init routine downwards
												; simply EXEC $reptch from $6xxx having DPlus at $4000 and resave the DOS
												; for easy of use just need to do: jmp $c005
; ===============================================================================================================================
												; 1st patch. To redirect ReadAbsoluteSector and WriteAbsoluteSector to DW4 new process
reptch	leau	$c106,pcr					; the DOS address code to be redirected here
			ldd	#$7e12						; opcodes for jmp and NOP
			ldx	#entry						; new entry point for DOS sector READ-WRITE commands
			sta	,u+							; put jmp
			stx	,u++							; put the new dispatch routine
			stb	,u								; put NOP
												; 2nd patch - for DSKINIT while writting to NOT directory sectors
			leau	$c325,pcr					; point to intercept
			ldx	#inidsk						; new added code
			sta	,u+							; put jmp
			stx	,u								; put the new code address
												; 3rd patch to fill the three buffers for DSKINIT ($800-$900-$a00) writting to directory sectors
			leau	$c9f3,pcr					; point to intercept
			ldx	#fildir						; new added code
			sta	,u+							; put jmp
			stx	,u								; put the new code address
												; 4th patch to correct track number when verifying tracks in DSKINIT to VDK files
			leau	$d1a4,pcr					; point to intercept
			ldx	#modtrk						; new added code
			sta	,u+							; put jmp
			stx	,u								; put the new code address
												; 5th patch to update system table at $6a7-8-9-a for BACKUP
			leau	$c836,pcr					; point to intercept
			ldx	#patbck						; new added code
			sta	,u+							; put jmp
			stx	,u								; put the new code address
												; 6th patch to prevent a rutine to test hdw for DW4 drives
			leau	$c170,pcr					; point to intercept
			ldx	#noHdw						; new added code
			sta	,u+							; put jmp
			stx	,u++							; put the new code address
			stb	,u								; put NOP
												; 7th patch for DOS reset routine
			leau	$c6c9,pcr					; point to intercept
			ldx	#dosReset					; new added code
			sta	,u+							; put jmp
			stx	,u++							; put the new code address
												; 8th patch to make BOOT command RSDOS capable (Mike Miller)
			leau	$c149,pcr					; point to patch location
			ldx	#rsbtchk				    	; address of new code in DPE
			sta	,u+							; patch jmp
			stx	,u++							; patch address

		  	rts									; return to caller
; ===============================================================================================================================

			IF drgrom > 0
				if (*%256 > 0)
						rzb	 256-(*%256)	; make it ROM compatible (length multiple of 256 bytes)
				endif
		 	ENDIF
; -------------------------------------------------------------------------------------------------------------------------------
absEnd	end
