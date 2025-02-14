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
                      ;                                                                        Mike Miller: Update size output to display large filesizes in SDIR, SDRIVE
                      ; DW42-0.25.14 - 2021-06-03 Pere Serrat: corrects RSboot upgrade
                      ; DW42-0.25.15 - 2021-06-04 Pere Serrat: adds cpyMPIcfg (copy of MPI config byte $ff7f)
                      ;                                                                                                         and modifies tstMPI to be fully compatible with mega-mini-MPI
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      
                                                                                                                              ; ROM routines
009F                  getNChr         equ     $9f                                                     ; get next char from input buffer
00A5                  redLChr         equ     $a5                                                     ; get last read char from input buffer
8000                  basini          equ     $8000                                                   ; basic rom beginning
8344                  syserr          equ   $8344                                                     ; system error
8371                  okprmpt         equ   $8371                                                     ; BASIC OK prompt, command loop
83ED                  basvect2        equ   $83ed                                                     ; initialize BASIC
841F                  basvect1        equ   $841f                                                     ; reset BASIC vectors, reset stack etc
849F                  runbasic        equ   $849f                                                     ; run_basic, enable IRQs etc
84ED                  romcmd          equ     $84ed                           ; rom entry to dispatch a tokenized command
89AA                  ckComma         equ     $89aa                                                   ; check for comma routine
89B4                  synerr          equ   $89b4                             ; rom entry to show syntax error
8B8D                  fcerr           equ   $8b8d                             ; entry to send message error
8E51                  asc2Num         equ     $8e51                                                   ; convert ASCII to number (1 byte)
8E83                  get16bN         equ   $8e83                                                     ; get a 16 bit number into x
90A5                  sendCR          equ     $90a5                                                   ; send a CR to screen
90E5                  outstr          equ   $90e5                                                     ; print string at X+1 to DEVNUM
957A                  outnum          equ   $957a                                     ; print number in d to devnum
9A89                  backspc         equ   $9a89                                                     ; print a backspace to DEVNUM
B3B4                  resetRt         equ     $b3b4                                                   ; reset routine
B44F                  wrmStrt         equ     $b44f                                                   ; warmStart routine
B54A                  outchr          equ   $b54a                                                     ; print char A to DEVNUM
B7AA                  getfnam         equ   $b7aa                                                     ; read file name and length into $1d1
B84B                  ioerror         equ   $b84b                                                     ; IO_error
BCAB                  outRegA         equ     $bcab                                                   ; output reg A to screen as number
C000                  romini          equ   $c000                                                     ; beginning of roms area 
C0F9                  gtbtsig                 equ     $c0f9                                                   ; get Boot signature
C14E                  rdbttrk                 equ     $c14e                                                   ; load disk track
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      
                                                                                                                              ; VARIABLES
0003                  ErasCtr         equ   $03                                                       ; sector count in eraser
0010                  noexec          equ   $10                                                       ; inhibit exec after loading
0011                  bootflag        equ   $11                                                       ; zero if run from command line, $55 if autorun on boot
0012                  dnum            equ   $12                                                       ; assigned drive number by dw4 server
0012                  tokens          equ   #18                               ; to be incremented each time words are added  =  NEW COMMANDS
001A                  newtok          equ   $1a                               ; this works with dos cartridge plugged in!
0050                  FPA0                    equ     $50                                                     ; number of bytes to flash
0052                  oldSlot         equ     $52                                                     ; FPA2 (re-used)
0053                  oldBank         equ     $53                                                     ; FPA3 (re-used)
                      
0076                  bufPtr          equ     $76                                                     ; (re-used) pointer to buffer end for new name
0076                  chksum          equ   $76                                                       ; checksum of the sector to be sent to dw4 (2 bytes)
0076                  dsktyp          equ     $76                                                     ; (re-used) only for SDRIVE creating file. 0=DSK, 1=VDK
0076                  dSource         equ     $76                                                     ; 2 bytes (re-used)
0076                  endadr          equ   $76                                                       ; last loaded address - probably no conflict but collides with  chksum  equ   $76       
0076                  hdwFlo          equ     $76                                                     ; slot number where floppies are found (only for tstHDW)
                      
0077                  hdwSdc          equ     $77                                                     ; slot number where SDC is found (only for tstHDW)
0086                  TargBank        equ   $86                                                       ; bank to be flashed
0089                  curlow          equ   $89                                                       ; low byte of cursor position
00B0                  usrptr          equ   $b0                               ; pointer to usr vector base
00E6                  bigfil          equ   $e6                                                       ; too big file flag - probably no conflict but collides with internal use of $e6 in format by dplus
00E7                  retry                   equ     $e7                                                     ; to catch 1st acess to DW4 drives error
00EB                  DCDRV                   equ     $eb                                                     ; bank number
00EE                  DCBPT                   equ     $ee                                                     ; pointer to subroutines
                      
00F8                  cpyPtr          equ     $f8                                                     ; (re-used) pointer to buffer end for SDIR
00F8                  numUni          equ     $f8                                                     ; unit number for new Commands (link, mount, bank)
00F8                  oriNum          equ     $f8                                                     ; number of requested slot-bank (re-used)
                      
00F9                  cpyWhat         equ     $f9                                                     ; command that identifies BANK - SLOT (re-used)
00F9                  numDir          equ     $f9                                                     ; number of received dirs (re-used)
00F9                  sdcSlot         equ     $f9                                                     ; 1 byte (re-used)
00F9                  swMap1          equ     $f9                                                     ; flag to switch to map1 (re-used)
00F9                  trknum          equ     $f9                                                     ; tracknumber in process for DSKINIT 4th patch
                      
00FA                  dDest                   equ     $fa                                                     ; 2 bytes (re-used)
00FA                  geodat          equ     $fa                                                     ; numTracks - numSect x track (2 bytes)
00FA                  itsRead         equ     $fa                                                     ; number of received items in a dir page (re-used)
00FA                  valPar          equ     $fa                                                     ; (re-used) value for parameter to send into B
                      
00FB                  cmdNum          equ     $fb                                                     ; (re-used) command code to be sent
00FB                  itsDone         equ     $fb                                                     ; number of items shown of a page
00FB                  secxtrk         equ     $fb                                                     ; numSect x track (low byte of previous word)
                      
00FC                  drvtab          equ     $fc                                                     ; system table for linked numbers to the 4 units (fc-fd-fe-ff)
0114                  floSDC          equ     $114                                                    ; bit 0 for unit 1, bit 1 for unit 2. If 0 use Floppy, if 1 use SDC
                                                                                                                                      ; bit 2 is 1 if Floppies present else 0
                                                                                                                                      ; bit 3 is 1 if SDC present else 0
                                                                                                                                      ; bit 5-4 show the slot where floppies are connected (0-3)
                                                                                                                                      ; bit 7-6 show the slot where SDC is connected (0-3)
0115                  idxHD                   equ     $115                                                    ; index to a HD array (maximum 90 discs of 720+1 sectors DW4 VDK type) (values 1-90)
0134                  newstb          equ   $134                              ; base of new stub
015E                  hokini          equ   $15e                                                      ; first system hook
01A8                  hokend          equ   $1a8                                                      ; last system hook
01CD                  sstack          equ   $1cd                                                      ; backup of original stack
01CF                  sector          equ   $1cf                                                      ; counter, used when loading file contents, just below namebuf/DECB header
                      
01D1                  nambuf          equ   $1d1                                                      ; fixed by BASIC, read filename function which stores in $1d1
01D1                  namebuf         equ   $1d1                                                      ; fixed by BASIC read filename function
                      
01D1                  decbhdr         equ   namebuf                                           ; DECB headers get copied in here
01D9                  startbuf        equ   namebuf+8                                 ; dw input data buffer, can reuse buffer after DECB header slot
02D9                  endbuf          equ   startbuf+256                              ; end of buffer area
03D9                  fST16                   equ     $3d9                                                    ; first sector of Track 16 (backup DIR)
03EA                  newusr          equ     $3ea                                                    ; will use $3ea - $3fd = 20 bytes (New USR area)
03FF                  cpyMPIcfg       equ     $03ff                                                   ; candidate to be a copy in RAM of $ff7f byte ($3EB - $3FF seem safe places ...)
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      
                                                                                                                              ; RAM area
3000                  ramini          equ   $3000                                                     ; where to copy the rom
6EFF                  rammax          equ   $6eff                                                     ; ram limit to send to highram (16k - 256 bytes) as a rom program to be run
6FFF                  rmaxld          equ   $6fff                                                     ; ram limit to load into low memory (for basic roms)
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      
                                                                                                                              ; SAM area 
FFD8                  R1CLR                   equ     $ffd8                                                   ; to clear fast poke $ffd9
FFDE                  RAMROM          equ     $ffde                                                   ; to go MAP0, resetting $ffdf (MAP1)
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      
                                                                                                                              ; HDW related addresses
FF20                  BBOUT                   equ     $ff20                                                   ; used for DW4
FF20                  PIA1Base        equ   $ff20                                                     ; used by DWRead/DWWrite
FF22                  BBIN                    equ     $ff22                                                   ; used for DW4
FF40                  CMDREG          equ   $ff40                                             ; command register (write)
FF40                  STATREG         equ   $ff40                                             ; status register (read)
FF41                  PREG1           equ   $ff41                                             ; param register 1
FF42                  PREG2           equ   $ff42                                             ; param register 2
FF42                  DATREGA         equ     PREG2                              ; first data register
FF43                  PREG3           equ   $ff43                                                     ; param register 3
FF43                  DATREGB         equ   PREG3                                ; second data register
FF48                  CTRLATCH        equ   $ff48                                             ; controller latch (write)
FF4A                  FLSHREG         equ   $ff4a                                                     ; Flash Register in SDC
FF4B                  BANKREG         equ   $ff4b                                                     ; Bank select in SDC
                      ;                                                                                                       HIGH nibble = ROM        (/CTS)
                      ;                                                                                                       LOW  nibble = HARDWARE   (/SCS)
FF7F                  MPIcfg          equ     $ff7f                                                   ; value used in Tandy MultiPacks to save slots used (/CTS and /SCS)
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      
                                                                                                                              ; CONSTANTS 
                                                                                                                              ; Status Register Masks
0001                  BUSY            equ   %00000001                            ; set while a command is executing
0002                  READY           equ   %00000010                            ; set when ready for a data transfer
0080                  FAILsdc         equ   %10000000                            ; set on command failure
                                                                                                                              ; Mode and Command Values
0000                  destPtr         equ     0                                                               ; ofsset to get to the pointer to destination buffer (2 bytes)
000B                  CMDMODE         equ   $0b                                                       ; was $43 for CoCo - control latch value to enable command mode
0050                  dimask          equ   $50                                                       ; disable IRQ and FIRQ
0050                  IntMasks        equ   $50                                                       ; used by DWRead/DWWrite
0080                  CMDREAD         equ   $80                                  ; read logical sector
00A0                  CMDWRITE        equ   $a0                                  ; write logical sector
00AF                  eimask          equ   $af                                                       ; enable interrupts
00C0                  CMDEX           equ   $c0                                  ; extended command
00D2                  OP_READEX       equ   'R'+$80                                           ; $D2 - Read one sector (DW4 opcode)
00E0                  CMDEXD          equ   $e0                                  ; extended command with data block
00F2                  OP_REREADEX     equ   'r'+$80                                           ; $F2 - Re-read one sector (DW4 opcode)
00F3                  E_CRC           equ   $f3                               ; Same as NitrOS-9 E$CRC
                      ; ===============================================================================================================================
                      
                                                                                                                      ; CODE BEGINNING
                            IF drgbin > 0                                                     ; to create Patcher - Extender binaries
DFF7                           org   $e000-9                                          ; to create the BIN header
DFF7  55                                      fcb     #$55                                                    ; header values for a Dragon binary file. magic 1st byte
DFF8  02                                      fcb     #$02                                                    ; file type
DFF9  6000                                    fdb     #$6000                                          ; load
DFFB  1BE7                                    fdb     #absEnd-$e000                           ; length
DFFD  6002                                    fdb     #$6002                                          ; exec
DFFF  AA                                      fcb     #$aa                                                    ; magic end byte
                                      ELSE
                                              org     $e000                                                   ; real program beginning
                               put   $e000                                                    ; allocated at $e000
                            ENDIF
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      
                                                                                                                      ; PROGRAM INIT
E000  4549            mark            fcb     $45,$49                                         ; "EI" mark to signal to the DOS that this extension wants to be called to initialize itself
E002  7EE58F                                  jmp     initext                                         ; initialize extension
E005  161B7F                                  lbra    reptch                                          ; direct jump to DOSPlus patcher
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Entry point for DOS command dispatcher. 1st patch
                      ; Data received:
                      ;       A = track number
                      ;       B = command code  (2=READ - 3=Unknown - 4=WRITE - 7=VERIFY(dummy read)
                      ;       X = address of system variable "number of sectors x track" for this drive 
                      ;       Y = LSN
                      ;       U = sector counter (if not directory track) 
                      ; -------------------------------------------------------------------------------------------------------------------------------
E008  3402            entry    pshs  a                                                                ; save register 'A' (code overwritten. It's the first instruction in the DOS)
E00A  3401                                    pshs  cc                                                                ; save flags
E00C  8D0F                                    bsr     phyvir                                          ; detect physical or virtual drive
E00E  241E                                    bcc     dw4                                                     ; if greater than 4 is a DriveWire slot number
E010  4D                                      tsta                                                                    ; is linked value equal 0? - demanat per KenH
E011  271B                                    beq     dw4                                                     ; yes, so DriveWire slot 0
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; The DOS part respects the original code
                      ; -------------------------------------------------------------------------------------------------------------------------------
E013  3501            todos           puls    cc                                                              ; restore flags 
E015  170CA1                                  lbsr    flOrSd                                          ; control access to floppy or SDC
E018  8602                                    lda   #$02                                      ; get value #2 (number of retries). It's the second instruction in the DPlus49b
E01A  7EC10A                                  jmp     $c10a                                                   ; back to next sentence of DOS original code
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Detects if drive is physical or virtual (code in A)
                      ; returns Carry set if DW4 slot number detected (now any value greater than 4)
                      ; must use extended addressing because for DSKINIT, DP arrives here with $FF and if not virtual should return like that
                      ; -------------------------------------------------------------------------------------------------------------------------------
E01D  3410            phyvir  pshs    x                                                               ; save working register
E01F  8E00FB                                  ldx     #drvtab-1                                       ; point to 1 byte before the slots table
E022  B600EB                                  lda     >$00eb                                          ; get drive number
E025  A686                                    lda     a,x                                                     ; get slot number for that drive
E027  B70012                                  sta     >dnum                                                   ; put it into DW4 variable
E02A  8105                                    cmpa    #5                                                              ; is a DW4 number? - if not, Carry will be set
E02C  3590                                    puls    x,pc                                                    ; restore register and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; The dw4 part distinguishes between READ, VERIFY and WRITE opcodes
                      ; -------------------------------------------------------------------------------------------------------------------------------
E02E  3501            dw4     puls    cc                                                              ; get saved flags
                                                                                                                      ; to avoid problems created by SOUND and PLAY over DW4
E030  3402                                    pshs    a                                                               ; save register
E032  8602                                    lda     #2                                                              ; get value to mark output
E034  B7FF20                                  sta     BBOUT                                                   ; to DW4
E037  4F                                      clra                                                                    ; set counter
E038  4A              dw4L01  deca                                                                    ; decrement counter
E039  26FD                                    bne     dw4L01                                          ; not zero, loopback
E03B  3502                                    puls    a                                                               ; restore register
E03D  3474                                    pshs  b,x,y,u                                           ; save rest of registers (A was already onto stack) before recalculatimg Y
E03F  1A50                                    orcc  #dimask                                           ; disable interrupts to work with dw4                                                   
E041  8D4A                                    bsr     calcy                                                   ; do not relay on Y. Recalculate it from $eb-ec-ed
E043  8D68                                    bsr     ctrlHD                                          ; control index in HD if needed
E045  3021                                    leax  1,y                               ; X points now to the rigth dw4 sector (skipping the header) - [DW needs registers X-Y inverted!)
E047  109EEE                                  ldy   <$ee                                              ; Y points to RAM buffer address
E04A  C102                    cmpb  #2                                                                ; is this a read command?
E04C  270F                                    beq   reddw4                                            ; yes, read sector from dw4
E04E  C107                    cmpb  #7                                                                ; is this a verify command?
E050  2606                                    bne   wrtdw4                                            ; no, write sector to dw4
E052  108EFCFF                                ldy     #$fcff                                          ; initial byte of 256 area to be overwritten while verifying
E056  2005                                    bra     reddw4                                          ; go read that sector
E058  BDE2C7          wrtdw4  jsr   dowrit                                            ; write a sector from buffer to dw4
E05B  2010                                    bra   verify                                            ; verify operation result
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; This part asks dw4 a sector to be read and dw4 log reflects it accordingly. It is as simple as possible.
                      ; -------------------------------------------------------------------------------------------------------------------------------
E05D  BDFA84          reddw4  jsr   DoRead                                            ; call dw4 to read one sector
E060  250B                                    bcs     verify                                          ; if operation error, give message
E062  2609                                    bne     verify                                          ; if incompleted, give message
E064  E6E4                                    ldb     ,s                                                              ; get pushed B (opcode / command)
E066  C102                                    cmpb    #2                                                              ; is it read?
E068  261F                                    bne     jstvfy                                          ; no, adapt flags and exit
E06A  BDE20D                                  jsr     correct                                         ; correct LSN if needed
E06D  3574            verify  puls  b,x,y,u                                      ; restore received registers
E06F  3502                                    puls  a                                                         ; and first one pushed
E071  2506                                    bcs   failed                                            ; detect operation error
E073  2604                    bne   failed                                            ; detect incomplete read operation
E075  5F                                      clrb                                                                    ; operation succesful - no error code returned in B
E076  1CAF            outgo           andcc #eimask                                           ; enable interrupts
E078  39                                      rts                                                                     ; return to caller
E079  0DE7            failed   tst    <retry                                          ; is flag retry ON ($01)?
E07B  2706                                    beq     fail01                                          ; no, exit
E07D  2B04                                    bmi     fail01                                          ; if negative exit too
E07F  00E7                                    neg     <retry                                          ; to get value $FF
E081  2085                                    bra     entry                                                   ; try it again
E083  C6A6            fail01  ldb   #$a6                                                      ; set error code (unknown = ??)
E085  1A01                                    orcc    #%00000001                                      ; set Carry flag (error)
E087  20ED                                    bra   outgo                                                     ; return
E089  1A04            jstvfy  orcc    #%00000100                                      ; set flag Z=1          
E08B  20E0                                    bra     verify                                          ; exit via verify
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; This is used to calculate the LSN number from system variables $ec (track) and $ed (sector)
                      ; Controls the number of sides of the disks to change 18 s/t to 36
                      ; returns the calculated LSN in register Y
                      ; -------------------------------------------------------------------------------------------------------------------------------
E08D  3416            calcy           pshs    d,x                                                     ; save working registers
E08F  8E06A6                                  ldx     #$6a6                                                   ; point to byte before sectors x track table
E092  D6EB                                    ldb     <$eb                                                    ; get drive number
E094  A685                                    lda     b,x                                                     ; get number of sectors x track for this drive
E096  D6EC            calcy01 ldb     <$ec                                                    ; get track number
E098  3D                                      mul                                                                     ; calculate sectors amount
E099  1F01                                    tfr     d,x                                                     ; X gets this value
E09B  96ED                                    lda     <$ed                                                    ; get sector number
E09D  4A                                      deca                                                                    ; decrement
E09E  3186                                    leay    a,x                                                     ; add to X and pass result to Y
E0A0  3596                                    puls    d,x,pc                                          ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Aternative entry for DSKINIT that comes without data in $6a7-8-9-a. So this one relies on system variable $f4 (as numSides)
                      ; -------------------------------------------------------------------------------------------------------------------------------
E0A2  3416            caldi           pshs    d,x                                                     ; save working registers
E0A4  8612                                    lda     #$12                                                    ; sectors x track (single side)
E0A6  0DF4                                    tst     <$f4                                                    ; is disk single sided?
E0A8  2701                                    beq     ecaldi                                          ; yes, skip next
E0AA  48                                      asla                                                                    ; double number of sectors x track
E0AB  20E9            ecaldi  bra     calcy01                                         ; jump to calculation routine
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; to apply index to a DW4 HD on drive 4 if it is the case
                      ; -------------------------------------------------------------------------------------------------------------------------------
E0AD  3406            ctrlHD  pshs    a,b                                                     ; save register
E0AF  D6EB                                    ldb     <$eb                                                    ; get drive number
E0B1  C104                                    cmpb    #4                                                              ; is it 4?
E0B3  2613                                    bne     eCtrl                                                   ; no, exit
E0B5  F60115                                  ldb     idxHD                                                   ; get index of disc to be accesed
E0B8  C101                                    cmpb    #1                                                              ; is index 0 or 1?
E0BA  230C                                    bls     eCtrl                                                   ; yes, no offset
E0BC  5A                                      decb                                                                    ; else deduct one
E0BD  31A5                                    leay    b,y                                                     ; Y = LSN + index
E0BF  86F0                                    lda     #240                                                    ; maximum acceptable value that can be used to multiply the index
E0C1  3D                                      mul                                                                     ; D = 240 * Index  (if index=90, result 21360 -> $5370) always will be positive
E0C2  31AB                                    leay    d,y                                                     ; Y = LSN + 241*index
E0C4  31AB                                    leay    d,y                                                     ; Y = LSN + 481*index
E0C6  31AB                                    leay    d,y                                                     ; Y = LSN + 721*index  now we have calculated the real LSN to be used, no loops!
E0C8  3586            eCtrl           puls    a,b,pc                                          ; restore register and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Process that substitutes the calls to "WriteTrack". It is used ONLY by DSKINIT
                      ; Writes the 18 sectors of one side in a loop
                      ; If sector number received is 0, will write to side 1, else ($ff) will do side 2 
                      ; -------------------------------------------------------------------------------------------------------------------------------
E0CA  3401            inidsk  pshs    cc                                                              ; save flags
E0CC  BDE01D                                  jsr     phyvir                                          ; is drive physical or virtual?
E0CF  240B                                    bcc     virtual                                         ; virtual, process it
E0D1  4D                                      tsta                                                                    ; is linked value 0?
E0D2  2708                                    beq     virtual                                         ; yes, so DW4 slot 0
E0D4  3501                                    puls    cc                                                              ; get saved flags
E0D6  CC47F4                                  ldd   #$47f4                                            ; physical, do nothing. This was overwritten by the patch
E0D9  7EC328                                  jmp     $c328                                                   ; back to DOS
                                                                                                                      ; to avoid problems created by SOUND and PLAY over DW4
E0DC  0DEC            virtual tst     <$ec                                                    ; is track 0?
E0DE  260D                                    bne     iniD01                                          ; no, skip section
E0E0  3402                                    pshs    a                                                               ; save register
E0E2  8602                                    lda     #2                                                              ; get value to mark output
E0E4  B7FF20                                  sta     BBOUT                                                   ; to DW4
E0E7  4F                                      clra                                                                    ; set counter
E0E8  4A              iniL01  deca                                                                    ; decrement counter
E0E9  26FD                                    bne     iniL01                                          ; not zero, loopback
E0EB  3502                                    puls    a                                                               ; restore register
E0ED  3501            iniD01  puls    cc                                                              ; get saved flags
E0EF  1A50                                    orcc  #dimask                                           ; disable interrupts to work with dw4   
E0F1  4F                                      clra                                                                    ; value 0
E0F2  1F8B                                    tfr     a,dp                                                    ; set direct page to 0
E0F4                                          setdp   0                                                               ; notify compiler
E0F4  3476                                    pshs    d,x,y,u                                         ; save registers
E0F6  8D42                                    bsr     filbuf                                          ; fill buffer
E0F8  D6EC                                    ldb     <$ec                                                    ; get track number
E0FA  C114                                    cmpb    #$14                                                    ; is this the directory (T20 track)?
E0FC  2607                                    bne     wrt00                                                   ; no, init the track
E0FE  0DF3                                    tst     <$f3                                                    ; yes, is it side 2? (0 means side 1)
E100  2603                                    bne     wrt00                                                   ; init side 2!
                                                                                                                      ; yes, don't waste time, it will be filled later
E102  4F                                      clra                                                                    ; mark no error
E103  2025                                    bra     eIni01                                          ; exit
E105  4C              wrt00           inca                                                                    ; 1st track of side 1
E106  0DF3                                    tst     <$f3                                                    ; is it side 1?
E108  2702                                    beq     wrt01                                                   ; yes, skip next one
E10A  8613                                    lda     #19                                                     ; get 1st track of side 2
E10C  97ED            wrt01           sta     <$ed                                                    ; update system variable
E10E  8D92                                    bsr     caldi                                                   ; calculate first LSN to write to (into Y)
E110  8D9B                                    bsr     ctrlHD                                          ; control index of HD if needed
E112  3021                                    leax  1,y                               ; X points to the rigth dw4 sector (skipping the header) - [DW needs them inverted!)
E114  109EEE                                  ldy   <$ee                                              ; Y points to RAM buffer address
E117  C612                                    ldb     #18                                                     ; number of sectors to be initialized
E119  E7E2                                    stb     ,-s                                                     ; put counter in stack
E11B  BDE2C7          wrtmor  jsr     dowrit                                          ; write sector to DW4 
E11E  2510                                    bcs   failed2                                           ; detect operation error
E120  260E                    bne   failed2                                           ; detect incomplete read operation
E122  3001                                    leax    1,x                                                     ; point to next LSN
E124  6AE4                                    dec     ,s                                                              ; dec counter
E126  26F3                                    bne     wrtmor                                          ; if not yet 0, do next one
E128  3261                                    leas    1,s                                                     ; get rid of counter
E12A  3576            eIni01  puls    d,x,y,u                                    ; restore registers (0 will be pulled for A)
E12C  4D              eIniDsk tsta                                                                    ; update flags 
E12D  1CAF                                    andcc #eimask                                           ; enable interrupts
E12F  39                                      rts                                                                     ; return
E130  3261            failed2 leas    1,s                                                     ; get rid of counter
E132  3576                                    puls    d,x,y,u                                    ; restore registers
E134  86FF                                    lda     #$ff                                                    ; mark error for Dskinit function
E136  1A01                               orcc #$01                                                    ; set Carry flag (error)
E138  20F2                                    bra   eIniDsk                                           ; return to caller
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Fill the buffer pointed by $ee all with value $e5
                      ; -------------------------------------------------------------------------------------------------------------------------------
E13A  3454            filbuf  pshs    b,x,u                                                   ; save registers
E13C  9EEE                                    ldx     <$ee                                                    ; get buffer address
E13E  CEE5E5                                  ldu     #$e5e5                                          ; value to put
E141  8D32                                    bsr     cpu2xb                                          ; put 128 times U at X
E143  35D4                                    puls    b,x,u,pc                                                ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Fill the directory track T20 for DW4 and floppy as well
                      ; Only used by DSKINIT, but to be sure, it controls a return address in the stack to work else returns
                      ; -------------------------------------------------------------------------------------------------------------------------------
E145  3446            fildir  pshs    d,u                                                     ; save working registers
E147  CCC9EF                                  ldd     #$c9ef                                          ; return address when called to write directory tracks by DSKINIT
E14A  10A364                                  cmpd    4,s                                                     ; is it the write track call?
E14D  2607                                    bne     efildir                                         ; not the write track command, skip fill subroutines
E14F  8D0D                                    bsr     fil16s                                          ; fill buffer for the 16 Dir sectors
E151  8D2A                                    bsr     ffat1                                                   ; fill buffer for FAT 1st sector
E153  BDE1D2                                  jsr     ffat2                                                   ; fill buffer for FAT 2nd sector
E156  3546            efildir puls    d,u                                                     ; restore registers
E158  8E0800                                  ldx     #$800                                                   ; overwritten line by interceptor
E15B  7EC9F6                                  jmp     $c9f6                                                   ; back to DOS
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Fill the buffer for the 16 directory sectors with 10 unused entries
                      ; -------------------------------------------------------------------------------------------------------------------------------
E15E  8E0A00          fil16s  ldx     #$a00                                                   ; point to third buffer
E161  DE8A                                    ldu     <$8a                                                    ; get 16 bits zero
E163  8D10                                    bsr     cpu2xb                                          ; put 128 times U at X
E165  8689                                    lda     #$89                                                    ; code to mark not used entry
E167  8E0A00                                  ldx     #$a00                                                   ; point to the beginning again
E16A  A784            fild02  sta     ,x                                                              ; mark an entry
E16C  308819                                  leax    25,x                                                    ; point to next one
E16F  8C0AE8                                  cmpx    #$ae8                                                   ; done 10 entries?
E172  25F6                                    blo     fild02                                          ; no, loop back
E174  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Put value in U in bytes pointed by X, B times (each time a word)
                      ; -------------------------------------------------------------------------------------------------------------------------------
E175  C680            cpu2xb  ldb     #128                                                    ; number of words to write
E177  EF81            cp01            stu     ,x++                                                    ; write one
E179  5A                                      decb                                                                    ; decrement counter
E17A  26FB                                    bne     cp01                                                    ; if not 0, loopback
E17C  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Fill the first FAT sector. Must mark as used only the 18 sectors of Tracks 20 and 16 if applies
                      ; -------------------------------------------------------------------------------------------------------------------------------
E17D  8E0800          ffat1           ldx     #$800                                                   ; point to first buffer
E180  96F2                                    lda     <$f2                                                    ; get number of tracks  
E182  C612                                    ldb     #$12                                                    ; sectors x track (single side)
E184  0DF4                                    tst     <$f4                                                    ; is disk single sided?
E186  2701                                    beq     ffat100                                         ; yes, skip next
E188  58                                      aslb                                                                    ; double number of sectors x track
E189  DDFA            ffat100 std     <geodat                                         ; save geometry data for later use
E18B  C65A                                    ldb     #90                                                     ; there are 180 FAT bytes, so 90 words, but 180k discs need only HALF that!
E18D  DEFA                                    ldu     <geodat                                         ; get geometry data
E18F  11832812                                cmpu    #$2812                                          ; is it a Single Sided Single Density disk (180k)?
E193  2601                                    bne     ffat101                                         ; no, skip next
E195  56                                      rorb                                                                    ; yes, divide by 2 to get 45 words
E196  CEFFFF          ffat101 ldu     #$ffff                                          ; code to mark 16 free sectors
E199  8DDC                                    bsr     cp01                                                    ; copy B times U at X
E19B  3341                                    leau    1,u                                                     ; create 0 value
E19D  EF81            ffat102 stu     ,x++                                                    ; clear a word
E19F  8C08FC                                  cmpx    #$900-4                                         ; reached end of buffer minus 4 bytes to put geometry data there?
E1A2  25F9                                    blo     ffat102                                         ; no, loopback
E1A4  DCFA                                    ldd     <geodat                                         ; get geometry data
E1A6  ED81            ffat103 std     ,x++                                                    ; save it in 1st FAT sector
E1A8  43                                      coma                                                                    ; complement
E1A9  53                                      comb                                                                    ; both bytes
E1AA  ED84                                    std     ,x                                                              ; store again in FAT
E1AC  C624                                    ldb     #36                                                     ; This the FAT byte where dir T16 begins for single sided discs
E1AE  96FB                                    lda     <secxtrk                                                ; get sectors x track
E1B0  8112                                    cmpa    #$12                                                    ; is it single?
E1B2  2701                                    beq     ffat104                                         ; yes, skip next
E1B4  58                                      aslb                                                                    ; double to 72 (the same place for Double Side disks)
E1B5  3404            ffat104 pshs    b                                                               ; save value for later use
E1B7  86F0                                    lda     #$f0                                                    ; flag value for parameter '/' (leave T16 free for data storage)
E1B9  91E6                                    cmpa    <$e6                                                    ; has $e6 this value?
E1BB  2702                                    beq     ffat105                                         ; yes, don't mark sector T16 as used                                                            
E1BD  8D05                                    bsr     markfat                                         ; mark used sectors of T16
E1BF  D6FB            ffat105 ldb     <secxtrk                                                ; get sectors x track
                                                                                                                      ; distance between T16 and T20 is 4 * tracks * N_Sxt / 8 bits = NSxt / 2
E1C1  56                                      rorb                                                                    ; convert distance to FAT bytes to add to
E1C2  EBE0                                    addb    ,s+                                                     ; the previously pushed FAT byte for T16, clear stack
E1C4  8E0800          markfat ldx     #$800                                                   ; point to the begining of the FAT
E1C7  3085                                    leax    b,x                                                     ; add offset to 1st DIR sector byte of that track
E1C9  4F                                      clra                                                                    ; 16 bit zero
E1CA  5F                                      clrb                                                                    ; to mark 16
E1CB  ED81                                    std     ,x++                                                    ; used sectors by DIR track
E1CD  86FC                                    lda     #%11111100                                      ; and two zero bits more
E1CF  A784                                    sta     ,x                                                              ; to complete the 18 reserved sectors for any directory Track
E1D1  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Fill the second FAT sector. This is only used for 720k discs
                      ; -------------------------------------------------------------------------------------------------------------------------------
E1D2  8E0900          ffat2           ldx     #$900                                                   ; point to second buffer
E1D5  DCFA                                    ldd     <geodat                                         ; get geometry data
E1D7  10835024                                cmpd    #$5024                                          ; is it a 720K disc?
E1DB  2607                                    bne     ffat201                                         ; no, do not put $FFs
E1DD  CEFFFF                                  ldu     #$ffff                                          ; to mark free sectors
E1E0  C65A                                    ldb     #90                                                     ; must mark 180 bytes
E1E2  8D93                                    bsr     cp01                                                    ; copy B times U at X
E1E4  DE8A            ffat201 ldu     <$8a                                                    ; get 16 bits zero
E1E6  EF81            ffat202 stu     ,x++                                                    ; put 2 zero bytes
E1E8  8C0A00                                  cmpx    #$a00                                                   ; reached end of buffer?
E1EB  25F9                                    blo     ffat202                                         ; no, loopback
E1ED  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Keep the good track number to avoid strange sequences. Called by DSKINIT and
                      ; called from other points, one of them the COPY routine and from SAVE as well ...
                      ; Has a control to detect that it is called from DSKINIT, else do nothing at all!
                      ; when applied to DW4 the track number suffers non lineal sequences. Here we correct that
                      ; -------------------------------------------------------------------------------------------------------------------------------
E1EE  3406            modtrk  pshs    d                                                               ; save register
E1F0  CCC9E1                                  ldd     #$c9e1                                          ; the return address that identifies DSKINIT
E1F3  10A367                                  cmpd    7,s                                                     ; is the one corresponding to DSKINIT?
E1F6  2610                                    bne     notdsk                                          ; no, do nothing
E1F8  96EC                                    lda     <$ec                                                    ; get track number
E1FA  97F9                                    sta     <trknum                                         ; save in variable
E1FC  BDD36B                                  jsr     $d36b                                           ; (buf_read_sector) - this line was overwritten by patch
E1FF  96F9                                    lda     <trknum                                         ; get saved track number
E201  97EC                                    sta     <$ec                                                    ; back to its place
E203  3506            emodtrk puls    d                                                               ; restore register
E205  7ED1A7                                  jmp     $d1a7                                                   ; back to DOS
E208  BDD36B          notdsk  jsr     $d36b                                                   ; (buf_read_sector) - this line was overwritten by patch
E20B  20F6                                    bra     emodtrk                                         ; clean stack and back to DOS
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Part to detect a bigger than 180k disc and update tables accordingly. The same if disc was changed
                      ; If read sector has no geometry data, we go to the other possible localization for directory ($168 - $2D0) (+1 for DW4)
                      ; X is updated to the new LSN to be read, if necessary
                      ; must be avoided ONLY when working with an HD at unit 4 (idxHD $115 >1)
                      ; -------------------------------------------------------------------------------------------------------------------------------
E20D  3466            correct pshs    d,y,u                                                   ; save registers
E20F  96EB                                    lda     <$eb                                                    ; get unit number
E211  8104                                    cmpa    #4                                                              ; is it #4?
E213  2605                                    bne     cor00                                                   ; no, skip control
E215  7D0115                                  tst     idxHD                                                   ; working on a HD in a disk with index > 0?
E218  2639                                    bne     ecorok                                          ; yes, exit
E21A  DCEC            cor00           ldd     <$ec                                                    ; get track-sector numbers
E21C  10831401                                cmpd    #$1401                                          ; is it 1st FAT sector?
E220  2631                                    bne     ecorok                                          ; no, exit
E222  CCC869                                  ldd     #$c869                                          ; get return address from BACKUP command
E225  10A3E814                                cmpd    20,s                                                    ; is this the case?
E229  2728                                    beq     ecorok                                          ; yes, exit
E22B  DEEE                                    ldu     <$ee                                                    ; get read buffer address
E22D  ECC900FE                                ldd     $fe,u                                                   ; get complemented geodata bytes
E231  43                                      coma                                                                    ; complement both bytes
E232  53                                      comb                                                                    ; A=tracks; B=sectors x track ... possible values: 2812 - 2824 - 5012 - 5024
                                                                                                                              ; their 1st FAT sector will equal that one:        0169 - 02d1 - 0169 - 02d1 (+1 due to DW4) 
E233  10A3C900FC                              cmpd    $fc,u                                                   ; do they equal the not complemented ones?
E238  2717                                    beq     ecorok1                                         ; yes, we have a right FAT sector, update system tables and exit
                                                                                                                              ; Bad 1st FAT sector read, we must change single <--> double as required
E23A  8C0169                                  cmpx    #$0169                                          ; was sector read $0169 (single sided)
E23D  2605                                    bne     cor01                                                   ; no, change to single sided
E23F  8E02D1                                  ldx     #$02d1                                          ; yes, change to double sided
E242  2003                                    bra     cor02                                                   ; go save new 1st FAT LSN
E244  8E0169          cor01           ldx     #$0169                                          ; get single sided value
E247  8D10            cor02           bsr     updtab                                          ; update system tables (in some cases they are not yet filled)
E249  109EEE                                  ldy     <$ee                                                    ; get buffer address
E24C  BDFA84                                  jsr     DoRead                                          ; read newly calculated T20 1st FAT LSN
E24F  2006                                    bra     ecor                                                    ; exit
E251  8D06            ecorok1 bsr     updtab                                          ; update system tables
E253  1A04            ecorok  orcc    #%00000100                                      ; set flag Z=1, result OK
E255  1CFE                    andcc   #%11111110                                      ; clear Carry flag, no errors
E257  35E6            ecor            puls    d,y,u,pc                                                ; restore registers and return                  
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Updates system tables using param X as LSN for 1st DIR track (should get here with DW4 value)
                      ; Affects system tables at $61c (dir LSN) and $6a7 (sectors x track)
                      ; -------------------------------------------------------------------------------------------------------------------------------
E259  CE0616          updtab  ldu     #$616                                                   ; point 6 bytes before drives info table                        
E25C  D6EB                                    ldb     <$eb                                                    ; get drive number
E25E  8606                                    lda     #6                                                              ; register length
E260  3D                                      mul                                                                     ; calculate offset
E261  311F                                    leay    -1,x                                                    ; system works with $0168 or $02d0 (DW4 adds one because of the header)
E263  10AFCB                                  sty     d,u                                                     ; put LSN at requested drive table
E266  8612                                    lda     #$12                                                    ; default value for single sided
E268  8C0169                                  cmpx    #$0169                                          ; is new value single sided?
E26B  2701                                    beq     cor03                                                   ; yes, skip next
E26D  48                                      asla                                                                    ; double value
E26E  CE06A6          cor03           ldu     #$6a6                                                   ; point 1 byte before sectors x track table
E271  D6EB                                    ldb     <$eb                                                    ; get drive number
E273  A7C5                                    sta     b,u                                                     ; update value in table for that drive
E275  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Updates table $6a6 if drive is virtual. Uses data from stack (only BACKUP)
                      ; -------------------------------------------------------------------------------------------------------------------------------
E276  3416            patbck  pshs    d,x                                                     ; save registers
E278  8E00FB                                  ldx     #drvtab-1                                       ; point to 1 byte before the slots table
E27B  A6C4                                    lda     ,u                                                              ; get source drive from stack hole
E27D  8D0C                                    bsr     updsxt                                          ; update sectors x track
E27F  A649                                    lda     9,u                                                     ; get destination drive from stack hole
E281  8D08                                    bsr     updsxt                                          ; update sectors x track
E283  3516                                    puls    d,x                                                     ; restore registers
E285  A648                                    lda     8,u                                                     ; these 2 sentences were
E287  3D                                      mul                                                                     ; overwritten by patcher
E288  7EC839                                  jmp     $c839                                                   ; back to DOS
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Updates sectors x track of that drive (comes in A)
                      ; -------------------------------------------------------------------------------------------------------------------------------
E28B  E686            updsxt  ldb     a,x                                                     ; get associated slot number in drives table
E28D  2704                                    beq     upds01                                          ; if cero is a special DW4 case, process it
E28F  C104                                    cmpb    #4                                                              ; is it lower than 5?
E291  230B                                    bls     eupd                                                    ; yes, is a physicall drive, do not process it
E293  3410            upds01  pshs    x                                                               ; save register
E295  8E06A6                                  ldx     #$6a6                                                   ; point 1 byte before sectors x track table
E298  E648                                    ldb     8,u                                                     ; get number of sectors x track in stack hole
E29A  E786                                    stb     a,x                                                     ; into table for that drive
E29C  3510                                    puls    x                                                               ; restore register
E29E  39              eupd            rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; This part avoids calling a hardware routine when the drive is virtual
                      ; and when floppies allow it, in order to avoid that the FDC floppy light stays on forever
                      ; -------------------------------------------------------------------------------------------------------------------------------
E29F  3403            noHdw           pshs    a,cc                                                    ; save A and flags
E2A1  BDE01D                                  jsr     phyvir                                          ; detect drive type
E2A4  240F                                    bcc     retHdw                                          ; if virtual, return
E2A6  4D                                      tsta                                                                    ; is linked value 0?
E2A7  270C                                    beq     retHdw                                          ; yes, so DW4 slot 0
E2A9  3503                                    puls    a,cc                                                    ; restore A and flags
                                                                                                                      ; so it goes for floppies or SDC
E2AB  170A0B                                  lbsr    flOrSd                                          ; will take care of destination hdw                     
E2AE  1A50                                    orcc  #dimask                                           ; first overwritten instruction
E2B0  66E4                                    ror     ,s                                                         ; second overwritten instruction
E2B2  7EC174                                  jmp     $c174                                                   ; back to DOS
E2B5  0DE7            retHdw  tst     <retry                                          ; is flag off (0)?
E2B7  2602                                    bne     ret01                                                   ; no, skip next
E2B9  0CE7                                    inc     <retry                                          ; turn flag ON ($01)
E2BB  3262            ret01           leas    2,s                                                     ; get rid of 2 pushed regs
E2BD  66E4                                    ror     ,s                                                              ; get rid of pushed carry value
E2BF  35FB                                    puls    a,cc,dp,x,y,u,pc                        ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; This intercepts the std DOS reset routine
                      ; -------------------------------------------------------------------------------------------------------------------------------
E2C1  170308          dosReset lbsr   initask                                         ; put initial values
E2C4  7EB44F                                  jmp     wrmStrt                                         ; call basic warmstart
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; This part sends sectors to dw4 to be written. It is a block very similar to DoRead() but must work a bit different:
                      ; Must send three blocks of data: The dw4 command, the data and the checksum, all of them in just one chunk
                      ; -------------------------------------------------------------------------------------------------------------------------------
E2C7  9612            dowrit  lda   <dnum                                                     ; get drive number (real dw4 number)
E2C9  5F                      clrb                                                     ; LSN bits 23-16
E2CA  3436                    pshs  a,b,x,y                            ; save registers / data                                        STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum
E2CC  8E0000                  ldx     #0                                                              ; zero to calculate Checksum for that sector
E2CF  4F                      clra                                                                    ; byte counter (256)
E2D0  E6A0            calc    ldb     ,Y+                                                     ; read a byte
E2D2  3A                      abx                                                                     ; add to checksum
E2D3  4A                      deca                                                                    ; decrement counter
E2D4  26FA                    bne   calc                                                      ; if not all 256, keep on
E2D6  9F76                    stx   <chksum                                           ; save checksum
E2D8  8657                                    lda   #$57                      ; dw4 OP_WRITE (write one sector)
E2DA  3402            rewrit  pshs  a                                          ; save command onto stack                              STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum, OP_WRITE
E2DC  30E4                    leax  ,s                                                ; point X to top of stack
E2DE  108E0005                ldy   #5                                ; 5 bytes
E2E2  BDFB57                  jsr   DWWrite                           ; send 5 bytes to dw4:  OP_WRITE, dnum, $0, Sector(H-L)
E2E5  3502                    puls  a                                          ; discard command                                                      STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum
E2E7  AE64                    ldx   4,s                                        ; get sector buffer pointer = received Y
E2E9  108E0100                ldy   #$0100                             ; to write 256 bytes
E2ED  BDFB57                  jsr   DWWrite                           ; send sector (256 bytes)
E2F0  109E76                                  ldy   <chksum                                           ; get calculated checksum
E2F3  3420                    pshs  y                                          ; save it onto stack                                   STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum, ChksumL, ChksumH
E2F5  30E4                    leax  ,s                                                ; point X to top of stack
E2F7  108E0002                ldy   #2                                                ; 2 bytes
E2FB  BDFB57                  jsr   DWWrite                           ; send checksum to dw4
                                                                                                                      ; get answer from dw4 for the whole pack
E2FE  3121                                    leay  1,y                                        ; after a DWRITE Y returns zeroed, so this way it becomes equal to one
E300  BDFAD2                  jsr   DWRead                            ; read dw4 answer       - this overwrites dnum byte
E303  3262                    leas  2,s                                        ; discard checksum                                             STACK: RETADRS, YL, YH, SectorL, SectorH, $0, Returnedvalue
E305  2513                    bcs   writex                             ; detected framing error
E307  2611                    bne   writex                             ; detected not all bytes received
E309  A6E4                    lda   ,s                                                ; get server answer (in old dnum position)
E30B  270D                    beq   writex                             ; zero=OK, go out
E30D  81F3                    cmpa  #$F3                                       ; error type = #E_CRC?
E30F  2608                    bne   writer                             ; not this one, go out signaling error
E311  9612                                    lda   <dnum                           ; get drive number
E313  A7E4                    sta   ,s                                                ; put again in stack                                            STACK: RETADRS, YL, YH, SectorL, SectorH, $0, dnum
E315  8677                    lda   #$77                                                      ; dw4 code for OP_REWRITE
E317  20C1                    bra   rewrit                             ; go to rewrite same sector again
E319  53              writer  comb                                                     ; sets the carry flag (ERROR)
E31A  35B6            writex  puls  a,b,x,y,pc                ; restore received registers and return      STACK: - - -
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
E31C  0D4457342653444320455854454E444552202856302E32352E3135290D psmsg           fcc     13,"DW4&SDC EXTENDER (V0.25.15)",13
E339  4259205045524520534552524154202832303231290D0D00                         fcn        "BY PERE SERRAT (2021)",13,13
                      ; --------------------------------------------------------------------------------------------------------------------------------                      
E351  204C4F4144494E4720524F4D0D00 ldtextr         fcn   / LOADING ROM/,13                                         ; initial message to the user
E35F  0D524F4D204C454E4754483A2000 endtxt  fcn   $0d, /ROM LENGTH: /                                       ; message to show rom length
E36D  0D524F4D20544F4F2042494700 bigrom  fcn   $0d, /ROM TOO BIG/                                        ; message for roms greater than 16k-256bytes
E37A  4E4F5420412044573420534C4F540D00 notdw4  fcn     /NOT A DW4 SLOT/,13                                     ; message for lrom without params and with default drive <> dw4
E38A  544F4B454E20544F4F204C4F570D00 low     fcn   /TOKEN TOO LOW/,13                                        ; message for not recognized token
E399  544F4B454E20544F4F20484947480D00 high    fcn   /TOKEN TOO HIGH/,13                                       ; message for not recognized token
E3A9  4452495645204D55535420455155414C20554E49540D00 drverr  fcn     /DRIVE MUST EQUAL UNIT/,13                      ; message for drive <> unit for physical drives
E3C0  4953204C494E4B454420544F20414E4F5448455220554E49540D00 alrput  fcn     /IS LINKED TO ANOTHER UNIT/,13  ; message for trying to use an already linked disc
E3DB  554E4954204C494E4B0D00 headlnk fcn     /UNIT LINK/,13                                                  ; header for LLINK
E3E6  2020202000      spaces  fcn     /    /                                                                  ; text for detail line in LLINK
E3EB  4C4F4144494E4720424F4F5420545241434B00 ldtext   fcn   /LOADING BOOT TRACK/                                     ; message for OS9boot
E3FE  0D4E4F5420414E204F5320424F4F5420545241434B00 noostxt  fcn   $0d, /NOT AN OS BOOT TRACK/              ; errro message for os9boot
E414  202300          xidxTxt fcn     / #/                                                                            ; text that precedes HD index number                    
E417  535452494E4720544F4F204C4F4E470D00 Str2Long        fcn     /STRING TOO LONG/,13                                    ; message for param string too long
E428  4E4F204D50492050524553454E540D00 NotAnMPI fcn    /NO MPI PRESENT/,13                                     ; message for no NPI and  ... next message too
E438  4E4F205344432050524553454E540D00 NotAnSDC        fcn     /NO SDC PRESENT/,13                                     ; message for no SDC present
E448                  NotActSDC
E448  4E4F20414354495645205344430D00                         fcn     /NO ACTIVE SDC/,13                                      ; mmesage for not selected SDC
E457                  BadFmtType
E457  464F524D41542056414C5545204E4F542056414C49440D00                         fcn     /FORMAT VALUE NOT VALID/,13             ; message for parameter invalid
E46F  494E44455820544F4F20484947480D00 idxHigh fcn     /INDEX TOO HIGH/,13                                     ; index higher than max allowed
E47F  43555252454E542044495220495320524F4F540D00 noCurDir        fcn   /CURRENT DIR IS ROOT/,13                  ; message to user
E494  4E4F5420504F535349424C450D00 canNot  fcn     /NOT POSSIBLE/,13                                               ; message for switching not possible
E4A2  3A2D2D4E4F20444953432D2D0D00 sdcNoDsc        fcn     /:--NO DISC--/,13                                               ; message for no disk in sdc drive
E4B0  3A544F54414C20534543544F52533A2000 totSect fcn     /:TOTAL SECTORS: /                                      ; text for detail in SDRIVE
E4C1  5344462000      datSDF  fcn     /SDF /                                                                  ; text for SDRIVE
E4C6  4449522000      datDIR  fcn     /DIR /                                                                  ; text for SDRIVE
E4CB  2020202000      datNUL  fcn     /    /                                                                  ; text for SDRIVE
E4D0  2020634F4E5400  msgCont fcn     /  cONT/                                                                        ; message to user CONTinue
E4D7  20206558495400  msgExit fcn     /  eXIT/                                                                        ; message to user EXIT
E4DE  494E56414C494420504152414D455445522056414C55450D00 wrongPar        fcn     /INVALID PARAMETER VALUE/,13            ; message
E4F7  535441434B20434F4E464C494354530D00 stackPrb        fcn     /STACK CONFLICTS/,13                                    ; error message 
E508  54494D454F55540D00 timeout         fcn     /TIMEOUT/,13                                                    ; error message for SDC access
E511                  targetInUse
E511  54415247455420494E205553450D00                         fcn     /TARGET IN USE/,13                                      ; error message for SDC
E520                  dirNotFound             
E520  4449524543544F5259204E4F5420464F554E440D00                         fcn     /DIRECTORY NOT FOUND/,13                        ; error message for SDC
E535                  pathNameInvalid         
E535  494E56414C494420504154484E414D450D00                         fcn     /INVALID PATHNAME/,13                           ; error message for SDC
E547                  miscHardware            
E547  4D4953432E204841524457415245204552524F520D00                         fcn     /MISC. HARDWARE ERROR/,13                       ; error message for SDC
E55D  554E4B4E4F574E204552524F520D00 unknown         fcn     /UNKNOWN ERROR/,13                                      ; error message for SDC
E56C                  targetNotFound  
E56C  544152474554204E4F5420464F554E440D00                         fcn     /TARGET NOT FOUND/,13                           ; error message for SDC
E57E  4E4F2036344B20434F4D50555445520D00 not64K  fcn     /NO 64K COMPUTER/,13                                    ; error message for SDC
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; install new routines
                      ; Creates a new USR table and fills it with calls to 'FC ERROR'
                      ; usess the old USR table space to create a new STUB for the
                      ; new commands that will be added after the D.O.S. ones
                      ; -------------------------------------------------------------------------------------------------------------------------------
E58F  1A50            initext orcc    #dimask                                         ; disable interrupts
E591  308D1E55                                leax  newusr,pcr                        ; new address table for usr's
E595  9FB0                    stx   usrptr                            ; save at memory variable
E597  108E8B8D                ldy   #fcerr                            ; address for error message
E59B  860A                    lda   #10                               ; number of usr's
E59D  10AF81          nxtvec  sty   ,X++                              ; fill element table
E5A0  4A                      deca                                    ; decrement counter
E5A1  26FA                    bne   nxtvec                            ; not finished, go back
E5A3  8612                    lda   #tokens                           ; get number of new tokens
E5A5  B70134                  sta   newstb                            ; store at stub base byte
E5A8  308C72                  leax  newrds,pcr                        ; base of new reserved words table
E5AB  BF0135                  stx   newstb+1                          ; save on stub
E5AE  308C4C                  leax  newdsp,pcr                        ; address of dispatching routine
E5B1  BF0137                  stx   newstb+3                          ; save on stub
E5B4  8EE31B                                  ldx     #psmsg-1                                                ; point to message string
E5B7  BD90E5                                  jsr     outstr                                          ; show to screen
E5BA  B6FF7F                                  lda     MPIcfg                                          ; get received MPI config value for mega-mini-MPI compatibility
E5BD  8433                                    anda    #$33                                                    ; avoid using bits 7-6-3-2
E5BF  B703FF                                  sta     cpyMPIcfg                                       ; initialize copy
E5C2  B7FF7F                                  sta     MPIcfg                                          ; update config byte
E5C5  8D05                                    bsr     initask                                         ; put initial values
E5C7  1CAF                                    andcc   #eimask                                         ; enable interrupts
E5C9  7EF8A8                                  jmp     expert                                          ; goto auto-start experiments
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
E5CC  0FE7            initask clr     <retry                                          ; NO retry mark
E5CE  CC0102                                  ldd     #$0102                                          ; default values for the first two physical drives
E5D1  8E00FC                                  ldx     #$fc                                                    ; point to config area
E5D4  ED81                                    std     ,x++                                                    ; save these values
E5D6  CCF6F5                                  ldd     #$f6f5                                          ; default values for Units 3-4 pointing to DW4 slots (245-246)
E5D9  ED84                                    std     ,x                                                              ; save these values
E5DB  DC8A                                    ldd     <$8a                                                    ; get 16 bits zero
E5DD  FD0114                                  std     floSDC                                          ; clear $114 and $115
                                                                                                                      ; pre-test on MPI                       
E5E0  1705F0                                  lbsr    tstMPI                                          ; is there an MPI?
E5E3  2604                                    bne     initsk01                                                ; no, go to individual checks
E5E5  170548                                  lbsr    tstHARD                                         ; yes, verify connected hardware
E5E8  39                                      rts                                                                     ; return
                                                                                                                      ; else verify individual elements (only one active)
E5E9  17060B          initsk01        lbsr    tstSDC                                          ; is SDC active?
E5EC  2709                                    beq     fndSDC                                          ; yes, skip next test
E5EE  17061D                                  lbsr    tstFLO                                          ; is FDC active instead?
E5F1  2609                                    bne     fndNthg                                         ; no, go out, no harware present at all
E5F3  8604            fndFDC  lda     #%00000100                                      ; set FDC present and let both SDC drives OFF
E5F5  2002                                    bra     savHdwF                                         ; go save it
E5F7  860B            fndSDC  lda     #%00001011                                      ; mark SDC present and put both SDC drives ON
E5F9  B70114          savHdwF sta     floSDC                                          ; save result
E5FC  39              fndNthg rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command dispatcher called from BASIC
                      ; decodes the received token and passes it with the address
                      ; of the new dispatch table to the ROM command execution
                      ; -------------------------------------------------------------------------------------------------------------------------------
E5FD  811A            newdsp  cmpa  #newtok                           ; compare to first defined token
E5FF  250C                    blo   lowerr                            ; if lower show error
E601  812C                    cmpa  #newtok+tokens                    ; compare to last one
E603  240E                    bhs   higher                            ; if higher show error
E605  801A                    suba  #newtok                           ; first token converts to 0
E607  308C68                  leax  nwtb01,pcr                        ; get new commands dispatch table base address
E60A  7E84ED                  jmp   romcmd                            ; pass to rom command execution
E60D  308DFD79        lowerr  leax  low,pcr                           ; point to message
E611  2004                    bra   new1                                                      ; show error and return to interpreter
E613  308DFD82        higher  leax  high,pcr                          ; point to message
E617  BD90E5          new1    jsr   outstr                            ; show the selected error
E61A  7E89B4                  jmp   synerr                            ; rom will show syntax error message
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; new reserved words
                      ; -------------------------------------------------------------------------------------------------------------------------------
E61D  44574C4F41C4    newrds  fcc   /DWLOA/,$C4                       ; char 'D' OR $80               dwload  = DLOAD (the new into this ROM)
E623  4D4F554ED4              fcc   /MOUN/,$D4                        ; char 'T' OR $80               mounT           = Associate a VDK to a Drive Number
E628  4C494ECB                fcc     /LIN/,$CB                                       ; char 'K' or $80               linK            = fill unit cell with received data
E62C  4C4C494ECB              fcc     /LLIN/,$CB                                      ; char 'K' or $80               llinK           = show units links
E631  444FD3                  fcc     /DO/,$D3                                                ; char 'S' or $80               doS             = Boot a NitrOS-9 disc
E634  4C524FCD                fcc     /LRO/,$CD               ; char 'M' or $80               lroM            = Load or Run a ROM image
E638  42414ECB                fcc     /BAN/,$CB                                       ; char 'K' or $80               banK            = change active SDC-bank
E63C  534C4FD4                fcc     /SLO/,$D4                                       ; char 'T' OR $80               sloT            = change to another MPI slot ($ef)
E640  5344524956C5            fcc     /SDRIV/,$C5                                     ; char 'E' or $80               sdrivE  = multipurpose for SDC-card
E646  534449D2                fcc     /SDI/,$D2                                       ; char 'R' or $80               sdiR            = multipurpose for SDC-card
E64A  48444944D8              fcc   /HDID/,$D8                                        ; char 'X' or $80               hdidX           = select the index disc in an HD array (max 90)
E64F  534D4B4449D2            fcc     /SMKDI/,$D2                                     ; char 'R' or $80               smkdiR  = create a directory in the SDC-card
E655  534B494CCC                         fcc  /SKIL/,$CC                                      ; char 'L' or $80               skilL           = kill a file or an empty directory in a SDC-card
E65A  535245CE                                fcc     /SRE/,$CE                                       ; char 'N' or $80               sreN            = rename a file in a SDC-card
E65E  554E4C4F41C4                            fcc     /UNLOA/,$C4                     ; char 'D' OR $80               unloaD  = token to be used just as parameter ($f6)
E664  534348C4                                fcc     /SCH/,$C4                               ; char 'D' OR $80               schD            = to change ACTUAL Directory
E668  57524954C5                              fcc     /WRIT/,$C5                                      ; char 'E' or $80               writE           = write to a flash bank
E66D  53434F50D9                              fcc     /SCOP/,$D9                                      ; char 'Y' or $80    scopY    = copy ROM from bank or slot to RAM
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Dispatch table. This is an indirect table
                      ; it contains the start address of every new command 
                      ; -------------------------------------------------------------------------------------------------------------------------------
E672  F8F7            nwtb01  fdb   dwload                                            ; routine for DWLOAD command
E674  E6A3                                    fdb   mount                                             ; routine for MOUNT command                
E676  E711                                    fdb     link                                                    ; routine for LINK command
E678  E78E                                    fdb     llink                                                   ; routine for LLINK command
E67A  E7EB                                    fdb     os9boot                                         ; routine to boot NitrOS-9
E67C  E86B                                    fdb     xlrom                                                   ; routine to load - run a ROM image
E67E  EA11                                    fdb     xbank                                                   ; routine to switch SDC active bank
E680  EC2B                                    fdb     xslot                                                   ; routine to change of MPI slot
E682  ED1B                                    fdb     xsdrive                                         ; routine for SDC-card DRIVE functions
E684  F30F                                    fdb     xsdir                                                   ; routine for SDC-card DIR functions
E686  ED01                                    fdb     xHDidx                                          ; routine to select index of HD
E688  F20C                                    fdb     xsmkdir                                         ; routine for SMKDIR command
E68A  F236                                    fdb     xskill                                          ; routine for SDELETE command
E68C  F23D                                    fdb     xsren                                                   ; routine for SRENAME command
E68E  E775                                    fdb     snError                                         ; simply show syntax error
E690  F2D4                                    fdb     xschd                                                   ; routine for SCHD command
E692  F4B5                                    fdb     xwrite                                          ; routine for WRITE command
E694  F67F                                    fdb     xscopy                                          ; routine for SCOPY command
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Dispatch routines for the new added commands
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Routine to mark as invalid the four I/O buffers
                      ; -------------------------------------------------------------------------------------------------------------------------------
E696  4F              mrkmod  clra                                                                    ; get a 0
E697  8E0636                                  ldx     #$636                                                   ; point to 1st I/O buffer flags data
E69A  C615                                    ldb     #21                                                     ; offset to last I/O
E69C  A785            mo01            sta     b,x                                                     ; mark as invalid
E69E  C007                                    subb    #7                                                              ; offset to previous I/O
E6A0  2AFA                                    bpl     mo01                                                    ; if not negative, loopback
E6A2  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command to ask DW4 to mount a VDK file and get the slot number back
                      ; -------------------------------------------------------------------------------------------------------------------------------
E6A3  1700BA          mount           lbsr    readunit                                                ; read unit number
E6A6  D7F8                                    stb     <numUni                                         ; save it as number of Unit
E6A8  BD89AA                                  jsr     ckComma                                         ; CkComma
E6AB  BDB7AA                                  jsr     getfnam                                         ; GetNam into $1d1 (nambuf)
E6AE  8E01D0                  ldx   #nambuf-1                                 ; byte before packet buffer start
E6B1  6F84                    clr   ,x                                                                ; zero MSB to make a 16 bit value for Y
E6B3  10AE81                  ldy   ,x++                                                      ; length of file name (16 bit)
E6B6  2756                    beq   failvdk                                           ; if len=0 no file entered
E6B8  E61F                    ldb   -1,x                                                      ; length of name (8 bit)
                                                                                                                      ; Adding .VDK if not termination entered, costs 31 bytes       
E6BA  3414                                    pshs  b,x                                                       ; save pointer to beginning of name and name length
E6BC  A680            isdot   lda   ,x+                                                       ; get a char from name
E6BE  812E                    cmpa  #'.'                                                      ; is it a dot?
E6C0  2718                    beq   alrdot                                            ; yes, stop search, quit loop
E6C2  5A                      decb                                                                    ; decrement counter
E6C3  26F7                    bne   isdot                                                     ; not end of string, get next char name
E6C5  CC2E56                  ldd   #$2e56                                            ; get chars ".V"
E6C8  ED81                    std   ,x++                                                      ; add to end of name
E6CA  CC444B                  ldd   #$444b                                            ; get chars "DK"
E6CD  ED81                    std   ,x++                                                      ; add to end of name
E6CF  E6E4                                    ldb   ,s                                                                ; get old name length
E6D1  CB04                                    addb  #4                                                                ; add 4 (because of .VDK added)
E6D3  E7E4                                    stb   ,s                                                                ; update new length in stack
E6D5  F701D1                  stb   nambuf                                            ; save new length in byte before string
E6D8  3124                    leay  4,y                                                       ; correction to name length - DW parameter
E6DA  3402            alrdot  pshs    a                                                               ; save register
E6DC  8602                                    lda     #2                                                              ; get value to mark output
E6DE  B7FF20                                  sta     BBOUT                                                   ; to DW4
E6E1  4F                                      clra                                                                    ; set counter
E6E2  4A              mount01 deca                                                                    ; decrement counter
E6E3  26FD                                    bne     mount01                                         ; not zero, loopback
E6E5  3502                                    puls    a                                                               ; restore register
E6E7  3514                                    puls  b,x                                               ; restore pointer and length
E6E9  6F85                                    clr   b,x                                                       ; zero terminate name string (for error)
E6EB  6C83                    inc   ,--x                                                      ; 1 = DriveWire OP_NAMEOBJ_MOUNT command
E6ED  3122                    leay  2,y                                                       ; length of DW packet = name length + 2
E6EF  1A50                    orcc    #dimask                                         ; disable interrupts to work with DW4
E6F1  BDFB57                  jsr   DWWrite                                           ; send file request to server
E6F4  8E0012                  ldx   #dnum                                                     ; our drive number variable
E6F7  6F84                    clr   ,x                                                                ; clear it
E6F9  3121                    leay  1,y                                                       ; read one byte back
E6FB  BDFAD2                  jsr   DWRead                                            ; get drive number
E6FE  1CAF                                    andcc #eimask                                           ; enable interrupts again 
E700  6D84                    tst   ,x                                                                ; verify received value
E702  2605                    bne   vdkok                                                     ; successful mount                                       
E704  C619                    ldb   #$19                                                      ; code for MO ERROR
E706  7E8344                                  jmp   syserr                                            ; show error message (syserr)
E709  D612            vdkok           ldb     <dnum                                                   ; pass slot to B
E70B  8D25                                    bsr     stval                                                   ; verify / store new value
E70D  39                                      rts                                                                     ; return
E70E  7EB84B          failvdk jmp   ioerror                                           ; I/O error message
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command to link a Unit with a drive number or a DW4 slot number
                      ; -------------------------------------------------------------------------------------------------------------------------------
E711  8D4D            link            bsr     readunit                                                ; read unit number
E713  D7F8                                    stb     <numUni                                         ; save it as number of Unit
E715  8D65                                    bsr     opt8bit                                         ; search another param 8 bit number
E717  260E                                    bne     link01                                          ; if greater than 0, goto more tests
                                                                                                                      ; requested slot #0
E719  D6F8                                    ldb     <numUni                                         ; get unit number
E71B  C101                                    cmpb    #1                                                              ; is it 1?
E71D  2604                                    bne     notalw                                          ; no, error. Only unit 1 is allowed to be linked to slot #0
E71F  0FFC                                    clr     <$fc                                                    ; put 0 value in first cell (unit 1, value 0)
E721  200E                                    bra     link03                                          ; exit
E723  C608            notalw  ldb     #$08                                                    ; code for FC error
E725  2045                                    bra     basyserr                                                ; show error message
E727  C104            link01  cmpb    #4                                                              ; is new value greater than 4 (DW4)?
E729  2204                                    bhi     link02                                          ; yes, skip test
E72B  D1F8                                    cmpb    <numUni                                         ; equals new drive number the unit number? (mandatory!)
E72D  262B                                    bne     numError                                                ; no, show unit error
E72F  8D01            link02  bsr     stval                                                   ; verify and store new value
E731  39              link03  rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Verifies that the new value (B) is not linked to another unit. If good, saves it
                      ; -------------------------------------------------------------------------------------------------------------------------------
E732  8E00FB          stval           ldx     #$00fb                                          ; point one byte before drive/slot table
E735  8604                                    lda     #4                                                              ; counter (number of units)
E737  E186            stv02           cmpb    a,x                                                     ; equals new value the contents of that slot?
E739  2715                                    beq     stv04                                                   ; yes, see if it is the same unit
E73B  4A                                      deca                                                                    ; decrement counter
E73C  26F9                                    bne     stv02                                                   ; if not 0, loopback
E73E  8E00FB          estval  ldx     #$00fb                                          ; point to one byte before data table
E741  96F8                                    lda     <numUni                                         ; get unit number
E743  8104                                    cmpa    #4                                                              ; is unit 4?
E745  2603                                    bne     stv03                                                   ; no, skip next
E747  7F0115                                  clr     idxHD                                                   ; set index to HD to 0
E74A  E786            stv03           stb     a,x                                                     ; store slot into table
E74C  17FF47                                  lbsr    mrkmod                                          ; modify data to flag drive changes to system   
E74F  39                                      rts                                                                     ; everything alright, return
E750  91F8            stv04           cmpa    <numUni                                         ; is unit with this value the same as destination in LINK command?
E752  27EA                                    beq     estval                                          ; yes, acceptable, return
E754  3262                                    leas    2,s                                                     ; get rid of return address
                                                                                                                              ; show error. We do not admit the same slot number in two units!
                      ;                       bra     already                                 ; *** Fall Thru *** not needed 
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
E756  8EE3BF          already ldx     #alrput-1                                       ; point to error message
E759  10                                      fcb     #$10                                                    ; to jump over next one
E75A  8EE3A8          numError ldx    #drverr-1                                       ; point to error message
E75D  7E90E5                                  jmp     outstr                                          ; show message and return to interpreter
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
E760  0FF8            readunit        clr     <numUni                                         ; clear unit number
E762  8D0D                                    bsr     getunit                                         ; get a one digit number
E764  2704                                    beq     errNum                                          ; if zero jump to device error
E766  C104                                    cmpb    #4                                                              ; is it greater than 4?
E768  2305                                    bls     OKnext                                          ; no, goto OK
E76A  C628            errNum  ldb     #$28                                                    ; device number error (bas_err_DN)
E76C  7E8344          basyserr        jmp     syserr                                          ; bas_system_error
E76F  0EA5            OKnext  jmp     <redLChr                                                ; bas_poll_char                 
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
E771  9DA5            getunit jsr     <redLChr                                                ; get a char (bas_poll_char)
E773  2603                                    bne     r8b01                                                   ; if there is something, skip next
E775  7E89B4          snError jmp     synerr                                          ; show error message (BAS-SN-Error)
E778  BDE786          r8b01           jsr     get8bit                                         ; get a number (8bits)
E77B  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
E77C  9DA5            opt8bit jsr     <redLChr                                                ; get a char (bas_poll_char)
E77E  2705                                    beq     eo8b                                                    ; if 0, return
E780  BD89AA                                  jsr     ckComma                                         ; check if there is a comma separator (CkComa)
E783  8D01                                    bsr     get8bit                                         ; get a 8 bits number
E785  39              eo8b            rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
E786  3460            get8bit pshs    y,u                                                     ; save working registers
E788  BD8E51                                  jsr     asc2Num                                         ; convert ASCII to number (1 byte)
E78B  5D                                      tstb                                                                    ; update flags upon result
E78C  35E0                                    puls    y,u,pc                                          ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command to show the links of every unit
                      ; -------------------------------------------------------------------------------------------------------------------------------
E78E  8EE3DA          llink           ldx     #headlnk-1                                      ; point to message header
E791  BD90E5                                  jsr     outstr                                          ; show it
E794  8E00FB                                  ldx     #$00fb                                          ; point one byte before first data byte
E797  C601                                    ldb     #1                                                              ; unit counter
E799  A685            llink01 lda     b,x                                                     ; get linked value
E79B  8D06                                    bsr     showLine                                                ; show data
E79D  5C                                      incb                                                                    ; increment counter
E79E  C104                                    cmpb    #4                                                              ; done all units?
E7A0  23F7                                    bls     llink01                                         ; no, loopback
E7A2  39                                      rts                                                                     ; return to caller
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
E7A3  3416            showLine        pshs    d,x                                                     ; save working registers
E7A5  8EE3E7                                  ldx     #spaces+1                                       ; point to print 2 spaces
E7A8  BD90E5                                  jsr     outstr                                          ; print them
E7AB  E661                                    ldb     1,s                                                     ; get unit number
E7AD  4F                                      clra                                                                    ; clear high byte
E7AE  BD957A                                  jsr     outnum                                          ; print number
E7B1  E6E4                                    ldb     ,s                                                              ; get linked value
E7B3  4F                                      clra                                                                    ; clear high byte
E7B4  C10A                                    cmpb    #10                                                     ; is value lower than 10?
E7B6  2506                                    blo     sL01                                                    ; yes, goto print
E7B8  4C                                      inca                                                                    ; one space less
E7B9  C164                                    cmpb    #100                                                    ; is value lower than 100?
E7BB  2501                                    blo     sL01                                                    ; yes, goto print
E7BD  4C                                      inca                                                                    ; one space less
E7BE  8EE3E5          sL01            ldx     #spaces-1                                       ; point to print 4 spaces
E7C1  3086                                    leax    a,x                                                     ; skip the calculated ones upon the value
E7C3  BD90E5                                  jsr     outstr                                          ; print required spaces
E7C6  E6E4                                    ldb     ,s                                                              ; get linked value
E7C8  4F                                      clra                                                                    ; clear high byte                                                       
E7C9  BD957A                                  jsr     outnum                                          ; print number
E7CC  E661                                    ldb     1,s                                                     ; get unit number
E7CE  C104                                    cmpb    #4                                                              ; is it 4?
E7D0  2612                                    bne     sL02                                                    ; no, stop printing
E7D2  F60115                                  ldb     idxHD                                                   ; get index for HD
E7D5  270D                                    beq     sL02                                                    ; if zero, skip printing it
E7D7  8EE413                                  ldx     #xidxTxt-1                                      ; point to text
E7DA  BD90E5                                  jsr     outstr                                          ; show it
E7DD  F60115                                  ldb     idxHD                                                   ; get unit number
E7E0  4F                                      clra                                                                    ; zero to high byte
E7E1  BD957A                                  jsr     outnum                                          ; call ROM to print integer in D
E7E4  860D            sL02            lda     #$0d                                                    ; enter
E7E6  BDBCAB                                  jsr     outRegA                                         ; print A 
E7E9  3596                                    puls    d,x,pc                                          ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command to Boot a NitrOS-9 disc
                      ; -------------------------------------------------------------------------------------------------------------------------------
E7EB  5F              os9boot  clrb                                                                   ; default drive 0
E7EC  9DA5                     jsr   <redLChr                 ; peek at next char
E7EE  2703                     beq   defnum                                           ; not found, use default
E7F0  BD8E51                   jsr   asc2Num                                  ; Get 1 byte
E7F3  D712            defnum  stb   <dnum                                                     ; save drive number
E7F5  8EE3EA                                  ldx   #ldtext-1                                 ; point to 'loading' message
E7F8  BD90E5                   jsr   outstr                                           ; show it
E7FB  1A50                                    orcc  #dimask                                           ; disable interrupts
E7FD  BDE859                                  jsr     trick                                                   ; make sure DW4 connection is on
                                                                                                                              ; DriveWire bootstrap code. Get sectors 612-629 to $2600
E800  8E0264                        ldx   #612                                                        ; starting sector number to load
E803  108E2600                 ldy   #$2600                                           ; destination in memory
E807  BDFA84          DOSLoop  jsr   DoRead                                           ; read sector (into Y)
E80A  252F                     bcs   fail2                                                    ; start all over
E80C  262D                     bne   fail2                                                    ; show error
E80E  8C0264                   cmpx  #612                                                     ; our first sector?
E811  2608                     bne   not1st                                           ; branch if not
E813  ECA4                     ldd   ,y                                                               ; else get 1st two bytes at $2600
E815  10834F53                 cmpd  #$4f53                                           ; OS?
E819  261A                     bne   noos                                                     ; no, show error
E81B  866A            not1st   lda   #'*'+$40                                         ; print star
E81D  31A90100                 leay  256,y                                                    ; move for next sector
E821  3001                     leax  1,x                                                      ; increment number of sector to be read
E823  A789023B                 sta   571,x                                                    ; print progress (1024+32*5-612-1)
E827  8C0276                   cmpx  #630                                                     ; are we at last sector?
E82A  2DDB                     blt   DOSLoop                                          ; branch if not
E82C  8661                     lda   #'!'+$40                                         ; print bang
E82E  A789027B                 sta   635,x                                                    ; over last '*' (636-1)
E832  7E2602                   jmp   $2602                                                    ; jump into Boot code
E835  8EE3FD          noos     ldx   #noostxt-1                                       ; point to message
E838  BD90E5                   jsr   outstr                                           ; print string at x+1
E83B  7EB84B          fail2    jmp   ioerror                                          ; IO_error equ $b84b
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Patch to DosPlus Boot to allow RSDOS disks to boot too (Mike Miller 2021-05)
                      ; -------------------------------------------------------------------------------------------------------------------------------
E83E  8C4F53          rsbtchk  cmpx   #$4F53                                          ; Check for signature
E841  2713                                    beq     rsbt01                                                  ; Boot DragonDOS style disk
E843  CC2201                                  ldd     #$2201                                                  ; Look instead RSDOS location track 34 first sector
E846  BDC0F9                                  jsr     gtbtsig                                    ; Read boot sig (enter after track and sector set)
E849  2401                                    bcc     rsbt00                                          ; it's OK, skip next
                      ;                       jmp     $C166                                                   ; Failed to read then exit
E84B  39                                      rts                                                             ; Failed to read then exit
E84C  9E4F            rsbt00  ldx     $4F                                                     ; Get first 2 bytes 
E84E  8C4F53                                  cmpx    #$4F53                                          ; Check for signature 
E851  2703                                    beq     rsbt01                                          ; it is Bootable!
E853  7EC164                                  jmp     $C164                                              ; Not bootable RSDOS way either, exit with ?BT error 
E856  7EC14E          rsbt01  jmp     $C14E                                                   ; Otherwise load the track
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Routine that marks on the DW4 line for a while, to avoid fails at 1st access
                      ; -------------------------------------------------------------------------------------------------------------------------------
E859  3414            trick    pshs   b,x                                                     ; save working registers
E85B  8EFF20                                  ldx   #PIA1Base                         ; address of PIA1
E85E  C602                     ldb   #$02                                                     ; put value #2 (bit1 to high level)
E860  E784                     stb   ,x                               ; make RS-232 output marking
                                                                                                                              ; Spin for a while so that the RS-232 bit stays hi for a time
E862  8EA000          waw             ldx   #$a000                                            ; wait counter
E865  301F            waw1     leax  -1,x                                                     ; decrement counter
E867  26FC                     bne   waw1                                                     ; if not 0, loopback
E869  3594                     puls   b,x,pc                                          ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command to load - run a ROM image
                      ; -------------------------------------------------------------------------------------------------------------------------------
E86B  1A50            xlrom           orcc  #dimask                   ; disable firq, irq
E86D  8DEA                                    bsr     trick                                                   ; make sure dw4 connection is on
E86F  9DA5            lromrty         jsr   <redLChr                                          ; peek at next character (after "rom")
E871  804E                    suba  #'N'                                                      ; to detect if it was an 'n' - if so don't exec it after loading
E873  9710                    sta   <noexec                                           ; save difference. 0 = noexec
E875  2602                    bne   savvec                                            ; if not 'n' jump next instruccion
E877  9D9F                    jsr   <getNChr                                          ; jump over that char (the 'n')
E879  8EE350          savvec  ldx   #ldtextr-1                                        ; byte before message
E87C  8D76                                    bsr   prtstr                                            ; print string
E87E  BDB7AA                          jsr   getfnam                                           ; read rom file name and length into $1d1 = nambuf
E881  7D01D1                  tst     nambuf                                          ; is filename length equal 0?
E884  261D                    bne     rom01                                                   ; no, process it
E886  F6060A                  ldb     $60a                                                    ; get default drive number
E889  8E00FB                  ldx     #$00fb                                          ; point to table of links x unit
E88C  A685                    lda     b,x                                                     ; search slot number in table
E88E  2604                    bne     ver01                                                   ; if not 0, do more tests
E890  C101                    cmpb    #1                                                              ; is slot 0 linked to unit 1? - to avoid tricks with poke unit's slot numbers
E892  2704                    beq     ver02                                                   ; yes, accept it and go on. the only legal one!
E894  8105            ver01           cmpa    #5                                                              ; is it a dw4 slot?
E896  2504                    blo     errdw4                                          ; no, show error
E898  9712            ver02           sta     <dnum                                                   ; save slot number at variable
E89A  2064                    bra     rdecb                                                   ; go process rom at default drive
E89C  8EE379          errdw4  ldx     #notdw4-1                                       ; point to message error
E89F  BD90E5                                  jsr   outstr                                            ; show message
E8A2  39                                      rts                                                                     ; return
E8A3  8E01D0          rom01           ldx   #nambuf-1                                 ; byte before packet buffer start
E8A6  6F84                    clr   ,x                                                                ; zero msb to make a 16bit value for y
E8A8  10AE81                  ldy   ,x++                                                      ; length of file name (16 bit)
E8AB  2742                    beq   failedr                                           ; if len=0 no file entered
E8AD  E61F                    ldb   -1,x                                                      ; length of name (8 bit)
                                                                                                                              ; adding .rom if not termination entered      
E8AF  3414                                    pshs  b,x                                                       ; save pointer to beginning of name and name length
E8B1  A680            isdotr          lda   ,x+                                                       ; get a char from name
E8B3  812E                    cmpa  #'.'                                                      ; is it a dot?
E8B5  2718                    beq   alrdotr                                           ; yes, stop search, quit loop
E8B7  5A                      decb                                                                    ; decrement counter
E8B8  26F7                    bne   isdotr                                            ; not end of string, get next char name
E8BA  CC2E52                  ldd   #$2e52                                            ; get chars ".r"
E8BD  ED81                    std   ,X++                                                      ; add to end of name
E8BF  CC4F4D                  ldd   #$4f4d                                            ; get chars "om"
E8C2  ED81                    std   ,x++                                                      ; add to end of name
E8C4  E6E4                                    ldb   ,s                                                                ; get old name length
E8C6  CB04                                    addb  #4                                                                ; add 4 (because of .rom added)
E8C8  E7E4                                    stb   ,s                                                                ; update new length in stack
E8CA  F701D1                  stb   nambuf                                            ; save new length in byte before string
E8CD  3124                    leay  4,y                       ; correction to name length - dw parameter
E8CF  3514            alrdotr         puls  b,x                                               ; restore pointer and length
E8D1  6F85                    clr   b,x                                                       ; zero terminate name string (for error)
E8D3  6C83                    inc   ,--x                                                      ; 1 = drivewire op_nameobj_mount command
E8D5  3122                    leay  2,y                                                       ; length of dw packet = name length + 2
E8D7  BDFB57                  jsr   DWWrite                                           ; send file request to server
E8DA  8E0012                  ldx   #dnum                                                     ; our drive number variable
E8DD  6F84                    clr   ,x                                                                ; clear it
E8DF  3121                    leay  1,y                                                       ; read one byte back
E8E1  BDFAD2                  jsr   DWRead                                            ; get drive number
E8E4  6D84                    tst   ,x                                                                ; verify received value
E8E6  2618                    bne   rdecb                                                     ; successful mount. if z has not been set = error (coundn't mount). jump to readin
E8E8  C619            moError         ldb   #$19                                                      ; errror code for mo error
E8EA  1CAF                    andcc #eimask                                   ; enable interrupts
E8EC  7E8344                  jmp   syserr                                            ; show error message
E8EF  1CAF            failedr         andcc #eimask                                   ; enable interrupts
E8F1  7EB84B                                  jmp   ioerror                                           ; i/o errror message
E8F4  D689            prtstr  ldb     <curlow                                                 ; get low byte of cursor position
E8F6  C41F                    andb  #$1f                                                      ; is column zero?
E8F8  2602                    bne   shwmsg                                    ; no, print whith leading enter
E8FA  3001                                    leax  1,x                                       ; jump leading enter 
E8FC  BD90E5          shwmsg  jsr   outstr                                       ; print string
E8FF  39                                      rts                                                                     ; return
E900  8E0000          rdecb   ldx   #0                                                                ; begin at sector zero
E903  108E3000                ldy   #ramini                                           ; destination address
E907  9F76                    stx   <endadr                                           ; set last load address to zero 
E909  0FE6                    clr   <bigfil                                           ; flag for long file to false
E90B  1700AB          romlop  lbsr  doreadr                                   ; read sector (into y) 
E90E  25DF                    bcs   failedr                                           ; detect frame error
E910  26DD                    bne   failedr                                           ; detect incomplete read
E912  2D15                    blt   nomore                    ; detect end of file. finish
E914  862A                    lda   #'*'                                                      ; get 'star' ascii code
E916  BDB54A                                  jsr   outchr                                            ; show on screen, we have read a sector
E919  31A90100                leay  256,y                                                     ; point to next sector address
E91D  108C6FFF                cmpy  #rmaxld                                           ; got to the ram limit?
E921  2404                    bhs   bigend                                            ; no, keep on copying
E923  3001                    leax  1,x                                                       ; increment sector number
E925  20E4                    bra   romlop                                            ; go read next sector
E927  0CE6            bigend  inc   <bigfil                                           ; big file flag to true
E929  109F76          nomore  sty   <endadr                                           ; save as last loaded address
E92C  8608                    lda   #$08                                                      ; get backspace ascii code
E92E  BDB54A                                  jsr      outchr                                         ; print on screen (go back one char)
E931  8621                    lda   #'!'                                                      ; get 'bang' ascii code
E933  BDB54A                                  jsr   outchr                                            ; put it on screen
E936  0DE6                                    tst   <bigfil                                           ; is this a too long file?
E938  2705                                    beq   final                                                     ; no, jump message
E93A  8EE36C                                  ldx   #bigrom-1                                 ; message for rom greater than 16k-256bytes
E93D  8DB5                                    bsr   prtstr                                            ; print message
E93F  0D10            final   tst   <noexec                                           ; has to be run?
E941  2610                    bne   runit                                                     ; yes, do it
E943  8EE35E                  ldx   #endtxt-1                                 ; point to end text
E946  8DAC                                    bsr   prtstr                                            ; print message
E948  DC76                                    ldd   <endadr                                           ; get last loaded ram address
E94A  833000                                  subd  #ramini                                           ; subtract init ram address
E94D  BD957A                                  jsr   outnum                                            ; print length 
E950  1CAF                            andcc #eimask                                           ; enable interrupts
E952  39                      rts                                                                     ; return to the caller
E953  FCC000          runit   ldd   >romini                                           ; get two bytes from rom area
E956  8E9669                                  ldx   #$9669                                            ; create a pattern 1001 0110 0110 1001
E959  BFC000                                  stx   >romini                                           ; store into two dos-rom bytes
E95C  BCC000                                  cmpx  >romini                                           ; have been modified?
E95F  2603                                    bne   basicup                                           ; no, must switch to map1 and copy basic to upper ram
E961  FDC000                                  std   >romini                                           ; put back two saved bytes
E964  8EE976          basicup  ldx    #basic                                          ; point to beginning of code to be moved
E967  CE2A00                                  ldu     #$2a00                                          ; destination in low ram
E96A  EC81            basup01 ldd     ,x++                                                    ; get a word
E96C  EDC1                                    std     ,u++                                                    ; put at destination
E96E  8CE9B9                                  cmpx    #doreadr                                                ; got to the end?
E971  23F7                                    bls     basup01                                         ; no, loopback
E973  7E2A00                                  jmp     $2a00                                                   ; transfer control to copied code in ram
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
E976  8E8000          basic   ldx   #basini                                           ; get basic initial address
E979  B7FFDE          bas1    sta   $ffde                                                     ; go map0 (rom)
E97C  EC84                    ldd   ,x                                                                ; get 2 bytes
E97E  B7FFDF                  sta   $ffdf                                                     ; go map1 (ram)
E981  ED81                    std   ,x++                                                      ; store 2 bytes, incr. pointer
E983  8CC000                  cmpx  #romini                                           ; reached end of basic?
E986  25F1                    blo   bas1                                                      ; no, keep on copying basic
E988  8E3000          map1            ldx     #ramini                                         ; get rom origin address at ram area
E98B  CEC000                  ldu   #romini                                           ; get rom destination address
E98E  B7FFDF                  sta   $ffdf                                                     ; go map1 (ram) and stay there
E991  EC81            move    ldd   ,x++                                                      ; get two bytes, inc pointer
E993  EDC1                    std   ,u++                                                      ; store two bytes, inc pointer
E995  9C76                    cmpx  <endadr                                           ; reached last loaded address?
E997  2405                                    bhs   nohook                                            ; yes, quit loop
E999  8C6EFF                                  cmpx  #rammax                                           ; reached max rom possible (16k-256bytes)?
E99C  25F3                                    blo      move                                                   ; no, keep on copying rom
E99E  8639            nohook  lda   #$39                                                      ; opcode for rts
E9A0  8E015E                                  ldx   #hokini                                           ; get initial address of system hooks
E9A3  A780            hooks   sta   ,x+                                                       ; overwrite them
E9A5  8C01A8                  cmpx  #hokend                                           ; got hooks end?
E9A8  25F9                    blo   hooks                                                     ; no, keep on overwriting them
E9AA  CC444B                                  ldd     #$444b                                          ; sign for dos roms
E9AD  8EC000                                  ldx     #romini                                         ; beginning of the rom
E9B0  10A384                                  cmpd    ,x                                                              ; is a dos what we have loaded?
E9B3  2602                                    bne     bas2                                                    ; no, execute it
E9B5  3002                                    leax    2,x                                                     ; skip over dos signature
E9B7  6E84            bas2            jmp   ,x                                                                ; start loaded rom
                      
                      ; -------------------------------------------------------------------------------------------
E9B9  9612            doreadr         lda   <dnum                                                     ; our drive number
E9BB  5F                      clrb                                                                    ; lsn bits 23-16
E9BC  3436                    pshs  a,b,x,y                                           ; save registers / data
E9BE  86D2                    lda   #$d2                                                      ; dw op_readex (read one sector)
E9C0  3402            rereadr         pshs  a                                                         ; save command to stack
E9C2  30E4                    leax  ,s                                                                ; point x to top of stack
E9C4  108E0005                ldy   #5                                                                ; 5 bytes
E9C8  BDFB57                  jsr   DWWrite                                           ; send 5 bytes to dw      - readex, dnum, $0, sector
E9CB  3502                    puls  a                                                         ; discard command
E9CD  AE64                    ldx   4,s                                                       ; get read buffer pointer (old y)
E9CF  108E0100                ldy   #256                                                      ; read 256 bytes
E9D3  CC0085                  ldd   #133                                                      ; 1 second timeout
E9D6  BDFAD2                  jsr   DWRead                                            ; receive 256 bytes
E9D9  2534                    bcs   readex2                                           ; detect framing error
E9DB  2632                    bne   readex2                                           ; detect not all bytes received
E9DD  3420                    pshs  y                                                         ; save checksum
E9DF  30E4                    leax  ,s                                                                ; point x to top of stack
E9E1  108E0002                ldy   #2                                                                ; 2 bytes
E9E5  BDFB57                  jsr   DWWrite                                           ; send checksum
E9E8  3121                    leay  1,y                                                       ; after a dwrite 'y' returns zeroed, so this way it becomes equal to one
E9EA  CC0085                  ldd   #133                                                      ; 1 second timeout
E9ED  BDFAD2                  jsr   DWRead                                            ; read dw answer
E9F0  3262                    leas  2,s                                                       ; discard checksum
E9F2  251B                    bcs   readex2                                           ; detect framing error
E9F4  2619                    bne   readex2                                           ; detect not all bytes received
E9F6  A6E4                    lda   ,s                                                                ; get server answer
E9F8  2715                    beq   readex2                                           ; zero=ok, return
E9FA  81F3                    cmpa  #$f3                                                      ; error code is #e_crc?
E9FC  2606                    bne   reader                                            ; not this one, go verify eof condition
E9FE  86F2                    lda   #$f2                                                      ; command reread. #op_rereadex
EA00  6FE4                    clr   ,s                                                                ; clear received answer
EA02  20BC                    bra   rereadr                                           ; read again
EA04  81F4            reader  cmpa  #$f4                                      ; error code is 'read past end' (eof)?
EA06  2606                                    bne   noteof                                            ; no, mark read error
EA08  1CFC                    andcc #$fc                    ; unsets the 'c' and 'v' flags
EA0A  1A0C                    orcc  #$0c                                                      ; sets 'n' and 'z' flags. both prepare the flags to be positive at 'blt' test
EA0C  2001                    bra   readex2                                           ; jump over next instruction, not a read error
EA0E  53              noteof  comb                                                                    ; sets the carry flag (error)
EA0F  35B6            readex2         puls  a,b,x,y,pc                                        ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command to start a SDC-bank
                      ; -------------------------------------------------------------------------------------------------------------------------------
EA11  BDE771          xbank           jsr     getunit                                         ; read a number between 0-7
EA14  C107                                    cmpb    #7                                                              ; is it greater than 7?
EA16  2305                                    bls     OKbank                                          ; no, OK
EA18  C611            badnum  ldb     #$11                                                    ; SD error
EA1A  7E8344                                  jmp     syserr                                          ; bas_system_error
EA1D  1A50            OKbank  orcc    #dimask                                         ; disable interrupts
EA1F  D7F8                                    stb     <numUni                                         ; save received number in first sentence (B)
EA21  0FF9                                    clr     swMap1                                          ; by default don't change map type
EA23  9DA5                                    jsr     <redLChr                                                ; see next char after BANK
EA25  274B                                    beq     seeSlot                                         ; no extra chars, skip extra param search
EA27  812C                                    cmpa    #','                                                    ; is it a comma?
EA29  1026FD48                                lbne    snError                                         ; no, show error
EA2D  9D9F                                    jsr     <getNChr                                                ; get next char
EA2F  8152                                    cmpa    #'R'                                                    ; is it R (for RAM mode = MAP1)?
EA31  1026FD40                                lbne    snError                                         ; no, show error
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; copy and execute from RAM the 64k tester
EA35  8EEA4F                                  ldx     #tst64k                                         ; point to beginning of data to be copied
EA38  CE01DA                                  ldu     #$1da                                                   ; point to destination
EA3B  EC81            xb001           ldd     ,x++                                                    ; get a wrod
EA3D  EDC1                                    std     ,u++                                                    ; put at destination
EA3F  8CEA70                                  cmpx    #emod1B+2                                       ; got to the end?
EA42  25F7                                    blo     xb001                                                   ; no, loopback
EA44  BD01DA                                  jsr     $1da                                                    ; jsr that copied code from RAM
EA47  2627                                    bne     xb002                                                   ; if not zero, we have 64K
EA49  8EE57D                             ldx  #not64K-1                                       ; else show message NO 64K COMPUTER                     
EA4C  7E90E5                                  jmp     outstr                                          ; show message
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; validate if there are 64k - This must be executed from RAM
EA4F  8D0B            tst64k  bsr     mod1Byt                                         ; can modify rom area?
EA51  2608                                    bne     etst64k                                         ; yes, return result
EA53  B7FFDF                                  sta     $ffdf                                                   ; no, try switching to map1                     
EA56  8D04                                    bsr     mod1Byt                                         ; can modify area?
EA58  F7FFDE                                  stb     $ffde                                                   ; back to map0 where it was
EA5B  39              etst64k rts                                                                     ; return result
EA5C  5F              mod1Byt clrb                                                                    ; default 32K
EA5D  B6C000                                  lda     $c000                                                   ; get first DOS ROM char 
EA60  43                                      coma                                                                    ; invert bits
EA61  B7C000                                  sta     $c000                                                   ; update value
EA64  B1C000                                  cmpa    $c000                                                   ; has been updated?
EA67  2605                                    bne     emod1B                                          ; no, return
EA69  43                                      coma                                                                    ; invert bits again
EA6A  B7C000                                  sta     $c000                                                   ; restore value
EA6D  5C                                      incb                                                                    ; found 64k
EA6E  5D              emod1B  tstb                                                                    ; update flags
EA6F  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
EA70  0CF9            xb002           inc     swMap1                                          ; mark flag to change to map1
                                                                                                                      ; see if we have a SDC
EA72  B60114          seeSlot lda     floSDC                                          ; get hdw control byte
EA75  8508                                    bita    #%00001000                                      ; is there an SDC?
EA77  102700AC                                lbeq    NotSDCPresent                           ; no, show error
EA7B  84C0                                    anda    #%11000000                                      ; use bits 7-6 (slot where it is)
EA7D  44                                      lsra                                                                    ; move them to
EA7E  44                                      lsra                                                                    ; bits 5-4 (lower bits of high nibble)
EA7F  3402                                    pshs    a                                                               ; save high nibble
EA81  44                                      lsra                                                                    ; low nibble   
EA82  44                                      lsra                                                                    ; gets high nibble
EA83  44                                      lsra                                                                    ; that will end  
EA84  44                                      lsra                                                                    ; as zero
EA85  AAE0                                    ora     ,s+                                                     ; add high nibble (now they are the same value)
EA87  0DF9                                    tst     swMap1                                          ; should it be run in MAP1?
EA89  2616                                    bne     swMbank                                         ; yes, go to next section
EA8B  3402            swbank  pshs    a                                                               ; save SDC slot number
EA8D  8EEAB7                                  ldx     #special                                                ; point to the beginning of code to execute from RAM
EA90  CE01DA                                  ldu     #$1da                                                   ; point at destination
EA93  EC81            swB01           ldd     ,x++                                                    ; get a word
EA95  EDC1                                    std     ,u++                                                    ; copy at destination
EA97  8CEADE                                  cmpx    #exbank+3                                       ; got to the end?
EA9A  25F7                                    blo     swB01                                                   ; no, loopback
EA9C  3502                                    puls    a                                                               ; restore slot number
EA9E  7E01DA                                  jmp     $1da                                                    ; jumto to RAM copied code
EAA1  3402            swMbank pshs    a                                                               ; save SDC slot number
EAA3  8EEADE                                  ldx     #allRAM                                         ; point to the beginning of code to execute from RAM
EAA6  CE01DA                                  ldu     #$1da                                                   ; point at destination
EAA9  EC81            swMB01  ldd     ,x++                                                    ; get a word
EAAB  EDC1                                    std     ,u++                                                    ; copy at destination
EAAD  8CEB1B                                  cmpx    #m1L03+3                                                ; got to the end?
EAB0  25F7                                    blo     swMB01                                          ; no, loopback
EAB2  3502                                    puls    a                                                               ; restore slot number
EAB4  7E01DA                                  jmp     $1da                                                    ; jumto to RAM copied code
                                                                                                                      ; this TWO parts MUST be run from RAM because we are going
                                                                                                                      ; to lose control when switching the ROM slot and/or the bank!
                                                                                                                      ; code to start the bank in ROM mode (MAP0)
                                                                                                                      ; SDC slot# comes onto stack 
EAB7  1A50            special orcc    #dimask                                         ; disable interrupts
EAB9  7FFFDE                                  clr     $ffde                                                   ; switch to MAP0 (ROM-RAM mode)
EABC  B703FF                                  sta     cpyMPIcfg                                       ; update copy
EABF  B7FF7F                                  sta     MPIcfg                                          ; switch to the slot where the SDC is
EAC2  D6F8                                    ldb     <numUni                                         ; get requested bank number
EAC4  F7FF4B                                  stb     $ff4b                                                   ; point to it
EAC7  8E444B                                  ldx     #$444b                                          ; get mark for DOS system
EACA  BCC000                                  cmpx    romini                                          ; is there a DOS ROM?
EACD  270A                                    beq     spec01                                          ; yes, issue cold start
EACF  8E7F37                                  ldx     #$7f37                                          ; get std value for stack
EAD2  3284                                    leas    ,x                                                              ; assign it
EAD4  C4AF                                    andb    #eimask                                         ; enable interrupts
EAD6  7EC000                                  jmp     romini                                          ; no, execute ROMPACK
EAD9  0F71            spec01  clr     <$71                                                    ; force a cold boot
EADB  7EB3B4          exbank  jmp     resetRt                                         ; execute the system reset routine
                                                                                                                      ; code to start the bank but having copied everything into MAP1
                                                                                                                      ; SDC slot# comes onto stack 
EADE  1A50            allRAM  orcc  #dimask                           ; disable interrupts
EAE0  B703FF                                  sta     cpyMPIcfg                                       ; update copy
EAE3  B7FF7F                                  sta     MPIcfg                                          ; switch to the slot where the SDC is
EAE6  D6F8                                    ldb     <numUni                                         ; get requested bank number
EAE8  F7FF4B                                  stb     $ff4b                                                   ; point to it
                                                                                                                      ; copy ROM to RAM
EAEB  8E8000                                  ldx   #basini                           ; point to beginning of Basic
EAEE  B7FFDE          m1L01           sta   $ffde                             ; switch to ROM map
EAF1  EC84                                    ldd   ,x                                ; get a word
EAF3  B7FFDF                                  sta   $ffdf                             ; switch to RAM map
EAF6  ED81                                    std   ,x++                              ; copy at RAM destination
EAF8  8CFEFF                                  cmpx  #$feff                            ; end of ROM?
EAFB  25F1                                    bcs   m1L01                                     ; no, loopback
EAFD  8639                                    lda   #$39                              ; get RTS opcode
EAFF  8E015E                                  ldx   #$015e                            ; point to beginning of User vectors (hooks)
EB02  A780            m1L02           sta   ,x+                               ; put an RTS in each one
EB04  8C01A8                                  cmpx  #$01a8                            ; end of user vectors?
EB07  25F9                                    bcs   m1L02                                             ; no, loopback
EB09  BDBA77                                  jsr     $ba77                                                   ; clear screen
EB0C  CC444B                                  ldd     #$444b                                          ; get mark for O.S.
EB0F  10B3C000                                cmpd    romini                                          ; is there an O.S. in RAM now?
EB13  2703                                    beq     m1L03                                                   ; yes, start it
EB15  7EC000                                  jmp     romini                                          ; else start ROM from the beginning
EB18  7EC002          m1L03           jmp   romini+2                          ; start ROM (copied into RAM)
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                              ; MESSAGES. They all will return with carry set.
EB1B                  BadFormat
EB1B  8EE456                                  ldx     #BadFmtType-1                           ; point to message
EB1E  10                                      fcb     #$10                                                    ; skip next ldx
EB1F  8EE416          TooLong ldx     #Str2Long-1                                     ; point to message
EB22  10                                      fcb     #$10                                                    ; skip next ldx
EB23                  NotMPIPresent
EB23  8EE427                                  ldx     #NotAnMPI-1                                     ; point to message
EB26  10                                      fcb     #$10                                                    ; skip next ldx
EB27                  NotSDCPresent
EB27  8EE437                                  ldx     #NotAnSDC-1                                     ; point to message
EB2A  BD90E5                                  jsr     outstr                                          ; show it
EB2D  1A01                                    orcc    #%00000001                                      ; set carry flag
EB2F  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; This routine will copy the hardware tests routines at $1da and jump there 
                      ; to avoid hanging the system while switching of MPI slot
                      ; -------------------------------------------------------------------------------------------------------------------------------
EB30  8EEB43          tstHARD ldx     #tstHDW                                         ; point to beginning of program to be copied
EB33  CE01DA                                  ldu     #$1da                                                   ; point to destination
EB36  EC81            tstL01  ldd     ,x++                                                    ; get a word
EB38  EDC1                                    std     ,u++                                                    ; put at destination
EB3A  8CEC2B                                  cmpx    #xslot                                          ; got to end of program?
EB3D  25F7                                    blo     tstL01                                          ; no, loopback
EB3F  BD01DA                                  jsr     $1da                                                    ; execute copied program in low RAM
EB42  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; test for the presence of MPI, floppies, SDC and update $114 accordingly
                      ; -------------------------------------------------------------------------------------------------------------------------------
EB43  5F              tstHDW  clrb                                                                    ; get zero value
EB44  F70114                                  stb     floSDC                                          ; control byte default value (no hdw found)
EB47  170089                                  lbsr    tstMPI                                          ; is there an MPI present?
EB4A  10260084                                lbne    etstHDW                                         ; no, exit
EB4E  D776                                    stb     hdwFlo                                          ; no Floppies found
EB50  D777                                    stb     hdwSdc                                          ; no SDC found
                                                                                                                      ; search for SDC and/or floppies
EB52  B603FF                                  lda     cpyMPIcfg                                       ; get active MPI slot
EB55  C604                                    ldb     #4                                                              ; ready to check four slots
                                                                                                                      ; first look for SDC
EB57  0D77            tst01           tst     hdwSdc                                          ; have we found it before?
EB59  2611                                    bne     tst02                                                   ; yes, go to floppy test
EB5B  170099                                  lbsr    tstSDC                                          ; is there an SDC in this MPI slot?             
EB5E  260C                                    bne     tst02                                                   ; no, go to floppy test
EB60  9777                                    sta     hdwSdc                                          ; save slot where SDC has been found
EB62  B60114                                  lda     floSDC                                          ; get hdw control byte
EB65  8A08                                    ora     #%00001000                                      ; add flag for SDC present
EB67  B70114                                  sta     floSDC                                          ; update it
EB6A  2013                                    bra     tst03                                                   ; go process another slot
                                                                                                                      ; if not SDC, then for floppy
EB6C  0D76            tst02           tst     hdwFlo                                          ; have we found it before?
EB6E  260F                                    bne     tst03                                                   ; go process another slot
EB70  17009B                                  lbsr    tstFLO                                          ; is there a floppy controller in this MPI slot?
EB73  260A                                    bne     tst03                                                   ; no, process another slot
EB75  9776                                    sta     hdwFlo                                          ; save slot where floppy has been found
EB77  B60114                                  lda     floSDC                                          ; get hdw control byte
EB7A  8A04                                    ora     #%00000100                                      ; add flag for Floppies present
EB7C  B70114                                  sta     floSDC                                          ; update it
                                                                                                                      ; look at another slot
EB7F  B603FF          tst03           lda     cpyMPIcfg                                       ; get analized slot                                                                                                                                             ---     A = $33/$22/$11/$00
EB82  8011                                    suba    #$11                                                    ; calculate previous slot                                                                                                                               ---   A = $22/$11/$00/$EF
EB84  2A02                                    bpl     tst04                                           ; if not negative, skip next
EB86  8633                                    lda     #$33                                                    ; get upper value                                                                                                                                                       ---   A = $33
EB88  3402            tst04           pshs    a                                                               ; save new calculated slot number                                                                                                       ---   pushed $22/$11/$00/$33
EB8A  B603FF                                  lda     cpyMPIcfg                                       ; get last tested slot value                                                                                                                    ---        A = $33/$22/$11/$00
EB8D  84CC                                    anda    #$cc                                                    ; clean bits 0-1 in both nibbles                                                                                                                ---        A = $00/$00/$00/$00
EB8F  AAE0                                    ora     ,s+                                                     ; add new calculated slot number ($00-$11-$22-$33)                                                              ---        A = $22/$11/$00/$33
EB91  B703FF                                  sta     cpyMPIcfg                                       ; update copy
EB94  B7FF7F                                  sta     MPIcfg                                          ; change of I/O slot (not keeping the same ROM slot)                                                    ---  MPIcfg = $22/$11/$00/$33
EB97  5A                                      decb                                                                    ; decrement slot counter                                                                                                                                ---      B =   3/  2/  1/  0
EB98  26BD                                    bne     tst01                                                   ; if not zero, loopback
                                                                                                                      ; if zero: end of tests. MPIcfg must be restored to original value
EB9A  B60114          tst05           lda     floSDC                                          ; get hdw control byte
EB9D  8508                                    bita    #%00001000                                      ; is bit3 ON (SDC present)?
EB9F  271C                                    beq     tst07                                                   ; no, look for floppies 
EBA1  9677                                    lda     hdwSdc                                          ; get SDC slot                                                                                                                                                          --- A = $00
EBA3  B103FF                                  cmpa    cpyMPIcfg                                       ; equals copy of MPIcfg? 
EBA6  2608                                    bne     tst06                                                   ; no, don't modify floSDC
EBA8  F60114                                  ldb     floSDC                                          ; get hdw control byte                                                                                  
EBAB  CA03                                    orb     #%00000011                                      ; set floSDC.bits1-0 to #%11 (SDC ON)
EBAD  F70114                                  stb     floSDC                                          ; update control byte
EBB0  8403            tst06           anda    #%00000011                                      ; use just slot number                                                                                                                                  --- A = $00
EBB2  5F                                      clrb                                                                    ; result = 0                                                                                                                                                            --- B = 0
EBB3  44                                      lsra                                                                    ; send bit 0 to carry                                                                                                                                   --- A = 0, C = 0
EBB4  56                                      rorb                                                                    ; receive bit0 from carry in bit7                                                                                                       --- B = 0
EBB5  44                                      lsra                                                                    ; send bit 1 (now 0) to carry                                                                                                                   --- A = 0, C = 0
EBB6  56                                      rorb                                                                    ; receive bit1 (now 0) from carry in bit7 and bit6 goes to bit7                 --- B = 0
EBB7  FA0114                                  orb     floSDC                                          ; update bits 7-6 of $114 with slot used by SDC                                                                 --- floSDC.bits5-4 = %00
EBBA  F70114                                  stb     floSDC                                          ; save new value
EBBD  B60114          tst07           lda     floSDC                                          ; get hdw control byte
EBC0  8504                                    bita    #%00000100                                      ; is bit2 ON (floppies present)?
EBC2  270E                                    beq     etstHDW                                         ; no, exit
EBC4  9676                                    lda     hdwFlo                                          ; get floppies slot                                                                                                                                             --- A = $33
EBC6  8403                                    anda    #%00000011                                      ; get just slot number                                                                                                                                  --- A = $03
EBC8  48                                      lsla                                                                    ; move bits 0-1                                                                                                                                                 --- A = $06
EBC9  48                                      lsla                                                                    ; to bits 5-4                                                                                                                                                           --- A = $0C
EBCA  48                                      lsla                                                                    ;                                                                                                                                                                                               --- A = $18
EBCB  48                                      lsla                                                                    ;                                                                                                                                                                                               --- A = $30
EBCC  BA0114                                  ora     floSDC                                          ; update bits 5-4 of $114 with slot used by Floppies                                                    --- floSDC.bits7-6 = %11
EBCF  B70114                                  sta     floSDC                                          ; save new value
EBD2  39              etstHDW rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Tests for the presence of an MPI
                      ; returns zero flag Z=1 if present else Z=0
                      ; by Darren Atkinson (2015)
                      ; modified to support mega-mini-MPI by Pere Serrat (2021)
                      ; -------------------------------------------------------------------------------------------------------------------------------
EBD3  3402            tstMPI  pshs    a                                                               ; save register
EBD5  B603FF                                  lda     cpyMPIcfg                               ; get MPI settings
EBD8  8803                                    eora  #$03                                              ; toggle two low used bits in /SCS nibble
EBDA  B703FF                                  sta     cpyMPIcfg                                       ; update copy
EBDD  B7FF7F                                  sta   MPIcfg                                            ; update MPI settings
EBE0  B1FF7F                                  cmpa  MPIcfg                                            ; did it accept the change? MANDATORY. Do not CHANGE it!
EBE3  2704                                    beq   yesMPI                                            ; branch if yes (MPI present), will return with Z=1
EBE5  8601                                    lda     #1                                                              ; will return with Z=0
EBE7  200C                                    bra     noMPI                                                   ; exit
EBE9  B603FF          yesMPI  lda     cpyMPIcfg                                       ; get old value
EBEC  8803                                    eora    #$03                                                    ; recover modified bits in /SCS
EBEE  B703FF                                  sta     cpyMPIcfg                                       ; update copy
EBF1  B7FF7F                                  sta     MPIcfg                                          ; back to its old value
EBF4  4F                                      clra                                                                    ; make Z=1
EBF5  3582            noMPI           puls    a,pc                                                    ; restore register and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Tests for the presence of a CoCo SDC on active slot
                      ; returns zero flag Z=1 if present else to zero
                      ; by Darren Atkinson (2015)
                      ; -------------------------------------------------------------------------------------------------------------------------------
EBF7  3412            tstSDC  pshs    a,x                                                     ; save registers
EBF9  8EFF4A                          ldx   #$ff4a                                            ; point X at Flash registers (FF42 for CoCo)
EBFC  861A                            lda   #$1a                                              ; test pattern
EBFE  A784                            sta   ,x                                                ; store in data register
EC00  A601                            lda   1,x                                               ; get value from control register
EC02  6F84                            clr   ,x                                                ; clear data register
EC04  A801                            eora  1,x                                               ; get changed bits from control register
EC06  8118                            cmpa  #$18                                              ; did the expected bits change?
EC08  2702                            beq   yesSDC                                            ; branch if SDC present, will return with Z=1
EC0A  8601                                    lda     #1                                                              ; will return with Z=0
EC0C  3592            yesSDC  puls    a,x,pc                                          ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Test for presence of FDC in active MPI slot.
                      ; Returns with Z=1 if FDC has been found
                      ; by Darren Atkinson (2015)
                      ; -------------------------------------------------------------------------------------------------------------------------------
EC0E  3416            tstFLO   pshs  d,x                              ; save registers
EC10  8EFF40                   ldx   #$ff40                           ; address for Dragon DOS controller
EC13  86D0                     lda   #$d0                             ; FORCE INTERRUPT command
EC15  A784                     sta   ,x                               ; send to FDC command register
EC17  8D0D                     bsr   fdcDelay                         ; delay time
EC19  A684                     lda   ,x                               ; read FDC status to clear INTRQ
EC1B  EC02                     ldd   2,x                              ; get FDC sector and data registers
EC1D  43                       coma                                   ; invert sector bits only
EC1E  ED02                     std   2,x                              ; put both registers back
EC20  8D04                     bsr   fdcDelay                         ; delay time
EC22  A302                     subd  2,x                              ; set Z if read matches write
EC24  3596                     puls  d,x,pc                           ; restore registers and return
EC26  3437            fdcDelay pshs  y,x,d,cc                         ; push registers
EC28  3D                       mul                                    ; consume 11 CPU cycles
EC29  35B7                     puls  cc,d,x,y,pc                      ; restore registers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command to change of MPI-slot and start it 
                      ; it could happen that the new slot has no FDC nor SDC
                      ; but if it had any of them, then floSDC must be adjusted accordingly
                      ; -------------------------------------------------------------------------------------------------------------------------------
EC2B  9DA5            xslot           jsr   <redLChr                                          ; peek at next character after "SLOT"
EC2D  804E                    suba  #'N'                                                      ; to detect if it was an 'N' - if so do not switch to MAP0
EC2F  9710                    sta   <noexec                                           ; save difference. 0 = do not switch
EC31  2602                    bne   xslot1                                            ; if not 'N' skip next
EC33  9D9F                    jsr   <getNChr                                          ; jump over char 'N'
EC35  BDE771          xslot1  jsr     getunit                                         ; read a number between 1-4
EC38  2704                                    beq     nogood                                          ; if 0 error
EC3A  C104                                    cmpb    #4                                                              ; is it greater than 4?
EC3C  2305                                    bls     OKslot                                          ; no, OK
EC3E  C60D            nogood  ldb     #$0d                                                    ; MU error
EC40  7E8344                                  jmp     syserr                                          ; bas_system_error
EC43  8D8E            OKslot  bsr     tstMPI                                          ; is there an MPI present?
EC45  2704                                    beq     OKslot01                                                ; yes, skip section
EC47  17FED9                                  lbsr    NotMPIPresent                           ; no, show message
EC4A  39                                      rts                                                                     ; return
EC4B  5A              OKslot01        decb                                                                    ; slots range 0-3. We use reg B read at function getunit
EC4C  D7F8                                    stb     <numUni                                         ; save low nibble --- $00 - $03 if command was SLOTN1 - SLOTN4
EC4E  58                                      lslb                                                                    ; move it                       --- $00 - $06
EC4F  58                                      lslb                                                                    ; to the                        --- $00 - $0c
EC50  58                                      lslb                                                                    ; high                          --- $00 - $18
EC51  58                                      lslb                                                                    ; nibble                                --- $00 - $30
EC52  DAF8                                    orb     <numUni                                         ; add low nibble ($nn)          --- $00 - $33
EC54  D7F8                                    stb     <numUni                                         ; save calculated code          --- $00 - $33
                                                                                                                      ; now must compare the new slot with the one for FDC and SDC and modify floSDC accordingly
                                                                                                                      ; by definition, switching the slot MUST enable the drives of that slot, so $114 must vary
EC56  C430                                    andb    #%00110000                                      ; get just low bits of high nibble = new slot number    --- $00 - $30
EC58  3404                                    pshs    b                                                               ; save value                                                                                                            --- $00 - $30
EC5A  B60114                                  lda     floSDC                                          ; get hdw control byte                                                                                  --- $3C - $3C if we are in the FDC
EC5D  8430                                    anda    #%00110000                                      ; get the slot where the Floppies are                                           --- $30 - $30
EC5F  A1E0                                    cmpa    ,s+                                                     ; is new slot where the floppies are?                                           --- NO  - YES
EC61  2607                                    bne     xslot2                                          ; no, compare SDC                                                                                                       --- jmp - mod
EC63  B60114                                  lda     floSDC                                          ; get control value
EC66  84FC                                    anda    #%11111100                                      ; set both floppies ON
EC68  2016                                    bra     xslot3                                          ; skip section - no need to verify SDC
EC6A  96F8            xslot2  lda     <numUni                                         ; get new slot number                                                                                   --- $00 - $33
EC6C  8430                                    anda    #%00110000                                      ; get just low bits of high nibble = new slot number    --- $00 - $30
EC6E  48                                      asla                                                                    ; move 2 bits to 
EC6F  48                                      asla                                                                    ; the left                                                                                                                      --- $00 - $00
EC70  3402                                    pshs    a                                                               ; save value                                                                                                            --- $00 - $00
EC72  B60114                                  lda     floSDC                                          ; get control bte
EC75  84C0                                    anda    #%11000000                                      ; get the slot where SD is                                                                              --- $00 - $00
EC77  A1E0                                    cmpa    ,s+                                                     ; is the select one?                                                                                            --- YES - NO
EC79  2608                                    bne     xslot4                                          ; no, skip two next                                                                                             --- mod - jump
EC7B  B60114                                  lda     floSDC                                          ; get control value
EC7E  8A03                                    ora     #%00000011                                      ; set both floppies OFF
EC80  B70114          xslot3  sta     floSDC                                          ; update hdw control byte
                                                                                                                              ; this part MUST be run from RAM because we are going
                                                                                                                              ; to lose control when switching the ROM slot!
EC83  8EEC95          xslot4  ldx     #specia2                                                ; point to beginning of code to execute from RAM
EC86  CE01DA                                  ldu     #$1da                                                   ; point at destination
EC89  EC81            xsL01           ldd     ,x++                                                    ; get a word
EC8B  EDC1                                    std     ,u++                                                    ; put at destination
EC8D  8CECB9                                  cmpx    #strROM+3                                       ; got to the end?
EC90  25F7                                    blo     xsL01                                                   ; no, loopback
EC92  7E01DA                                  jmp     $1da                                                    ; jump to RAM copied code
EC95  1A50            specia2 orcc    #dimask                                         ; disable interrupts
EC97  0D10                                    tst     <noexec                                         ; should switch to MAP0?                                                                --- noexec = 0
EC99  2703                                    beq     specia3                                         ; no, skip next                                                                                 --- apply jump
EC9B  7FFFDE                                  clr     $ffde                                                   ; switch to MAP0 (ROM mode)                                                     --- skipped
EC9E  D6F8            specia3 ldb     <numUni                                         ; get calculated code                                                                   --- B = $cc
ECA0  F703FF                                  stb     cpyMPIcfg                                       ; update copy
ECA3  F7FF7F                                  stb     MPIcfg                                          ; switch to that slot                                                                   --- MPIcfg = $cc
ECA6  0D10                                    tst     <noexec                                         ; called with param N?
ECA8  2601                                    bne     eOKSlot                                         ; no, start slot
ECAA  39                                      rts                                                                     ; return
ECAB  CC444B          eOKSlot ldd     #$444b                                          ; signature for O.S.
ECAE  10B3C000                                cmpd    romini                                          ; is an O.S. at $C000?
ECB2  2602                                    bne     strROM                                          ; no, start RomPack
ECB4  0F71                                    clr     <$71                                                    ; force coldstart
ECB6  7EB3B4          strROM  jmp     resetRt                                         ; reset machine
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; to access floppies or SDC-card
                      ; uses byte floSDC ($114) to determine what it must access
                      ; byte structure: 
                      ;       bit 7-6 - slot number where is the CoCo-SDC  (0-3)
                      ;       bit 5-4 - slot number where are the floppies (0-3)
                      ;       bit 3           - 1 if SDC present else 0
                      ;       bit 2           - 1 if Floppy controller present else 0
                      ;       bit 1           - for Unit #2: if 0 use floppy, if 1 use SDC
                      ;       bit 0           - for Unit #1: if 0 use floppy, if 1 use SDC
                      ; -------------------------------------------------------------------------------------------------------------------------------
ECB9  3401            flOrSd  pshs    cc                                                              ; save register
ECBB  B60114                                  lda     floSDC                                          ; get hdw control byte
ECBE  840C                                    anda    #%00001100                                      ; test bits3-2 (SDC and Floppies present)
ECC0  810C                                    cmpa    #%00001100                                      ; are both present?
ECC2  263B                                    bne     eflOr                                                   ; no, exit
ECC4  96EB                                    lda     <$eb                                                    ; get unit number
ECC6  8102                                    cmpa    #2                                                              ; is greater than 2?  --- If 3-4 and SDC present, should change to FDC slot!
ECC8  221D                                    bhi     flo2flo                                         ; yes, switch to floppies
ECCA  B60114                                  lda     floSDC                                          ; get hdw control byte
ECCD  94EB                                    anda    <$eb                                                    ; test corresponding bit 0-1 (affected drive)
ECCF  2716                                    beq     flo2flo                                         ; if zero, switch to floppies
                                                                                                                      ; has to switch to slot with SDC (bits 7-6 of floSDC)
ECD1  B60114          flo2SDC lda     floSDC                                          ; get hdw control byte
ECD4  84C0                                    anda    #%11000000                                      ; use bits 7-6 to get slot number
ECD6  44                                      lsra                                                                    ; send them
ECD7  44                                      lsra                                                                    ; to bits 5-4 (2 low bits of high nibble)
ECD8  3402                                    pshs    a                                                               ; save high nibble
ECDA  44                                      lsra                                                                    ; move it
ECDB  44                                      lsra                                                                    ; 4 times to the right
ECDC  44                                      lsra                                                                    ; to have it
ECDD  44                                      lsra                                                                    ; as low nibble
ECDE  AAE0                                    ora     ,s+                                                     ; add high nibble and clean stack
ECE0  B103FF                                  cmpa    cpyMPIcfg                                       ; already in that slot?
ECE3  271A                                    beq     eflOr                                                   ; yes, do nothing
ECE5  2012                                    bra     flOr01                                          ; go update value
                                                                                                                      ; has to switch to slot with floppies (bits 5-4- of floSDC)
ECE7  B60114          flo2flo lda     floSDC                                          ; get hdw control byte
ECEA  8430                                    anda    #%00110000                                      ; use bits 5-4 (2 low bits of high nibble)
ECEC  3402                                    pshs    a                                                               ; save high nibble
ECEE  44                                      lsra                                                                    ; move it
ECEF  44                                      lsra                                                                    ; 4 times to the right
ECF0  44                                      lsra                                                                    ; to have it
ECF1  44                                      lsra                                                                    ; as low nibble
ECF2  AAE0                                    ora     ,s+                                                     ; add high nibble and clean stack
ECF4  B103FF                                  cmpa    cpyMPIcfg                                       ; already in that slot?
ECF7  2706                                    beq     eflOr                                                   ; yes, do nothing
ECF9  B703FF          flOr01  sta     cpyMPIcfg                                       ; update copy
ECFC  B7FF7F                                  sta     MPIcfg                                          ; change active MPI slot
ECFF  3581            eflOr           puls    cc,pc                                                   ; restore register and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; accept a value to be used as index disc in a HD in unit4 (for DW4)
                      ; -------------------------------------------------------------------------------------------------------------------------------
ED01  BDE771          xHDidx  jsr     getunit                                         ; read a number between 1-90
ED04  C101                                    cmpb    #1                                                              ; value under minimum accepted?
ED06  10250858                                lblo    badParam                                                ; yes, show message
ED0A  C15A                                    cmpb    #90                                                     ; exceeds the max index number?
ED0C  2207                                    bhi     idxBad                                          ; yes, show message
ED0E  F70115                                  stb     idxHD                                                   ; save disc index number
ED11  BDE696                                  jsr     mrkmod                                          ; mark as modified
ED14  39              exHD            rts                                                                     ; return
ED15  8EE46E          idxBad  ldx     #idxHigh-1                                      ; poit to message
ED18  7E90E5                                  jmp     outstr                                          ; show it
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Command to deal with SDC-card DRIVE commands (1,2)
                      ; -------------------------------------------------------------------------------------------------------------------------------
ED1B  9DA5            xsdrive jsr     <redLChr                                                ; peek at next char after SDRIVE
ED1D  102704BC                                lbeq    sdcLST                                          ; is the SDRIVE alone
                                                                                                                      ; validate syntax and drive number
ED21  BDE771                                  jsr     getunit                                         ; read a number between 1-2
ED24  1027FCF0                                lbeq    badnum                                          ; 0 not accepted
ED28  C102                                    cmpb    #2                                                              ; is it greater than 4?
ED2A  1022FCEA                                lbhi    badnum                                          ; no, ok
                                                                                                                      ; update table of linked DW4 slots back to the number of the drive!     
ED2E  8E00FB                                  ldx     #drvtab-1                                       ; point to linked slots -1, because we will use 1-2 as index (instead 0-1)
ED31  E785                                    stb     b,x                                                     ; link unit to DW4 slot with its same number
ED33  5A                                      decb                                                                    ; internally will work with 0-1
ED34  D7F8                                    stb     <numUni                                         ; save received number
ED36  0FFA                                    clr     <valPar                                         ; value param is 0
ED38  C6E0                                    ldb     #$e0                                                    ; more used value
ED3A  D7FB                                    stb     <cmdNum                                         ; store it
ED3C  BD89AA                                  jsr     ckComma                                         ; ckcomma. if not comma found syntax error
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; decode token
ED3F  81C2                                    cmpa    #$c2                                                    ; is this the token for off?
ED41  1027034F                                lbeq    flopON                                          ; yes, go process it
ED45  8188                                    cmpa    #$88                                                    ; is this the token for on?
ED47  10270340                                lbeq    flopOF                                          ; yes, go process it
ED4B  81B3                                    cmpa    #$b3                                                    ; is this the token for get?
ED4D  10270370                                lbeq    sdcGET                                          ; yes, go process it
ED51  818C                                    cmpa    #$8c                                                    ; is this the token for dim?
ED53  1027035C                                lbeq    sdcDIM                                          ; yes, go process it
ED57  81F6                                    cmpa    #$f6                                                    ; is this the token for unload?
ED59  260B                                    bne     nxtParam                                                ; no, go to params section
ED5B  9D9F                                    jsr     <getNChr                                                ; yes, it is the 'unload' command version. must skip the token to avoid a sn error when done
ED5D  CC0100          reuseUL ldd     #$0100                                          ; get 1 byte with value equal null
ED60  FD01D1                                  std     nambuf                                          ; put in nambuf area
ED63  8D60            dTok01  bsr     putCmd                                          ; skip parameters section
ED65  39                                      rts                                                                     ; return
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      
                                                                                                                      ; search for more parameters
ED66  BDB7AA          nxtParam        jsr   getfnam                                           ; read string and length into $1d1 = nambuf
ED69  F601D1                  ldb     nambuf                                          ; is string length 0?
ED6C  1027FA05                lbeq    snError                                         ; yes, show error
ED70  C1FD                    cmpb    #253                                                    ; is string longer than maximum accepted? (253 + m: + final null)
ED72  1024FDA9                                lbcc    TooLong                                         ; show error message
ED76  9DA5                                    jsr     <redLChr                                                ; get last read char
ED78  812C                                    cmpa    #$2c                                                    ; is it a comma?
ED7A  26E7                                    bne     dTok01                                          ; no, end of command. this is 'mount' command version
ED7C  9D9F                                    jsr     <getNChr                                                ; get next token
ED7E  1027F9F3                                lbeq    snError                                         ; if no token after comma, show error
ED82  5F                                      clrb                                                                    ; bit 7 value 0 means dsk format
ED83  8197                                    cmpa    #$97                                                    ; is it the token for new?
ED85  2708                                    beq     crtDisc                                         ; yes, create a dsk file
ED87  CA80                                    orb     #$80                                                    ; bit 7 value 1 means sdf format
ED89  8198                                    cmpa    #$98                                                    ; is it the token for def?
ED8B  1026F9E6                                lbne    snError                                         ; no, show error message
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; create disc
ED8F  D710            crtDisc stb     <noexec                                         ; save format flag (bit7)
ED91  17050A                                  lbsr    addExt                                          ; add extension to the file name
ED94  9D9F                                    jsr     <getNChr                                                ; get next char
ED96  812C                                    cmpa    #$2c                                                    ; is it a comma?
ED98  2707                                    beq     crt01                                                   ; yes, test size parameter
ED9A  8601                                    lda     #1                                                              ; by default type=1
ED9C  8E00B4                                  ldx     #180                                                    ; and will be 180k (720 sectors)
ED9F  201A                                    bra     crtstep                                         ; exit cheks
EDA1  9D9F            crt01           jsr     <getNChr                                                ; advance one char. mandatory for next routine
EDA3  BD8E83                                  jsr     get16bN                                         ; get a 16 bit number into x
EDA6  8601                                    lda     #1                                                              ; 1 will mean 180k dragon-like disc (40 tracks 1 side)
EDA8  8C00B4                                  cmpx    #180                                                    ; equals read value 180?
EDAB  270E                                    beq     crtstep                                         ; yes, exit checks
EDAD  4C                                      inca                                                                    ; 2 will mean 360k dragon-like disc (40 tracks 2 sides)
EDAE  8C0168                                  cmpx    #360                                                    ; equals read value 360?
EDB1  2708                                    beq     crtstep                                         ; yes, exit checks
EDB3  4C                                      inca                                                                    ; 3 will mean 720k dragon-like disc (80 tracks 2 sides)
EDB4  8C02D0                                  cmpx    #720                                                    ; equals read value 720?
EDB7  1026FD60                                lbne    BadFormat                                       ; not, bad format number, show error
EDBB  9B10            crtstep adda    <noexec                                         ; add disc type to format flag
EDBD  9710                                    sta     <noexec                                         ; save calculated value ($01-02-03 or $81-82-83)
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; entries to send command to the sdc
                                                                                                                      ; entry for create new disc comands
EDBF  CC4E3A                                  ldd     #"N:"                                                   ; command to be sent to create a file
EDC2  8D06                                    bsr     putCmd2                                         ; send command
EDC4  39              noMsgEr rts                                                                     ; return
                                                                                                                      ; entry for mount - unmount like commands
EDC5  0F10            putCmd          clr     <noexec                                         ; no params needed (no disk to be created)
EDC7  CC4D3A                                  ldd     #"M:"                                                   ; command to be sent to mount or unmount a drive
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; detect if sdc is already active
EDCA  0F77            putCmd2 clr     hdwSdc                                          ; no need to go back to old active slot
EDCC  17FE28                                  lbsr    tstSDC                                          ; is there an active sdc?
EDCF  2733                                    beq     xsd01                                                   ; yes, go on
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; if present but not active change to that slot, save old one                   
                                                                                                                      ; if sdc present, active it and mark somewhere that we must go back to the active one!
EDD1  3406                                    pshs    d                                                               ; save registers
EDD3  B60114                                  lda     floSDC                                          ; get hdw control byte
EDD6  8508                                    bita    #%00001000                                      ; is a sdc in the system?
EDD8  260E                                    bne     vfySD01                                         ; yes, jump section
EDDA  3264                                    leas    4,s                                                     ; get rid of d and 1 return address
EDDC  AEE4                                    ldx     ,s                                                              ; see if return address is $0900 (from sdir?)
EDDE  8C0900                                  cmpx    #$0900                                          ; comes from sdir?
EDE1  2602                                    bne     vfySD00                                         ; no, skip next
EDE3  3262                                    leas    2,s                                                     ; clean stack                                                                   
EDE5  16FD3F          vfySD00 lbra    NotSDCPresent                           ; no, show message
                                                                                                                      ; calculate sdc slot
EDE8  B60114          vfySD01 lda     floSDC                                          ; get hdw control byte
EDEB  84C0                                    anda    #%11000000                                      ; use bits 7-6 (slot where sdc is)
EDED  44                                      lsra                                                                    ; move them to
EDEE  44                                      lsra                                                                    ; bits 5-4 (lower bits of high nibble)
EDEF  3402                                    pshs    a                                                               ; save high nibble
EDF1  44                                      lsra                                                                    ; low nibble   
EDF2  44                                      lsra                                                                    ; gets high nibble
EDF3  44                                      lsra                                                                    ; that will end  
EDF4  44                                      lsra                                                                    ; as zero
EDF5  AAE0                                    ora     ,s+                                                     ; add high nibble (now they are the same value)
                                                                                                                      ; save active slot in hdwsdc
EDF7  F603FF                                  ldb     cpyMPIcfg                                       ; get active slot
EDFA  D777                                    stb     hdwSdc                                          ; save it for later use                                         
EDFC  B703FF                                  sta     cpyMPIcfg                                       ; update copy
EDFF  B7FF7F                                  sta     MPIcfg                                          ; activate sdc
EE02  3506                                    puls    d                                                               ; restore registers
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; pass string to data buffer
EE04  8E01D2          xsd01           ldx   #nambuf+1                                 ; point to 1st char of the input string
EE07  CE0800                                  ldu     #$0800                                          ; point to 1st dos buffer
EE0A  EDC1                                    std     ,u++                                                    ; put at the beginning of the command buffer
EE0C  F601D1                                  ldb     nambuf                                          ; get string length
EE0F  A680            str2buf lda     ,x+                                                     ; get a char from input string
EE11  A7C0                                    sta     ,u+                                                     ; copy in the buffer
EE13  5A                                      decb                                                                    ; decrement counter
EE14  26F9                                    bne     str2buf                                         ; not yet done, loopback
EE16  6FC4                                    clr     ,u                                                              ; put a final zero
EE18  0D10                                    tst     <noexec                                         ; was sdf format?
EE1A  2B06                                    bmi     addParms                                                ; yes, put params: cylinders and sides into b:x
                                                                                                                              ; the other varians send these registers with zero value                        
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; params to be passed (b:x)
EE1C  D6FA                                    ldb     <valPar                                         ; get b parameter value
EE1E  9E8A                                    ldx     <$8a                                                    ; 16 bits zero                                  
EE20  2015                                    bra     sendIt                                          ; send the command
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; special parameters for command 'create disc' (b:x)
EE22  9610            addParms        lda     <noexec                                         ; get disc type
EE24  8403                                    anda    #$03                                                    ; use lowest nibble
EE26  C628                                    ldb     #$28                                                    ; minimum value 40 cylinders
EE28  8103                                    cmpa    #3                                                              ; was type 3 (2880 sectors)?
EE2A  2601                                    bne     addP01                                          ; no, skip next
EE2C  58                                      aslb                                                                    ; double cylinders number
EE2D  8E0100          addP01  ldx     #$0100                                          ; one side by default
EE30  8101                                    cmpa    #1                                                              ; was type 0 or 1 (630 - 720 sectors)?
EE32  2303                                    bls     sendIt                                          ; yes, skip next
EE34  8E0200                                  ldx     #$0200                                          ; set double sided
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; send command to the sdc
                                                                                                                              ; b:x have zeros or tracks:sides (at x-high byte)
EE37  96FB            sendIt  lda     <cmdNum                                         ; code for extended command with data block
EE39  9BF8                                    adda    <numUni                                         ; add drive number
EE3B  CE0800                                  ldu     #$0800                                          ; point to beginning of data block
EE3E  BDF819                                  jsr     CommSDC                                         ; send command to sdc-card with (a,b:x,u)
EE41  2438                                    bcc     send01                                          ; if ok, skip section
                                                                                                                      ; if error $10 and sdir command, show message (not error)
EE43  C510                                    bitb    #$10                                                    ; is bit 4 set?
EE45  2714                                    beq     nxtCEr                                          ; no, goto next control
EE47  3406                                    pshs    d                                                               ; save registers
EE49  96FA                                    lda     <valPar                                         ; get parameter used
EE4B  8143                                    cmpa    #'C'                                                    ; was it a 'C' (sdir)?
EE4D  2608                                    bne     send00                                          ; no, exit
EE4F  8D19                                    bsr     currentDirIsRoot                        ; show message
EE51  3506                                    puls    d                                                               ; restore registers
EE53  1A01                                    orcc    #%00000001                                      ; set carry to mark error
EE55  2027                                    bra     send02                                          ; exit
EE57  3506            send00  puls    d                                                               ; clean stack
EE59  2020                                    bra     send01                                          ; exit
                                                                                                                      ; if error $08 and get-dim command, show message (not error)
EE5B  C508            nxtCEr  bitb    #$08                                                    ; is bit 3 set?
EE5D  271C                                    beq     send01                                          ; no, skip section
EE5F  84F0                                    anda    #$f0                                                    ; use high nibble
EE61  81C0                                    cmpa    #$c0                                                    ; was command $cn (get-dim)?
EE63  2616                                    bne     send01                                          ; no, go on
EE65  1A01                                    orcc    #%00000001                                      ; set carry to mark error
EE67  160220                                  lbra    exsdrv1                                         ; exit
EE6A                  currentDirIsRoot                                                                ; current dir = root!
EE6A  96F9                                    lda     <numDir                                         ; get number of directories requested in a series
EE6C  8101                                    cmpa    #1                                                              ; is first one?
EE6E  260A                                    bne     curNoMsg                                                ; no, do not show message
EE70  3450                                    pshs    x,u                                                     ; save registers
EE72  8EE47E                                  ldx     #noCurDir-1                                     ; point to message
EE75  BD90E5                                  jsr     outstr                                          ; show it
EE78  3550                                    puls    x,u                                                     ; restore registers
EE7A  39              curNoMsg        rts                                                                     ; return
EE7B  17096B          send01  lbsr    vfyStatus                                       ; verify operation result
EE7E  0D10            send02  tst     <noexec                                         ; test format type
EE80  10270206                                lbeq    exsdrv1                                         ; if coco-like 160k, all done, exit. should never happen
EE84  102B0202                                lbmi    exsdrv1                                         ; if sdf type, all done, exit   
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; additional swrite command for dragon-like disks
                                                                                                                      ; these three need a swrite,39(79),18(36),"",""
                                                                                                                      ; x must have the lsn number: 719,1439,2879
EE88  8E02CF                                  ldx     #719                                                    ; to have 720 sectors
EE8B  9610                                    lda     <noexec                                         ; get disc type
EE8D  8403                                    anda    #3                                                              ; use lowest 2 bits
EE8F  8101                                    cmpa    #1                                                              ; is it a 180k (720 sectors) disc? 
EE91  270A                                    beq     sendWrt                                         ; yes, send write order
EE93  8E059F                                  ldx     #1439                                                   ; to force 1440 sectors
EE96  8102                                    cmpa    #2                                                              ; is it a 360k (1440 sectors) disc?
EE98  2703                                    beq     sendWrt                                         ; yes, send write order
EE9A  8E0B3F                                  ldx     #2879                                                   ; to force 2880 sectors
                                                                                                      ; *** ADDED CODE TO SUPPORT VDK FORMAT CREATING FILES   
                                                                                                                      ; here we must test the file extension to be ".VDK"
EE9D  0F76            sendWrt clr     <dsktyp                                         ; Now equals 0 = DSK
EE9F  CE01D1                                  ldu     #nambuf                                         ; point to filename buffer
EEA2  E6C4                                    ldb     ,u                                                              ; get filename length
EEA4  C003                                    subb    #3                                                              ; to point to the possible dot (.) if .VDK was entered
EEA6  33C5                                    leau    b,u                                                     ; point to it
EEA8  CC2E56                                  ldd     #".V"                                                   ; get dot and letter V
EEAB  10A3C1                                  cmpd    ,u++                                                    ; have we received it?
EEAE  260C                                    bne     sWrt01                                          ; no, skip section
EEB0  CC444B                                  ldd     #"DK"                                                   ; get string DK
EEB3  10A3C4                                  cmpd    ,u                                                              ; have we received it?
EEB6  2604                                    bne     sWrt01                                          ; no, skip section
                                                                                                                      ; now we know the user wants to create a virtual VDK file
EEB8  3001                                    leax    1,x                                                     ; so that the disk has an extra sector (for the header)                                                                 
EEBA  0C76                                    inc     <dsktyp                                         ; now is 1 = VDK
                                                                                                      ; *** END_ADDED CODE TO SUPPORT VDK FORMAT CREATING FILES       
EEBC  CE0800          sWrt01  ldu     #$0800                                          ; point to beginning of buffer
EEBF  4F                                      clra                                                                    ; create a  
EEC0  5F                                      clrb                                                                    ; 16 bits zero
EEC1  EDC1            cleanBuf        std     ,u++                                                    ; clean a word
EEC3  11830900                                cmpu    #$0900                                          ; reached buffers end?
EEC7  25F8                                    blo     cleanBuf                                                ; no, loopback
EEC9  CE0800                                  ldu     #$0800                                          ; point to buffers beginning
EECC  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EECE  9BF8                                    adda    <numUni                                         ; add drive number
EED0  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EED3  170913                                  lbsr    vfyStatus                                       ; verify operation result
                                                                                                      ; *** ADDED CODE TO FORMAT ON THE FLY DSK and VDK FILES
                                                                                                                      ; as the buffer is clean, we just need to fill bytes 0-19
                                                                                                                              ; for a VDK file, we will write a suitable header to sector #0
                                                                                                                              ; it should contain:
                                                                                                                              ;  position             length contents                                  value
EED6  CC646B                                  ldd     #"dk"                                                   ;       offset 0     2     signature               ('dk')
EED9  EDC1                                    std     ,u++
EEDB  CC0001                                  ldd     #$0001                                          ;       offset 2     2     Header length           ($00, $01) = $100
EEDE  EDC1                                    std     ,u++
EEE0  CC1010                                  ldd     #$1010                                          ;       offset 4     1     VDK version - actual    ($10)
EEE3  EDC1                                    std     ,u++                                                    ;       offset 5     1     VDK version - compat    ($10)
EEE5  CC8800                                  ldd     #$8800                                          ;       offset 6     1     Source id               ($88)
EEE8  EDC1                                    std     ,u++                                                    ;       offset 7     1     Source version          ($00)
EEEA  108E2801                                ldy     #$2801                                          ; 40 tracks 01 side (180k)
EEEE  9610                                    lda     <noexec                                         ; get disc type
EEF0  8403                                    anda    #3                                                              ; use lowest 2 bits
EEF2  8101                                    cmpa    #1                                                              ; is it a 180k disc? 
EEF4  270A                                    beq     noMore2                                         ; yes, send write order
EEF6  3121                                    leay    1,y                                                     ; to force double sided ($2802)
EEF8  8102                                    cmpa    #2                                                              ; is it a 360k disc?
EEFA  2704                                    beq     noMore2                                         ; yes, send write order
EEFC  108E5002                                ldy     #$5002                                          ; it is a 720k disk
EF00  1F20            noMore2 tfr     y,d                                                     ; pass geodata to regD
EF02  97F2                                    sta     <$f2                                                    ; number of tracks to DOS system variable
EF04  5A                                      decb                                                                    ; because 0 means 1 side
EF05  D7F4                                    stb     <$f4                                                    ; number of sides to DOS system variable
                                                                                                                              ;       offset 8     1     Number of tracks        ($28)          40 or 80 --> ($28 or $50)
EF07  10AFC1                                  sty     ,u++                                                    ;       offset 9     1     Number of sides         ($01)          1 or 2
EF0A  CC0040                                  ldd     #$0040                                          ;       offset 10    1     Flags                   ($00)
EF0D  EDC1                                    std     ,u++                                                    ;       offset 11    1     Comp & Name length      ($40)          bits 0-2 = compression [0=off, >0=TBD]
                                                                                                                              ;                                                                           bits 3-7 = filename length [min 0, max 31]
EF0F  8E01D1                                  ldx     #nambuf                                         ;       offset 12    0-31  Disk name               (...)          (OPTIONAL) filename in ASCII. Doesn't need a zero terminator
EF12  E680                                    ldb     ,x+                                                     ; get filename length
EF14  C004                                    subb    #4                                                              ; subtract 4 to have the actual name without dot and extension
EF16  A680            cpy2buf lda     ,x+                                                     ; get a char from name
EF18  A7C0                                    sta     ,u+                                                     ; put into buffer
EF1A  5A                                      decb                                                                    ; decrement counter
EF1B  26F9                                    bne     cpy2buf                                         ; if not done, loopback, regB ends with value Zero
EF1D  CE0800                                  ldu     #$0800                                          ; point to buffers beginning
EF20  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EF22  9BF8                                    adda    <numUni                                         ; add drive number
EF24  8E0000                                  ldx     #$0000                                          ; to point to sector 0 (header)
EF27  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EF2A  1708BC                                  lbsr    vfyStatus                                       ; verify operation result
                                                                                                                      ; Once we have the header, we must add 20 tracks of $e5 bytes
EF2D  8E0800                                  ldx     #$0800                                          ; to use this buffer
EF30  9FEE                                    stx     <$ee                                                    ; update system pointer
EF32  BDE13A                                  jsr     filbuf                                          ; will fill it with bytes value $E5
EF35  CC1412                                  ldd     #$1412                                          ; get number of tracks to write (20) and numsectors x track
EF38  0DF4                                    tst     <$f4                                                    ; is single sided?
EF3A  2701                                    beq     noDouble                                                ; yes, do not double
EF3C  58                                      aslb                                                                    ; double number of sectors x track
EF3D  3D              noDouble        mul                                                                     ; calculate total number of sector to write
EF3E  1F02                                    tfr     d,y                                                     ; pass value to regY
EF40  8E0001                                  ldx     #1                                                              ; 1st sector (LSN) to be written for a VDK file
EF43  0D76                                    tst     <dsktyp                                         ; are we creating a VDK?
EF45  2602                                    bne     nSector                                         ; yes, skip next
EF47  301F                                    leax    -1,x                                                    ; 1st LSN for a DSK file (and so rewrite the header!)
EF49  3430            nSector pshs    x,y                                                     ; save variables
EF4B  CE0800                                  ldu     #$0800                                          ; point to buffers beginning
EF4E  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EF50  9BF8                                    adda    <numUni                                         ; add drive number
EF52  5F                                      clrb                                                                    ; higher byte to zero
EF53  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EF56  170890                                  lbsr    vfyStatus                                       ; verify operation result
EF59  3530                                    puls    x,y                                                     ; restore variables
EF5B  3001                                    leax    1,x                                                     ; point to next LSN
EF5D  313F                                    leay    -1,y                                                    ; decrement counter
EF5F  26E8                                    bne     nSector                                         ; if not done, loopback 
EF61  3410                                    pshs    x                                                               ; put next LSN on stack
                                                                                                                      ; now we must add 1st FAT sector
EF63  BDE17D                                  jsr     ffat1                                                   ; buffer: $800. fills 1st FAT sector upon geodata
EF66  CE0800                                  ldu     #$0800                                          ; point to buffers beginning
EF69  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EF6B  9BF8                                    adda    <numUni                                         ; add drive number
EF6D  5F                                      clrb                                                                    ; higher byte to zero. X is the good number
EF6E  AEE4                                    ldx     ,s                                                              ; get LSN from stack
EF70  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EF73  170873                                  lbsr    vfyStatus                                       ; verify operation result
                                                                                                                      ; let's try the same for track 16 (backup DIR)
                                                                                                                      ; we have to calculate the first sector for that track
EF76  8610                                    lda     #16                                                     ; number of track to write on
EF78  C612                                    ldb     #18                                                     ; number of sectors x track if single sided disk
EF7A  0DF4                                    tst     <$f4                                                    ; is double sided?
EF7C  2701                                    beq     1f                                                              ; no, skip next
EF7E  58                                      aslb                                                                    ; double sectors x track
EF7F  3D              1                       mul                                                                     ; to get sectors before sector 1 track 16
EF80  0D76                                    tst     <dsktyp                                         ; is it a VDK?
EF82  2703                                    beq     2f                                                              ; no, skip next
EF84  C30001                                  addd    #1                                                              ; add a sector (header)
EF87  DDD9            2                       std     <fST16                                          ; save that value for later use
                                                                                                                      ; now write 1st sector Track 16
EF89  CE0800                                  ldu     #$0800                                          ; point to buffers beginning
EF8C  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EF8E  9BF8                                    adda    <numUni                                         ; add drive number
EF90  5F                                      clrb                                                                    ; higher byte to zero. X is the good number
EF91  9ED9                                    ldx     <fST16                                          ; get LSN from variable
EF93  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EF96  170850                                  lbsr    vfyStatus                                       ; verify operation result
                                                                                                                      ; now we must add 2nd FAT sector
EF99  BDE1D2                                  jsr     ffat2                                                   ; buffer: $900. fills 2nd FAT sector upon geodata
EF9C  CE0900                                  ldu     #$0900                                          ; point to buffers beginning
EF9F  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EFA1  9BF8                                    adda    <numUni                                         ; add drive number
EFA3  5F                                      clrb                                                                    ; higher byte to zero. X is the good number
EFA4  AEE4                                    ldx     ,s                                                              ; get previous LSN from stack
EFA6  3001                                    leax    1,x                                                     ; increment number
EFA8  AFE4                                    stx     ,s                                                              ; update it
EFAA  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EFAD  170839                                  lbsr    vfyStatus                                       ; verify operation result
                                                                                                                      ; and do the same for Track 16
EFB0  CE0900                                  ldu     #$0900                                          ; point to buffers beginning
EFB3  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EFB5  9BF8                                    adda    <numUni                                         ; add drive number
EFB7  5F                                      clrb                                                                    ; higher byte to zero. X is the good number
EFB8  9ED9                                    ldx     <fST16                                          ; get previous LSN from variable
EFBA  3001                                    leax    1,x                                                     ; increment number
EFBC  9FD9                                    stx     <fST16                                          ; update it
EFBE  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EFC1  170825                                  lbsr    vfyStatus                                       ; verify operation result
                                                                                                                      ; now we go for the 16 directory sectors with entries ...
EFC4  BDE15E                                  jsr     fil16s                                          ; buffer: $a00. fills a sector to be used as diretory sector (3-18)
EFC7  C610                                    ldb     #16                                                     ; number of sectors to write
EFC9  3510                                    puls    x                                                               ; get previous LSN from stack
EFCB  3001                                    leax    1,x                                                     ; increment LSN number
EFCD  3414            nDSect  pshs    b,x                                                     ; save variables
EFCF  CE0A00                                  ldu     #$0a00                                          ; point to buffers beginning
EFD2  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EFD4  9BF8                                    adda    <numUni                                         ; add drive number
EFD6  5F                                      clrb                                                                    ; higher byte to zero. X is the good number
EFD7  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EFDA  17080C                                  lbsr    vfyStatus                                       ; verify operation result
EFDD  3514                                    puls    b,x                                                     ; restore variables
EFDF  3001                                    leax    1,x                                                     ; increment LSN number
EFE1  5A                                      decb                                                                    ; decrement sector counter
EFE2  26E9                                    bne     nDSect                                          ; not done, loop
EFE4  3410                                    pshs    x                                                               ; save next LSN (rest of the disk)
                                                                                                                      ; and do the same for Track16 too
EFE6  C610                                    ldb     #16                                                     ; number of sectors to write
EFE8  9ED9                                    ldx     <fST16                                          ; get previous LSN from variable
EFEA  3001                                    leax    1,x                                                     ; increment LSN number
EFEC  3414            n16Sect pshs    b,x                                                     ; save variables
EFEE  CE0A00                                  ldu     #$0a00                                          ; point to buffers beginning
EFF1  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
EFF3  9BF8                                    adda    <numUni                                         ; add drive number
EFF5  5F                                      clrb                                                                    ; higher byte to zero. X is the good number
EFF6  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
EFF9  1707ED                                  lbsr    vfyStatus                                       ; verify operation result
EFFC  3514                                    puls    b,x                                                     ; restore variables
EFFE  3001                                    leax    1,x                                                     ; increment LSN number
F000  5A                                      decb                                                                    ; decrement sector counter
F001  26E9                                    bne     n16Sect                                         ; not done, loop
                                                                                                                      ; finally we will write the rest of the sectors with $e5
                                                                                                                      ; X points to next LSN. end od disk might be: 720, 1440, 2880 (due to the header)
F003  8E0800                                  ldx     #$0800                                          ; to use this buffer again
F006  9FEE                                    stx     <$ee                                                    ; update system pointer
F008  BDE13A                                  jsr     filbuf                                          ; will fill it with bytes value $E5
F00B  3510                                    puls    x                                                               ; get LSN for rest of the disk from stack
F00D  108E02D1                                ldy     #721                                                    ; value to detect end of disk for 180k disk
F011  9610                                    lda     <noexec                                         ; get disc type
F013  8403                                    anda    #3                                                              ; use lowest 2 bits
F015  8101                                    cmpa    #1                                                              ; is it a 180k (720 sectors) disk? 
F017  270C                                    beq     1f                                                              ; yes, send write order
F019  108E05A1                                ldy     #1441                                                   ; to detect end of 360 disk
F01D  8102                                    cmpa    #2                                                              ; is it a 360k (1440 sectors) disk?
F01F  2704                                    beq     1f                                                              ; yes, send write order
F021  108E0B41                                ldy     #2881                                                   ; to detect end of 720k disk
F025  0D76            1                       tst     <dsktyp                                         ; are we creating a VDK file?
F027  2602                                    bne     nRDisk                                          ; yes, skip next
F029  313F                                    leay    -1,y                                                    ; subtract 1 (no header for DSK)
F02B  3430            nRDisk  pshs    x,y                                                     ; save registers (LSN, endOfFile)
F02D  CE0800                                  ldu     #$0800                                          ; point to buffers beginning
F030  86A0                                    lda     #$a0                                                    ; command to be sent: SDWRITE
F032  9BF8                                    adda    <numUni                                         ; add drive number
F034  5F                                      clrb                                                                    ; higher byte to zero. X is the good number
F035  BDF819                                  jsr     CommSDC                                         ; send command to SDC-card      (LSN = B:X)
F038  1707AE                                  lbsr    vfyStatus                                       ; verify operation result
F03B  3510                                    puls    x                                                               ; restore LSN number
F03D  3001                                    leax    1,x                                                     ; increment LSN number
F03F  ACE4                                    cmpx    ,s                                                              ; is X greater than Y (in the stack)
F041  3520                                    puls    y                                                               ; to balance stack
F043  26E6                                    bne     nRDisk                                          ; not done, loop
                                                                                                                      ; call SDRIVE n,UNLOAD
                                                                                                                      ; first we must save the filename from nambuf or it will be lost
                                                                                                                      ; we need from nambuf to nambuf + 12 (8 plus dot plus VDK)
F045  8E0A00                                  ldx     #$a00                                                   ; point to last DOS buffer
F048  CE01D1                                  ldu     #nambuf                                         ; point to filename buffer
F04B  C60D                                    ldb     #13                                                     ; number of bytes to copy
F04D  A6C0            1                       lda     ,u+                                                     ; get a byte from filename buffer
F04F  A780                                    sta     ,x+                                                     ; put into DOS buffer
F051  5A                                      decb                                                                    ; decrement counter
F052  26F9                                    bne     1b                                                              ; not done, loop
F054  0FFA                                    clr     <valPar                                         ; value param is 0
F056  C6E0                                    ldb     #$e0                                                    ; more used value
F058  D7FB                                    stb     <cmdNum                                         ; store it
F05A  BDED5D                                  jsr     reuseUL                                         ; call final part of SDRIVEx,UNLOAD
F05D  8E0A00                                  ldx     #$a00                                                   ; point to last DOS buffer
F060  CE01D1                                  ldu     #nambuf                                         ; point to filename buffer
F063  C60D                                    ldb     #13                                                     ; number of bytes to copy
F065  A680            1                       lda     ,x+                                                     ; get a byte from DOS buffer
F067  A7C0                                    sta     ,u+                                                     ; put into filename buffer
F069  5A                                      decb                                                                    ; decrement counter
F06A  26F9                                    bne     1b                                                              ; not done, loop
F06C  0FFA                                    clr     <valPar                                         ; value param is 0
F06E  C6E0                                    ldb     #$e0                                                    ; more used value
F070  D7FB                                    stb     <cmdNum                                         ; store it
F072  BDED63                                  jsr     dTok01                                  ; call SDRIVE n,"received FILENAME.VDK"
F075  9677                                    lda     hdwSdc                                          ; is here the old slot?
F077  2706                                    beq     1f                                                              ; no, exit
F079  B703FF                                  sta     cpyMPIcfg                                       ; update copy
F07C  B7FF7F                                  sta     MPIcfg                                          ; active it
F07F  39              1                       rts                                                                     ; return
                                                                              ; *** END_ADDED CODE TO SUPPORT VDK AND DSK FORMAT WHILE CREATING FILES 
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; restore old slot if needed
F080  9677            exsdrv  lda     hdwSdc                                          ; is here the old slot?
F082  2706                                    beq     exsdrv1                                         ; no, exit
F084  B703FF                                  sta     cpyMPIcfg                                       ; update copy
F087  B7FF7F                                  sta     MPIcfg                                          ; active it
F08A  39              exsdrv1 rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; special command floppy off
F08B  8D0F            flopOF  bsr     flopCom                                         ; see if it could be done
F08D  FA0114                                  orb     floSDC                                          ; add to hdw control byte (a bit to 1 disables floppy and enables sdc)
F090  F70114          eflop           stb     floSDC                                          ; update hdw control byte
F093  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; special command floppy on
F094  8D06            flopON  bsr     flopCom                                         ; see if it could be done
F096  53                                      comb                                                                    ; invert all bits: %00000001 - %00000010 convert to %11111110 - %11111101
F097  F40114                                  andb    floSDC                                          ; a bit 0 enables floppy and disables sdc
F09A  20F4                                    bra   eflop                                                     ; update and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; common part for floppy on-off
F09C  9D9F            flopCom jsr     <getNChr                                                ; skip token to avoid sn error when returning
F09E  F60114                                  ldb     floSDC                                          ; get hdw control byte
F0A1  C40C                                    andb    #%00001100                                      ; see if switching is possible
F0A3  C10C                                    cmpb    #%00001100                                      ; are both floppies and sdc present?
F0A5  2604                                    bne     notAllow                                                ; no, show error message
F0A7  D6F8                                    ldb     <numUni                                         ; get drive number (0-1)
F0A9  5C                                      incb                                                                    ; convert to 1-2
F0AA  39                                      rts                                                                     ; return
F0AB  3262            notAllow leas   2,s                                                     ; clean stack (is inside one subroutine)
F0AD  8EE493                                  ldx     #canNot-1                                       ; point to message
F0B0  7E90E5                                  jmp     outstr                                          ; show it and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; special command dim
F0B3  8651            sdcDIM  lda     #'Q'                                                    ; param to send into b
F0B5  8D14                                    bsr     sdcDAT                                          ; process it
F0B7  2527                                    bcs     noDiscEr                                                ; if got "not disc" error, explain it
F0B9  170134                                  lbsr    shwDIM                                          ; show received data
F0BC  8DC2            rstSlot bsr     exsdrv                                          ; restore slot if needed
F0BE  1CFE                                    andcc   #%11111110                                      ; clear carry flag
F0C0  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; special command get
F0C1  8649            sdcGET  lda     #'I'                                                    ; param to send into b
F0C3  8D06                                    bsr     sdcDAT                                          ; process it
F0C5  2519                                    bcs     noDiscEr                                                ; if got "not disc" error, explain it
F0C7  8D24                                    bsr     shwGET                                          ; show received data
F0C9  20F1                                    bra     rstSlot                                         ; restore slot if needed
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; common part for get-dim
F0CB  97FA            sdcDAT  sta     <valPar                                         ; save it
F0CD  9D9F                                    jsr     <getNChr                                                ; skip read token
F0CF  86C0                                    lda     #$c0                                                    ; command to send
F0D1  97FB                                    sta     <cmdNum                                         ; save it
F0D3  CC0100                                  ldd     #$0100                                          ; get 1 byte with value equal null
F0D6  FD01D1                                  std     nambuf                                          ; put in nambuf area (no name)
F0D9  0F10                                    clr     <noexec                                         ; no additional command needed
F0DB  DC8A                                    ldd     <$8a                                                    ; get 16 bits zero
F0DD  7EEDCA                                  jmp     putCmd2                                         ; send command
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; message no disc in drive
F0E0  96F8            noDiscEr        lda     <numUni                                         ; gets 0-1
F0E2  8B31                                    adda    #$31                                                    ; converts to '1'-'2'
F0E4  BDB54A                                  jsr     outchr                                          ; show drive number
F0E7  8EE4A1                                  ldx     #sdcNoDsc-1                                     ; point to message
F0EA  7E90E5                                  jmp     outstr                                          ; show it
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; show received 'get' info
F0ED  96F8            shwGET  lda     <numUni                                         ; get drive number
F0EF  8B31                                    adda    #$31                                                    ; converts to '1'-'2'
F0F1  BDB54A                                  jsr     outchr                                          ; show drive number
F0F4  863A                                    lda     #':'                                                    ; get char ':'
F0F6  BDB54A                                  jsr     outchr                                          ; show it
                                                                                                                      ; show drive status
F0F9  866F                                    lda     #'o'                                                    ; get fixed 1st letter
F0FB  BDB54A                                  jsr     outchr                                          ; show it
F0FE  866E                                    lda     #'n'                                                    ; get on as default
F100  D6F8                                    ldb     <numUni                                         ; get drive unit
F102  5C                                      incb                                                                    ; convert  0-1 into 1-2
F103  F40114                                  andb    floSDC                                          ; test corresponding bit of hdw control byte
F106  2607                                    bne     sdcS00                                          ; if not zero, skip next
F108  8666                                    lda     #'f'                                                    ; get value for off
F10A  BDB54A                                  jsr     outchr                                          ; put first 'f'
F10D  200D                                    bra     sdcS01                                          ; go put 2nd one instead of space
F10F  BDB54A          sdcS00  jsr     outchr                                          ; show result
F112  8620                                    lda     #32                                                     ; get space as inverted block to poke it
F114  9E88                                    ldx     <$88                                                    ; get cursor address
F116  A780                                    sta     ,x+                                                     ; put inverse space there
F118  9F88                                    stx     <$88                                                    ; update cusor address
F11A  2003                                    bra     sdcS02                                          ; skip next
F11C  BDB54A          sdcS01  jsr     outchr                                          ; show it
F11F  8D6D            sdcS02  bsr     shwFdat                                         ; show filename and type
F121  33C811                                  leau    17,u                                                    ; jump to length bytes
F124  8D06                                    bsr     shwFL                                                   ; show file length
F126  860D                                    lda     #$0d                                                    ; get enter value
F128  BDB54A                                  jsr     outchr                                          ; print it
F12B  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; show file length - structure of the fac
                                                                                                                      ;       $50 highest byte        --- always $00
                                                                                                                      ;       $51 high byte           --- high byte of u
                                                                                                                      ;       $52 mid  byte           --- low  byte of u
                                                                                                                      ;       $53 lower byte          --- reg. a ($00 = multiple of 256)
F12C  A642            shwFL           lda 2,u                                                         ; get highest byte in A
F12E  E641                                    ldb 1,u                                                         ; get high byte in B
F130  3406                                    pshs d                                                          ; hold on
F132  A6C4                                    lda ,u                                                          ; now get mid and low bytes
F134  E65F                                    ldb -1,u
F136  3540                                    puls u
                      ; Revised shwNum to take U and D
F138  11830010        shwNum  cmpu #$0010                                                     ; Check if high word shows >1M
F13C  251A                                    blo less1M                                                      ; if not display size in bytes
F13E  1F30                                    tfr u,d                         ; Otherwise take high word
F140  C4F0                                    andb #$F0                                                       ; Clear low 4 bits
F142  47                                      asra                                                                    ; Divide by 16 asr / ror
F143  56                                      rorb
F144  47                                      asra
F145  56                                      rorb
F146  47                                      asra
F147  56                                      rorb
F148  47                                      asra
F149  56                                      rorb
F14A  3406                                    pshs d                                                          ; Put B and then A onto stack
F14C  5F                                      clrb                                                                    ; zero B
F14D  3404                                    pshs b                                                          ; stack zero top byte as well
F14F  3540                                    puls u                                                          ; get to U
F151  3502                                    puls a                                                          ; and bottom byte in A
F153  5C                                      incb                                                                    ; flag for Megabytes
F154  3404                                    pshs b                                                          ; remember flag 
F156  200D                                    bra coreNum
F158  3402            less1M  pshs a                                                          ; stash mid byte
F15A  3440                                    pshs u                                                          ; and top two
F15C  3261                                    leas 1,s                                                                ; fudge frame drop high byte 
F15E  3540                                    puls u                                                          ; get u
F160  1F98                                    tfr b,a                                                         ; put low byte in a
F162  5F                                      clrb                                                                    ; flag for bytes
F163  3404                                    pshs b                                                          ; remember
F165  BDDD7D          coreNum         jsr     $dd7d                                                   ; integer to fac and normalize it
F168  BD9587                                  jsr     $9587                                                   ; fac to ascii (x points to string ended with $00
                                                                                                                              ; calculate length and add spaces to have total length = 7
F16B  3101                                    leay    1,x                                                     ; point to first digit
F16D  C608                                    ldb     #8                                                              ; desired length + final null
F16F  E0E4                                    subb ,s                                                         ; leave a space if we need M indicator
F171  5A              shwN01  decb                                                                    ; decrement counter
F172  A6A0                                    lda     ,y+                                                     ; get a digit
F174  26FB                                    bne     shwN01                                          ; if not 0, loopback
F176  5D                                      tstb                                                                    ; needs extra spaces?
F177  2708                                    beq     shwN02                                          ; no, skip section
F179  8620                                    lda     #32                                                     ; get space
F17B  BDB54A          shwN03  jsr     outchr                                          ; print it
F17E  5A                                      decb                                                                    ; decrement counter
F17F  26FA                                    bne     shwN03                                          ; if not zero, loopback
F181  BD90E5          shwN02  jsr     outstr                                          ; print lengths' string
F184  E6E0                                    ldb     ,s+                                                     ; Are we printing Megabytes?
F186  2705                                    beq     skipM                                                   ; No, bytes 
F188  864D                                    lda     #77                                                     ; M character
F18A  BDB54A                                  jsr     outchr                                          ; print it
F18D  39              skipM           rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; show filename and extension                   
F18E  CE0800          shwFdat ldu     #$800                                                   ; point to beginning of data
F191  C608            shwFd01 ldb     #8                                                              ; 8 chars for name
F193  A6C0            sdc01           lda     ,u+                                                     ; get a char
F195  BDB54A                                  jsr     outchr                                          ; show it
F198  5A                                      decb                                                                    ; decrement counter
F199  26F8                                    bne     sdc01                                                   ; not all, loopback
F19B  862E                                    lda     #'.'                                                    ; get a dot
F19D  BDB54A                                  jsr     outchr                                          ; show it
F1A0  C603                                    ldb     #3                                                              ; 3 chars for extension
F1A2  A6C0            sdc02           lda     ,u+                                                     ; get a char
F1A4  BDB54A                                  jsr     outchr                                          ; show it
F1A7  5A                                      decb                                                                    ; decrement counter
F1A8  26F8                                    bne     sdc02                                                   ; not all, loopback
                                                                                                                      ; show file type and extra info
F1AA  E6C0                                    ldb     ,u+                                                     ; get byte 11 (type)
F1AC  8620                                    lda     #32                                                     ; get space
F1AE  C102                                    cmpb    #$02                                                    ; is byte hidden?
F1B0  2602                                    bne     sdc05                                                   ; no, skip next 
F1B2  8668                                    lda     #'h'                                                    ; get h char
F1B4  BDB54A          sdc05           jsr     outchr                                          ; show it
F1B7  8620                                    lda     #32                                                     ; get space
F1B9  C101                                    cmpb    #$01                                                    ; is it locked?
F1BB  2602                                    bne     sdc06                                                   ; no, skip next 
F1BD  866C                                    lda     #'l'                                                    ; get l char
F1BF  BDB54A          sdc06           jsr     outchr                                          ; show it
F1C2  C110                                    cmpb    #$10                                                    ; is it a directory?
F1C4  270C                                    beq     sdc04                                                   ; yes, get that text
F1C6  C104                                    cmpb    #$04                                                    ; is it sdf type?
F1C8  2704                                    beq     sdc03                                                   ; yes, get that text
F1CA  8EE4CA                                  ldx     #datNUL-1                                       ; else get spaces
F1CD  10                                      fcb     #$10                                                    ; to skip next
F1CE  8EE4C0          sdc03           ldx     #datSDF-1                                       ; point to message
F1D1  10                                      fcb     #$10                                                    ; to skip next
F1D2  8EE4C5          sdc04           ldx     #datDIR-1                                       ; point to message
F1D5  3440                                    pshs    u                                                               ; save pointer
F1D7  BD90E5                                  jsr     outstr                                          ; put the three chars
F1DA  3540                                    puls    u                                                               ; restore pointer
F1DC  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; command sdrive alone
F1DD  0FF8            sdcLST  clr     <numUni                                         ; set drive 1
F1DF  17FEDF                                  lbsr    sdcGET                                          ; get and show info for that drive
                      ;                       bcs     esdcLST                                         ; if carry set, exit (no sdc present)
F1E2  210B                                    brn     esdcLST                                         ; if carry set, exit (no sdc present)
F1E4  9EA6                                    ldx     <$a6                                                    ; get input pointer
F1E6  301E                                    leax    -2,x                                                    ; move it back (will be moved forward later)
F1E8  9FA6                                    stx     <$a6                                                    ; update it
F1EA  0CF8                                    inc     <numUni                                         ; det drive 2                   
F1EC  17FED2                                  lbsr    sdcGET                                          ; get and show info for that drive
F1EF  39              esdcLST rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; show received 'dim' info
F1F0  96F8            shwDIM  lda     <numUni                                         ; get drive number
F1F2  8B31                                    adda    #$31                                                    ; converts to '1'-'2'
F1F4  BDB54A                                  jsr     outchr                                          ; show drive number
F1F7  8EE4AF                                  ldx     #totSect-1                                      ; point to message
F1FA  BD90E5                                  jsr     outstr                                          ; show it
F1FD  4F                                      clra                                    ; now get - Mike Miller 2021-05
F1FE  F6FF41                                  ldb $ff41                               ; four - Mike Miller 2021-05
F201  1F03                                    tfr d,u                                 ; bytes - Mike Miller 2021-05
F203  FCFF42                                  ldd $ff42                               ; in regs UD - Mike Miller 2021-05
F206  17FF2F                                  lbsr    shwNum                                          ; process and show it
F209  7E90A5                                  jmp     sendCR                                          ; send cr
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; command smkdir
F20C  8D09            xsmkdir bsr     others                                          ; read filename, get params
F20E  CC4B3A                                  ldd     #"K:"                                                   ; command to send
F211  BDEDCA          xsmk01  jsr     putCmd2                                         ; send command
F214  16FEA5                                  lbra    rstSlot                                         ; restore slot if needed
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; read filename and set parameters                      
F217  BDB7AA          others  jsr   getfnam                                           ; read string and length into $1d1 = nambuf
F21A  F601D1                  ldb     nambuf                                          ; is string length 0?
F21D  1027F554                lbeq    snError                                         ; yes, show error
F221  C1FD                    cmpb    #253                                                    ; is string longer than maximum accepted? (253 + m: + final null)
F223  2505                                    bcs     other01                                         ; no, skip section
F225  3262                                    leas    2,s                                                     ; clean stack (we are inside a subroutine)
F227  16F8F5                                  lbra    TooLong                                         ; show error message
F22A  5F              other01 clrb                                                                    ; set to zero
F22B  D7FA                                    stb     <valPar                                         ; parameter for b
F22D  D710                                    stb     <noexec                                         ; no more parmas
F22F  D7F8                                    stb     <numUni                                         ; drive 0
F231  86E0                                    lda     #$e0                                                    ; command to send
F233  97FB                                    sta     <cmdNum                                         ; save it
F235  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; command skill
F236  8DDF            xskill  bsr     others                                          ; read filename, get params
F238  CC583A                                  ldd     #"X:"                                                   ; command to send
F23B  20D4                                    bra     xsmk01                                          ; go send command
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; command sren
F23D  BDB7AA          xsren           jsr     getfnam                                         ; get old name
F240  F601D1                  ldb     nambuf                                          ; is string length 0?
F243  1027F52E                lbeq    snError                                         ; yes, show error
F247  C17D                    cmpb    #125                                                    ; is string longer than maximum accepted? (125 for each one)
F249  1024F8D2                lbcc    TooLong                                         ; show error message
                                                                                                                      ; send it to $800 ended with $00
F24D  8E01D2                                  ldx   #nambuf+1                                 ; point to 1st char of the input string
F250  CE0800                                  ldu     #$0800                                          ; point to 1st dos buffer
F253  F601D1                                  ldb     nambuf                                          ; get string length
F256  A680            xsren01 lda     ,x+                                                     ; get a char from input string
F258  A7C0                                    sta     ,u+                                                     ; copy in the buffer
F25A  5A                                      decb                                                                    ; decrement counter
F25B  26F9                                    bne     xsren01                                         ; not yet done, loopback
F25D  6FC0                                    clr     ,u+                                                     ; put a final zero
F25F  DF76                                    stu     <bufPtr                                         ;save pointer
F261  BDB7AA                                  jsr     getfnam                                         ; get new name
F264  F601D1                  ldb     nambuf                                          ; is string length 0?
F267  1027F50A                lbeq    snError                                         ; yes, show error
F26B  C17D                    cmpb    #125                                                    ; is string longer than maximum accepted? (125 for each one)
F26D  1024F8AE                lbcc    TooLong                                         ; show error message
                                                                                                                      ; add after old name
F271  8E01D2                                  ldx   #nambuf+1                                 ; point to 1st char of the input string
F274  DE76                                    ldu     <bufPtr                                         ; get old pointer
F276  F601D1                                  ldb     nambuf                                          ; get newname string length
F279  A680            xsren02 lda     ,x+                                                     ; get a char from input string
F27B  A7C0                                    sta     ,u+                                                     ; copy in the buffer
F27D  5A                                      decb                                                                    ; decrement counter
F27E  26F9                                    bne     xsren02                                         ; not yet done, loopback
F280  DF76                                    stu     <bufPtr                                         ; save last byte received
                                                                                                                      ; send it back to nambuf+1 - put length in nambuf 
F282  8E01D2                                  ldx     #nambuf+1                                       ; point to 1st char in input buffer                                                                             
F285  CE0800                                  ldu     #$800                                                   ; point to 1st byte of dos buffer
F288  5F                                      clrb                                                                    ; counter to zero
F289  A6C0            xsren03 lda     ,u+                                                     ; get a byte
F28B  A780                                    sta     ,x+                                                     ; put on the input buffer
F28D  5C                                      incb                                                                    ; increment counter
F28E  119376                                  cmpu    <bufPtr                                         ; got to the end?
F291  25F6                                    blo     xsren03                                         ; no, loopback
F293  F701D1                                  stb     nambuf                                          ; save total string length 
F296  8D92                                    bsr     other01                                         ; set parameters
F298  CC523A                                  ldd     #"R:"                                                   ; command to send
F29B  16FF73                                  lbra    xsmk01                                          ; go send command
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; routine to add extension to the file name if not present
                      ; param: noexec=$00 to add .DSK  --- noexec=$80 to add .SDF
                      ; -------------------------------------------------------------------------------------------------------------------------------
F29E  3416            addExt  pshs    d,x                                                     ; save pointers
F2A0  8E01D1                                  ldx     #nambuf                                         ; poit to name buffer
F2A3  E680                                    ldb   ,x+                                                       ; length of name
                                                                                                                      ; Check for existing extension
F2A5  A680            isadot          lda   ,x+                                                       ; get a char from name
F2A7  812E                    cmpa  #'.'                                                      ; is it a dot?
F2A9  2727                    beq   eAddExt                                           ; yes, exit
F2AB  5A                      decb                                                                    ; decrement counter
F2AC  26F7                    bne   isadot                                            ; not end of string, get next char name
                                                                                                                      ; so, we must add an extension
F2AE  0D10                    tst     <noexec                                         ; is for a SDF file?
F2B0  2B0C                    bmi     addSDF                                          ; yes, jump section
F2B2  CC2E44          addDSK          ldd   #$2e44                                            ; get chars ".D"
F2B5  ED81                    std   ,x++                                                      ; add to end of name
F2B7  CC534B                  ldd   #$534b                                            ; get chars "SK"
F2BA  ED81                    std   ,x++                                                      ; add to end of name
F2BC  200A                                    bra     add01                                                   ; skip next section
F2BE  CC2E53          addSDF          ldd   #$2e53                                            ; get chars ".S"
F2C1  ED81                    std   ,x++                                                      ; add to end of name
F2C3  CC4446                  ldd   #$4446                                            ; get chars "DF"
F2C6  ED81                    std   ,x++                                                      ; add to end of name
F2C8  6F84            add01           clr     ,x                                                              ; put a null at next byte
F2CA  F601D1                                  ldb   nambuf                                            ; get old name length
F2CD  CB04                                    addb  #4                                                                ; add 4 (because of dot and extension added)
F2CF  F701D1                  stb   nambuf                                            ; save new length in byte before string
F2D2  3596            eAddExt puls  d,x,pc                                            ; restore pointers and return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Section to deal with SDC-card SCHD commands
                      ; -------------------------------------------------------------------------------------------------------------------------------
F2D4  9DA5            xschd           jsr     <redLChr                                                ; peek at next char after SCHD
F2D6  1027F49B                                lbeq    snError                                         ; if null, syntax error
F2DA  8122                                    cmpa    #$22                                                    ; is this the double quote?  = new directory
F2DC  10270123                                lbeq    setCD01                                         ; do the same as SDIR = (by now!)
F2E0  81C6                                    cmpa    #$c6                                                    ; is it the token for /?
F2E2  271C                                    beq     chgRoot                                         ; process it
F2E4  812E                                    cmpa    #'.'                                                    ; is it a dot '.'?
F2E6  1026F48B                                lbne    snError                                         ; no, show syntax error
F2EA  9D9F                                    jsr     <getNChr                                                ; get next char
F2EC  812E                                    cmpa    #'.'                                                    ; is it another dot '.'?
F2EE  1026F483                                lbne    snError                                         ; no, show syntax error
                                                                                                                      ; this is to set parent dir as actual
F2F2  9D9F            chgParD jsr     <getNChr                                                ; skip token
F2F4  8602            chwPD01 lda     #$02                                                    ; only one char in nambuf
F2F6  C62E                                    ldb     #'.'                                                    ; char to design root directory (twice)
F2F8  FD01D1                                  std     nambuf                                          ; put signs
F2FB  F701D3                                  stb     nambuf+2                                                ; into input buffer
F2FE  2009                                    bra     chgR01                                          ; process it
                                                                                                                      ; this is to set root as actual dir
F300  9D9F            chgRoot jsr     <getNChr                                                ; skip token
F302  8601                                    lda     #$01                                                    ; only one char in nambuf
F304  C62F                                    ldb     #'/'                                                    ; char to design root directory
F306  FD01D1                                  std     nambuf                                          ; put data in input buffer area
F309  17FF1E          chgR01  lbsr    other01                                         ; set parameters
F30C  1600F7                                  lbra    setCD02                                         ; go to process the requested directory
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Section to deal with SDC-card SDIR commands
                      ; -------------------------------------------------------------------------------------------------------------------------------
F30F  9DA5            xsdir           jsr     <redLChr                                                ; peek at next char after sdir
F311  102700E6                                lbeq    shwCAct                                         ; is the sdir alone = show current dir contents
                                                                                                                      ; decode token - sign
F315  81B3                                    cmpa    #$b3                                                    ; is the token for get?
F317  102700AF                                lbeq    shwCDir                                         ; yes, show current dir name
F31B  8187                                    cmpa    #$87                                                    ; is it the token for print (question mark)?
F31D  102700EB                                lbeq    shwFull                                         ; yes, show the full path name of actual dir (maybe substitute sdir get!)
F321  81C4                                    cmpa    #$c4                                                    ; is this the token for ('-')? = show contents of root dir
F323  102700C5                                lbeq    shwRoot                                         ; yes, go process it
F327  81CB                                    cmpa    #$cb                                                    ; is this the token for ('=')? = set current dir
F329  102700D4                                lbeq    setCDir                                         ; yes, go process it
F32D  8122                                    cmpa    #$22                                                    ; is this the sign ('"')?  = show contents of the requested dir
F32F  1026F442                                lbne    snError                                         ; no, show syntax error
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; show contents of requested dir
                                                                                                                      ; send a initiate directory listing command
F333  17FEE1          shwRDir lbsr    others                                          ; read filename, get params
F336  CC4C3A          shwEDir ldd     #"L:"                                                   ; command to send
F339  BDEDCA                                  jsr     putCmd2                                         ; send command
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; ask for directory pages to the sdc
F33C  17FEEB          shwRD00 lbsr    other01                                         ; set some parameters
F33F  C63E                                    ldb     #'>'                                                    ; special parameter
F341  D7FA                                    stb     <valPar                                         ; save it
F343  86C0                                    lda     #$c0                                                    ; command to send
F345  97FB                                    sta     <cmdNum                                         ; save it
F347  CC0100                                  ldd     #$0100                                          ; mark 1 byte with value equal null
F34A  FD01D1                                  std     nambuf                                          ; put in nambuf area (no name)
F34D  BDEDCA                                  jsr     putCmd2                                         ; send command
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
F350  8E0800                                  ldx     #$0800                                          ; point to buffer beginning
F353  5F                                      clrb                                                                    ; counter to zero
F354  A684            shwRD01 lda     ,x                                                              ; get first char (1st name character)
F356  2709                                    beq     shwRD02                                         ; if zero, no more items
F358  5C                                      incb                                                                    ; increment received items counter
F359  308810                                  leax    16,x                                                    ; point to next item
F35C  8C0900                                  cmpx    #$0900                                          ; got to end of buffer?
F35F  25F3                                    blo     shwRD01                                         ; no, loopback
F361  5D              shwRD02 tstb                                                                    ; have we read any item?
F362  2762                                    beq     shwRD09                                         ; no, print ok and exit
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
F364  D7FA                                    stb     <itsRead                                                ; save number of items we received
F366  CE0800                                  ldu     #$0800                                          ; point to beginning of received data
F369  DFF8                                    stu     <cpyPtr                                         ; save it
F36B  0FFB                                    clr     <itsDone                                                ; for each item (clear counter)
F36D  0CFB            shwRD03 inc     <itsDone                                                ; add 1 to counter
F36F  17FE1F                                  lbsr    shwFd01                                         ; show item name and type
F372  30C4                                    leax    ,u                                                              ; point to msb highest byte of item length (not used)
F374  EE84                                    ldu     ,x                                                              ; get 2 next bytes (Mike Miller)
F376  EC02                                    ldd     2,x                                                     ; get lowest byte (Mike Miller)
F378  17FDBD                                  lbsr    shwNum                                          ; show item Length (U:A)
F37B  9EF8                                    ldx     <cpyPtr                                         ; get beginning of this record
F37D  308810                                  leax    16,x                                                    ; point to next one
F380  9FF8                                    stx     <cpyPtr                                         ; update pointer
F382  D6FA                                    ldb     <itsRead                                                ; get number of items read
F384  C110                                    cmpb    #16                                                     ; was 16?                                                               
F386  262C                                    bne     shwRD07                                         ; NO, so partial page. Send CR and go for next one
F388  96FB                                    lda     <itsDone                                                ; YES, it was a full page. Get item number just printed
F38A  810F                                    cmpa    #15                                                     ; was penultimate row?
F38C  2608                                    bne     shwRD04                                         ; no, see if it was the last one
F38E  8EE4CF                                  ldx     #msgCont-1                                      ; yes point to message CONT
F391  BD90E5                                  jsr     outstr                                          ; show message                  
F394  201E                                    bra     shwRD07                                         ; and send CR and go for next item      
F396  8110            shwRD04 cmpa    #16                                                     ; is the last one?
F398  261A                                    bne     shwRD07                                         ; no, send CR and go for next item
F39A  8EE4D6                                  ldx     #msgExit-1                                      ; yes, point to message EXIT
F39D  BD90E5                                  jsr     outstr                                          ; show message                  
F3A0  BD8006          shwRD05 jsr     $8006                                                   ; wait for user keypress
F3A3  27FB                                    beq     shwRD05                                         ; none, loopback
F3A5  8143                                    cmpa    #$43                                                    ; is 'C' for Continue?
F3A7  2605                                    bne     shwRD06                                         ; no, verify if is 'E'
F3A9  BD90A5                                  jsr     sendCR                                          ; send a CR (will scroll 1 line up)
F3AC  208E                                    bra   shwRD00                                           ; BREAK, ask for a new page to the SDC
F3AE  8145            shwRD06 cmpa    #$45                                                    ; is 'E' for eXIT?
F3B0  26EE                                    bne     shwRD05                                         ; no, loopback and wait for a correct key
F3B2  200D                                    bra     shwRD08                                         ; yes, send CR, show OK and exit
F3B4  BD90A5          shwRD07 jsr     sendCR                                          ; send CR
F3B7  96FB                                    lda     <itsDone                                                ; get number of items shown
F3B9  91FA                                    cmpa    <itsRead                                                ; work done?
F3BB  2709                                    beq     shwRD09                                         ; yes, send OK and exit
F3BD  DEF8                                    ldu     <cpyPtr                                         ; no, get saved buffer pointer
F3BF  20AC                                    bra     shwRD03                                         ; loopback for next item
F3C1  860D            shwRD08 lda     #$0d                                                    ; get enter value
F3C3  BDB54A                                  jsr     outchr                                          ; print it
F3C6  17FCB7          shwRD09 lbsr    exsdrv                                          ; restore slot if needed
F3C9  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; show name of current dir
F3CA  9D9F            shwCDir jsr     <getNChr                                                ; skip token
F3CC  8601                                    lda     #1                                                              ; put value 1
F3CE  97F9                                    sta     <numDir                                         ; as first level requested
F3D0  17FE57                                  lbsr    other01                                         ; put some parameters
F3D3  86C0                                    lda     #$c0                                                    ; command to be sent
F3D5  97FB                                    sta     <cmdNum                                         ; save it
F3D7  C643                                    ldb     #'C'                                                    ; param for b
F3D9  D7FA                                    stb     <valPar                                         ; save it
F3DB  CC0100                                  ldd     #$0100                                          ; get 1 byte with value equal null
F3DE  FD01D1                                  std     nambuf                                          ; put in nambuf area (no name)
F3E1  BDEDCA                                  jsr     putCmd2                                         ; send command
F3E4  2503                                    bcs     eshwCDir                                                ; if error, don't show garbage
F3E6  17FDA5                                  lbsr    shwFdat                                         ; show dirname and type
F3E9  16FC94          eshwCDir        lbra    exsdrv                                          ; restore slot if needed
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; show contents of root dir
F3EC  9D9F            shwRoot jsr     <getNChr                                                ; skip token
F3EE  8601                                    lda     #$01                                                    ; only one char in nambuf
F3F0  C62F                                    ldb     #'/'                                                    ; char to design root directory
F3F2  FD01D1          shwRt01 std     nambuf                                          ; put data in input buffer area
F3F5  17FE32                                  lbsr    other01                                         ; set parameters
F3F8  16FF3B                                  lbra    shwEDir                                         ; go to process the requested directory
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; show contents of current dir
F3FB  8601            shwCAct lda     #$01                                                    ; only one char in nambuf
F3FD  C62A                                    ldb     #'*'                                                    ; char to design current directory
F3FF  20F1                                    bra     shwRt01                                         ; complete data and process command
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; set current dir to requested name
F401  9D9F            setCDir jsr     <getNChr                                                ; get next entered char
F403  17FE11          setCD01 lbsr    others                                          ; read filename, get params
F406  CC443A          setCD02 ldd     #"D:"                                                   ; command to send
F409  16FE05                                  lbra    xsmk01                                          ; go send command
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; show actual dir full path
F40C  9D9F            shwFull jsr     <getNChr                                                ; skip token
F40E  327E                                    leas    -2,s                                                    ; create a 3 bytes hole for variables
F410  0FF9                                    clr     numDir                                          ; set items received to 0
F412  8E0900                                  ldx     #$0900                                          ; get 2nd dos buffer address
F415  AFE4                                    stx     destPtr,s                                       ; to receive results
F417  17FE10          shwF01  lbsr    other01                                         ; put some parameters
F41A  86C0                                    lda     #$c0                                                    ; command to be sent
F41C  97FB                                    sta     <cmdNum                                         ; save it
F41E  C643                                    ldb     #'C'                                                    ; param for b
F420  D7FA                                    stb     <valPar                                         ; save it
F422  CC0100                                  ldd     #$0100                                          ; get 1 byte with value equal null
F425  FD01D1                                  std     nambuf                                          ; put in nambuf area (no name)
F428  0CF9                                    inc     numDir                                          ; increment received items counter
F42A  BDEDCA                                  jsr     putCmd2                                         ; send command to get actual directory
                                                                                                                      ; processing single answer
F42D  253D                                    bcs     shwF80                                          ; if error (got to root) skip section  
                                                                                                                      ; save received data to $900 buffer
F42F  8E0800                                  ldx     #$800                                                   ; point to beginning of received data
F432  EEE4                                    ldu     destPtr,s                                       ; point ot destination record
F434  C608                                    ldb     #8                                                              ; max name length 
F436  A680            shwF02  lda     ,x+                                                     ; get a received char
F438  8120                                    cmpa    #$20                                                    ; is it space
F43A  2705                                    beq     shwF03                                          ; yes, stop copying name
F43C  A7C0                                    sta     ,u+                                                     ; pass char to record
F43E  5A                                      decb                                                                    ; decrement counter
F43F  26F5                                    bne     shwF02                                          ; if not zero, loopback
F441  E680            shwF03  ldb     ,x+                                                     ; get 1st termination char
F443  C120                                    cmpb    #$20                                                    ; is it a space?
F445  2714                                    beq     shwF04                                          ; yes, goto next dir element
F447  862E                                    lda     #'.'                                                    ; get dot char
F449  EDC1                                    std     ,u++                                                    ; add dot and 1st ext char
F44B  A680                                    lda     ,x+                                                     ; get 2nd extension char
F44D  8120                                    cmpa    #$20                                                    ; is it a space?
F44F  270A                                    beq     shwF04                                          ; yes, goto next dir element
F451  A7C0                                    sta     ,u+                                                     ; add 2nd extension char
F453  A680                                    lda     ,x+                                                     ; get 3rd extension char
F455  8120                                    cmpa    #$20                                                    ; is it a space?
F457  2702                                    beq     shwF04                                          ; yes, skip next
F459  A7C0                                    sta     ,u+                                                     ; add 3rd extension char
F45B  CC2F00          shwF04  ldd     #$2f00"                                         ; get a slash and zero terminator
F45E  EDC4                                    std     ,u                                                              ; add to destination buffer
F460  EEE4                                    ldu     destPtr,s                                       ; get beginning just used
F462  33C810                                  leau    16,u                                                    ; add 16 to buffer pointer
F465  EFE4                                    stu     destPtr,s                                       ; update it for next path directory
F467  17FE8A                                  lbsr    chwPD01                                         ; ask SDC to go to parent dir (SCHD"..")
F46A  20AB                                    bra     shwF01                                          ; go back to ask for new actual directory
                                                                                                                      ; got to the root, NO MORE data to request. 
F46C  96F9            shwF80  lda     <numDir                                         ; get number of received items
F46E  8101                                    cmpa    #1                                                              ; only one item received?
F470  273E                                    beq     eshwFull                                                ; yes, it was ROOT, so exit
                                                                                                                      ; process received data to show full path
F472  CE01D2                                  ldu     #nambuf+1                                       ; destination for full path name (input buffer)
F475  862F                                    lda     #$2f                                                    ; get a slash to denote root
F477  A7C0                                    sta     ,u+                                                     ; put into buffer as 1st char
                                                                                                                      ; add all the received dirs in reverse order
F479  AEE4                                    ldx     destPtr,s                                       ; get last record address (not used ... it was root)
F47B  3010            shwF81  leax    -16,x                                                   ; point to previous used entry
F47D  8C0900                                  cmpx    #$0900                                          ; gone back past the beginning?
F480  250E                                    bcs     shwF90                                          ; yes, end of putting items together
F482  3410                                    pshs    x                                                               ; save register
F484  A680            shwF82  lda     ,x+                                                     ; get a char from origin
F486  2704                                    beq     shwF83                                          ; if null, end of string
F488  A7C0                                    sta     ,u+                                                     ; else put at destination
F48A  20F8                                    bra     shwF82                                          ; go get next char
F48C  3510            shwF83  puls    x                                                               ; restore pointer
F48E  20EB                                    bra     shwF81                                          ; back to get next directory
F490  6FC4            shwF90  clr     ,u                                                              ; mark end of string
F492  1F30                                    tfr     u,d                                                     ; send poiter to D
F494  8301D2                                  subd    #nambuf+1                                       ; to calculate length of string
F497  F701D1                                  stb     nambuf                                          ; save it
F49A  8E01D1                                  ldx     #nambuf                                         ; point to whole path name
F49D  BD90E5                                  jsr     outstr                                          ; show the full mounted directory path
F4A0  860D                                    lda     #$0d                                                    ; code for Enter
F4A2  BDB54A                                  jsr     outchr                                          ; print it
                                                                                                                      ; issue a SCHD"" with the whole path to restore situation!!
F4A5  3262                                    leas    2,s                                                     ; clean stack
F4A7  17FD80                                  lbsr    other01                                         ; set some parameters
F4AA  CC443A                                  ldd     #"D:"                                                   ; command to send       
F4AD  16FD61                                  lbra    xsmk01                                          ; go back to the actual directory we had before
F4B0  3262            eshwFull        leas    2,s                                                     ; clean stack
F4B2  16FBCB                                  lbra    exsdrv                                          ; exit
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; process for command WRITE MEM @bank, start, destination, length
                      ; this code is made based on the FLASHER.BIN program by Darren Atkinson
                      ; -------------------------------------------------------------------------------------------------------------------------------
F4B5  9D9F            xwrite  jsr     <getNChr                                                ; get next char
F4B7  819B                                    cmpa    #$9b                                                    ; is the token for MEM?
F4B9  1026F2B8                                lbne    snError                                         ; no, show error message
F4BD  9D9F                                    jsr     <getNChr                                                ; get next byte
F4BF  8140                                    cmpa    #64                                                     ; is it sign '@'?
F4C1  1026F2B0                                lbne    snError                                         ; no, show error message
F4C5  9D9F                                    jsr     <getNChr                                                ; get number into A
F4C7  8130                                    cmpa    #$30                                                    ; is greater or equal 0?
F4C9  2504                                    bcs     xwrt01                                          ; no, show error
F4CB  8137                                    cmpa    #$37                                                    ; is less or equal 7?
F4CD  2305                                    bls     xwrt02                                          ; yes, goto next parameters
F4CF  C611            xwrt01  ldb     #$11                                                    ; SD error
F4D1  7E8344                                  jmp     syserr                                          ; bas_system_error
F4D4  8407            xwrt02  anda    #$07                                                    ; use only valid bits
F4D6  97EB                                    sta     <DCDRV                                          ; save requested bank number
F4D8  9D9F                                    jsr     <getNChr                                                ; advance one char
F4DA  260F                                    bne     morParms                                                ; if more chars, read them
                                                                                                                      ; default parameters for writting a ROMPACK
F4DC  8E3000                                  ldx     #$3000                                          ; get origin of data
F4DF  9F76                                    stx     <dSource                                                ; save start into variable
F4E1  8EC000                                  ldx     #$c000                                          ; get destination in bank
F4E4  9FFA                                    stx     <dDest                                          ; save destination into variable
F4E6  8E3F00                                  ldx     #$3f00                                          ; get max. length (16k-256bytes)
F4E9  203F                                    bra     savLen                                          ; skip section
F4EB  BD89AA          morParms        jsr     ckComma                                         ; CkComma
F4EE  BD8E83                                  jsr     get16bN                                         ; get a 16 bit number into X
F4F1  8C7FFF                                  cmpx    #basini-1                                       ; is it on the upper 32k?
F4F4  226C                                    bhi     badParam                                                ; yes, bad format number, show error
F4F6  9F76                                    stx     <dSource                                                ; save start into variable
F4F8  9DA5                                    jsr     <redLChr                                                ; advance one char
F4FA  BD89AA                                  jsr     ckComma                                         ; CkComma
F4FD  BD8E83                                  jsr     get16bN                                         ; get a 16 bit number into X
F500  8CC000                                  cmpx    #romini                                         ; is it out of SDC-ROM area?
F503  255D                                    blo     badParam                                                ; yes, bad format number, show error
F505  8CF000                                  cmpx    #$f000                                          ; is it higher than the beginning of 4th sector?
F508  2258                                    bhi     badParam                                                ; yes, bad format number, show error
F50A  9FFA                                    stx     <dDest                                          ; save destination into variable
F50C  9DA5                                    jsr     <redLChr                                                ; advance one char
F50E  BD89AA                                  jsr     ckComma                                         ; CkComma
F511  BD8E83                                  jsr     get16bN                                         ; get a 16 bit number into X
F514  1F10                                    tfr     x,d                                                     ; pass value to D
F516  D3FA                                    addd    <dDest                                          ; add destination value
F518  2548                                    bcs     badParam                                                ; yes, sum exceeds flash capacity
F51A  1083FF00                                cmpd    #$ff00                                          ; uses result the reserved 256 bytes?
F51E  2242                                    bhi     badParam                                                ; yes, bad format number, show error
F520  1F10                                    tfr     x,d                                                     ; get length again
F522  D376                                    addd    <dSource                                                ; add source beginning
F524  10837FFF                                cmpd    #basini-1                                       ; exceeds RAM area?
F528  2238                                    bhi     badParam                                                ; yes, bad format number, show error
F52A  9F50            savLen  stx     <FPA0                                                   ; save byte count
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; search slot where SDC is
F52C  17F6C8                                  lbsr    tstSDC                                          ; is SDC already active?
F52F  271A                                    beq     xwrt03                                          ; yes, save slot number (if no MPI this will be $00)
F531  B60114                                  lda     floSDC                                          ; no, get hdw control byte
F534  8508                                    bita    #%00001000                                      ; is a SDC in the system?
F536  1027F5ED                                lbeq    NotSDCPresent                           ; no, show message
                                                                                                                      ; yes, calculate occupied slot
F53A  B60114                                  lda     floSDC                                          ; get hdw control byte
F53D  84C0                                    anda    #%11000000                                      ; use bits 7-6 (slot where SDC is)
F53F  44                                      lsra                                                                    ; move them to
F540  44                                      lsra                                                                    ; bits 5-4 (lower bits of high nibble)
F541  3402                                    pshs    a                                                               ; save high nibble
F543  44                                      lsra                                                                    ; low nibble   
F544  44                                      lsra                                                                    ; gets high nibble
F545  44                                      lsra                                                                    ; that will end  
F546  44                                      lsra                                                                    ; as zero
F547  AAE0                                    ora     ,s+                                                     ; add high nibble (now they are the same value)
F549  2003                                    bra     xwrt04                                          ; save slot number
F54B  B603FF          xwrt03  lda     cpyMPIcfg                                       ; get active slot number
F54E  97F9            xwrt04  sta     <sdcSlot                                                ; save slot where SDC is
                                                                                                                      ; copy the flasher routine to $1da. Should not exceed $2d2
F550  8EF571          cpyFlsh ldx     #xflash                                         ; point to beginning of program to be copied
F553  CE01DA                                  ldu     #$1da                                                   ; point to destination
F556  EC81            cpyFl01 ldd     ,x++                                                    ; get a word
F558  EDC1                                    std     ,u++                                                    ; put at destination
F55A  8CF67F                                  cmpx    #erasEnd+2                                      ; got to end of program?
F55D  25F7                                    blo     cpyFl01                                         ; no, loopback
F55F  7E01DA                                  jmp     $1da                                                    ; execute copied program in low RAM
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
F562  8EE4DD          badParam        ldx     #wrongPar-1                                     ; point to message
F565  BD90E5                                  jsr     outstr                                          ; show it
F568  9DA5                                    jsr     <redLChr                                                ; read last char
F56A  2704                                    beq     ebadPrm                                         ; if null, exit
F56C  9D9F            bP001           jsr     <getNChr                                                ; skip an unused byte
F56E  26FC                                    bne     bP001                                                   ; if not null, loopback
F570  39              ebadPrm rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; This code manages the process of flashing a bank
                      ; -------------------------------------------------------------------------------------------------------------------------------
F571  1A50            xflash  orcc  #$50                    ; mask interrupts
F573  96F9                     lda    <sdcSlot                                                ; get the slot where SDC is
                                                                                                                      ; this is to avoid starting the just flashed bank!
F575  F603FF                   ldb    cpyMPIcfg                                       ; get current active slot
F578  D7F9                                    stb     <sdcSlot                                                ; save it to go back to it at the end
F57A  B703FF                                  sta     cpyMPIcfg                                       ; update copy
F57D  B7FF7F                                  sta     MPIcfg                                          ; switch to the SDC slot
F580  B7FFD8                             sta   R1CLR                   ; normal speed for CoCo 3 (0,89MHz) - $ffd8 (resets $ffd7)
F583  B7FFDE                   sta   RAMROM                  ; activate RAM/ROM map mode = MAP0 - resets $ffdf
F586  D6EB                     ldb   <DCDRV                  ; get target bank number (parameter at $eb)
F588  8D1B                     bsr   FlashKill               ; erase target bank
F58A  308C60                   leax  burner,pcr                   ; point X at burner subroutine
F58D  9FEE                     stx   <DCBPT                  ; store subroutine address: $ee
F58F  9E76                                    ldx     <dSource                                                ; point X at source data
F591  DEFA                                    ldu     <dDest                  ; point U at flash destination
F593  8D45                     bsr   callX                   ; write data to the flash
F595  D6F9                     ldb    <sdcSlot                                                ; get original active slot
F597  F703FF                   stb    cpyMPIcfg                                       ; update copy
F59A  F7FF7F                   stb    MPIcfg                                          ; set it again
F59D  0F71                     clr   <$71                    ; force a cold boot
F59F  1A50                     orcc  #$50                    ; mask interrupts
F5A1  6E9FFFFE                 jmp   [$fffe]                 ; execute the system reset routine
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ;  Erase flash bank specified in ACCB
                      ; -------------------------------------------------------------------------------------------------------------------------------
F5A5                  FlashKill 
F5A5  D786                                    stb   <TargBank               ; store bank number: $86
F5A7  9650                                    lda     <FPA0                                                   ; get byte count                                                                                                                                                        ; 2B - 20
F5A9  84F0                                    anda    #$f0                                                    ; use only high nibble                                                                                                                                  ; 20 - 20
F5AB  3402                                    pshs    a                                                               ; push value (number of sectors of 4K)                                                                                          ; 20 - 20
F5AD  DC50                                    ldd     <FPA0                                                   ; get full byte count                                                                                                                                   ; 2B00 - 2000
F5AF  840F                                    anda    #$0f                                                    ; mask high nibble of high byte                                                                                                         ; 0B00 - 0000
F5B1  10830000                                cmpd    #$0000                                          ; is masked D equal 0?                                                                                                                                  ; NO   - YES
F5B5  2706                                    beq     flK01                                                   ; yes, don't correct values                                                                                                                     ; corr - jump
F5B7  8610                                    lda     #$10                                                    ; add one sector (x16)
F5B9  ABE4                                    adda    ,s                                                              ; to pushed value                                                                                                                                                       ; 30
F5BB  A7E4                                    sta     ,s                                                              ; update it in the stack                                                                                                                                ; 30
F5BD  3502            flK01           puls    a                                                               ; get number of sectors to kill (x16 'cause in high nibble)                                     ; 30 - 20
F5BF  8140                                    cmpa    #$40                                                    ; number of sectors greater than 4?
F5C1  2302                                    bls     flK02                                                   ; no, go on
F5C3  8640                                    lda     #$40                                                    ; yes, limit to 4
F5C5  1F89            flK02           tfr     a,b                                                     ; copy to B                                                                                                                                                                     ; 30 - 20
F5C7  54                                      lsrb                                                                    ; move
F5C8  54                                      lsrb                                                                    ; 4 times to
F5C9  54                                      lsrb                                                                    ; the right to
F5CA  54                                      lsrb                                                                    ; divide by 16 to get the actual number of sectors      to kill                                 ; 03 - 02
F5CB  96FA                                    lda     <dDest                                          ; get destination ini high byte                                                                                                         ; D3 - C0       ($D300 - $C000)
F5CD  84F0                                    anda    #$f0                                                    ; use high nibble                                                                                                                                                       ; D0 - C0
                                                                                                                      ; now D should contain: 
                                                                                                                      ;     A = High byte of address of 1st sector to kill ($C0-$D0-$E0-$F0)
                                                                                                                      ;     B = number of sectors to be killed
                                                                                                                      ; save parameter for later use
F5CF  9752                     sta   <FPA0+2                 ; store MSB of default sector address: $52
F5D1  D703                     stb   <ErasCtr                ; store sector count: $03
F5D3  0F53                     clr   <FPA0+3                 ; clear LSB of sector address: $53
F5D5  308C65                   leax  eraser,pcr              ; point X at erasure routine
F5D8  9FEE                     stx   <DCBPT                                           ; save it: $ee
F5DA  9686            callX    lda   <TargBank                        ; get current flash bank selection: $86
F5DC  8407                     anda  #$07                    ; mask out undefined bits
F5DE  3403                     pshs  a,cc                    ; save current bank and IRQ mask
F5E0  AD9F00EE                 jsr   [DCBPT]                 ; call utility routine: $ee -> eraser (routine)
F5E4  B6FF22                   lda   $ff22                   ; read PIA data reg to clear any CART interrupt
F5E7  3503                     puls  cc,a                    ; restore old IRQ mask and pop saved bank number
F5E9  7FFF4A                   clr   FLSHREG                 ; clear the Flash byte register: $ff4a
F5EC  39                       rts                           ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Write Data to Flash
                      ; Entry:
                      ;   X = source data address
                      ;   U = destination flash address
                      ;   High word of FPA0 = byte count ($50)
                      ; -------------------------------------------------------------------------------------------------------------------------------
F5ED  DC50            burner   ldd   <FPA0                   ; D = byte count: $50
F5EF  31CB                     leay  d,u                     ; Y = destination end address
F5F1  109F52                   sty   <FPA0+2                 ; save end address in FPA0: $52
F5F4  3184                     leay  ,x                      ; point Y at the source data
F5F6  30C4                     leax  ,u                      ; point X at the destination
F5F8  338C1F                   leau  burnPrfx,PCR            ; point U at the prefix data for Byte Program
F5FB  9686                     lda   <TargBank               ; get target bank number: $86
F5FD  B7FF4B                   sta   BANKREG                 ; activate target bank: ff4b
F600  8A80                     ora   #$80                    ; setup bank number combined with PGM Enable
F602  E6A0            burn010  ldb   ,y+                     ; get next data byte from source
F604  E184                     cmpb  ,x                      ; does flash already contain this value?
F606  2604                     bne   burn020                 ; branch if needs to change
F608  3001                     leax  1,x                     ; increment flash address
F60A  2008                     bra   burn030                 ; skip ahead to end of loop
F60C  3456            burn020  pshs  u,x,b,a                 ; save registers
F60E  8D17                     bsr   flshPrefx               ; send the unlock sequence
F610  3556                     puls  a,b,x,u                 ; restore target bank and destination address
F612  8D20                     bsr   flshWrCyc               ; write the byte to the Flash destination
F614  9C52            burn030  cmpx  <FPA0+2                 ; have we reached the final address: $52
F616  25EA                     blo   burn010                 ; loop if more
F618  2016                     bra   flshDone                ; restore original bank selection and return
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; - - -  Prefix Data for Byte-Program  - - -
F61A  81AA            burnPrfx fdb   $81aa                   ; Bank = 1, Data = $AA...
F61C  D555                     fdb   $d555                   ; ...Address = $D555 ($5555)
F61E  8055                     fdb   $8055                   ; Bank 0, Data = $55...
F620  EAAA                     fdb   $eaaa                   ; ...Address = $EAAA ($2AAA)
F622  81A0                     fdb   $81a0                   ; Bank 1, Data = $A0...
F624  D555                     fdb   $d555                   ; ...Address = $D555 ($5555)
F626  00                       fcb   $0                                 ; end of sequence
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Send a Programming Prefix Sequence to the Flash.
                      ; Enter with U pointing to Prefix data.
                      ; -------------------------------------------------------------------------------------------------------------------------------
F627                  flshPrefx
F627  3716                                    pulu  a,b,x                   ; get next bank, data byte and address
F629  4D                       tsta                          ; if reached end-of-sequence..
F62A  2710                     beq   flshRts                 ; ..then return
F62C  8D06                     bsr   flshWrCyc               ; write byte to flash
F62E  20F7                     bra   flshPrefx               ; loop for next byte of sequence
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; Restores old flash bank and returns upon completion of Flash operation
F630  A663            flshDone lda   3,s                     ; get orignal bank number from stack
F632  847F                     anda  #$7f                    ; make sure the PGM Enable bit is cleared
                                                                                                                      ;  *** Fall Thru ***
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                      ; Perform a Write Cycle to the Flash.
                      ; Enter with Address in X, Bank+PGM Enable in ACCA and the data byte in ACCB.
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
F634                  flshWrCyc
F634  B7FF4B                                  sta   BANKREG                 ; select Flash bank / PGM Enable: $ff4b
F637  F7FF4A                   stb   FLSHREG                 ; store data byte in Flash register: $ff4a
F63A  E580                     bitb  ,x+                     ; perform Flash Write by reading target address
F63C  39              flshRts  rts                           ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; Erase Flash Sector
                      ; -------------------------------------------------------------------------------------------------------------------------------
F63D  338C27          eraser   leau  erasPrfx,PCR            ; point U to the prefix data for Sector Erase
F640  8DE5                     bsr   flshPrefx               ; write the prefix bytes
F642  CC8030                   ldd   #$8030                  ; ACCA = PGM Enable;  ACCB = final prefix byte
F645  9A86                     ora   <TargBank               ; combine target bank number with PGM Enable: $86
F647  9E52                     ldx   <FPA0+2                 ; X = sector address: $52 ($C0xx)
F649  8DE9                     bsr   flshWrCyc               ; write final prefix byte at sector address
F64B  8407                     anda  #$07                    ; turn off the..
F64D  B7FF4B                   sta   BANKREG                 ; ..PGM Enable bit: $ff4b
F650  4F                       clra                          ; clear ACCD for use..
F651  5F                       clrb                          ; ..as timeout counter
F652  6D1F            eras025  tst   -1,x                    ; poll the sector address: $C0xx-1
F654  2B05                     bmi   eras030                 ; branch if bit 7 is set (erasure complete)
F656  830001                   subd  #1                      ; decrement timeout counter
F659  26F7                     bne   eras025                 ; loop if not yet timed out
F65B  30890FFF        eras030  leax  $0fff,x                 ; increment X to point at the next sector
F65F  9F52                     stx   <FPA0+2                 ; save sector address in FPA0: $52 ($D0xx)
F661  0A03                     dec   <ErasCtr                ; decrement erase sector counter: $03
F663  26D8                     bne   eraser                  ; loop if erasing another sector
F665  20C9                     bra   flshDone                ; restore original bank selection and return
                      ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                                                                                                                      ; - - -  Programming Prefix for Sector Erase  - - -
F667  81AA            erasPrfx fdb   $81aa                   ; Bank = 1, Data = $AA...
F669  D555                     fdb   $d555                   ; ...Address = $D555 ($5555)
F66B  8055                     fdb   $8055                   ; Bank 0, Data = $55...
F66D  EAAA                     fdb   $eaaa                   ; ...Address = $EAAA ($2AAA)
F66F  8180                     fdb   $8180                   ; Bank 1, Data = $80...
F671  D555                     fdb   $d555                   ; ...Address = $D555 ($5555)
F673  81AA                     fdb   $81aa                   ; Bank = 1, Data = $AA...
F675  D555                     fdb   $d555                   ; ...Address = $D555 ($5555)
F677  8055                     fdb   $8055                   ; Bank 0, Data = $55...
F679  EAAA                     fdb   $eaaa                   ; ...Address = $EAAA ($2AAA)
F67B  0000                     fdb   $0000                   ; end of sequence
F67D  0000            erasEnd  fdb   $0000                                                    ; ...a record is 4 bytes long ...
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; process for command SCOPY MEM @number   ,&HC000,&H4000,&H3F00
                      ; or alternate        SCOPY MEM SLOTnumber,&HC000,&H4000,&H3F00
                      ; -------------------------------------------------------------------------------------------------------------------------------
F67F  9D9F            xscopy  jsr     <getNChr                                                ; get next char
F681  819B                                    cmpa    #$9b                                                    ; is the token for MEM?
F683  1026F0EE                                lbne    snError                                         ; no, show error message
F687  9D9F                                    jsr     <getNChr                                                ; get next byte
F689  97F9                                    sta     <cpyWhat                                                ; save received command
F68B  8140                                    cmpa    #64                                                     ; is it sign '@' for BANK?
F68D  2717                                    beq     xsbnk01                                         ; yes, go get bank number
F68F  81EF                                    cmpa    #$ef                                                    ; is the token for SLOT?
F691  1026F0E0                                lbne    snError                                         ; no, show error
                                                                                                                      ; processing slot command
F695  9D9F                                    jsr     <getNChr                                                ; get number into A
F697  8131                                    cmpa    #$31                                                    ; is greater or equal 1?
F699  2507                                    bcs     xscerr1                                         ; no, show error
F69B  8134                                    cmpa    #$34                                                    ; is less or equal 4?
F69D  2203                                    bhi   xscerr1                                           ; no, show error 
F69F  4A                                      deca                                                                    ; internally MUST work with 0-1-2-3
F6A0  2013                                    bra     xsParam                                         ; yes, goto next parameters
F6A2  C60D            xscerr1 ldb     #$0d                                                    ; MU error
F6A4  200C                                    bra     xscerr0                                         ; goto bas_system_error
                                                                                                                      ; processing bank command
F6A6  9D9F            xsbnk01 jsr     <getNChr                                                ; get number into A
F6A8  8130                                    cmpa    #$30                                                    ; is greater or equal 0?
F6AA  2504                                    bcs     xscerr2                                         ; no, show error
F6AC  8137                                    cmpa    #$37                                                    ; is less or equal 7?
F6AE  2305                                    bls     xsParam                                         ; yes, goto next parameters
F6B0  C611            xscerr2         ldb     #$11                                                    ; SD error
F6B2  7E8344          xscerr0 jmp     syserr                                          ; bas_system_error
                                                                                                                      ; look for more parameters
F6B5  8030            xsParam suba    #$30                                                    ; convert ASCII to integer (0-3 or 0-7)
F6B7  97F8                                    sta     <oriNum                                                 ; save requested bank/slot number
F6B9  9D9F                                    jsr     <getNChr                                                ; advance one char
F6BB  260F                                    bne     otherPar                                                ; if more chars, read them
                                                                                                                      ; default parameters for writting a ROMPACK
F6BD  8EC000                                  ldx     #$c000                                          ; get origin of data
F6C0  9F76                                    stx     <dSource                                                ; save start into variable
F6C2  8E3000                                  ldx     #$3000                                          ; get destination in bank
F6C5  9FFA                                    stx     <dDest                                          ; save destination into variable
F6C7  8E3F00                                  ldx     #$3f00                                          ; get max. length (16k-256bytes)
F6CA  2028                                    bra     storeLen                                                ; skip section                  
F6CC  BD89AA          otherPar        jsr     ckComma                                         ; CkComma
F6CF  BD8E83                                  jsr     get16bN                                         ; get a 16 bit number into X
F6D2  8CC000                                  cmpx    #romini                                         ; is it on the upper 32k?
F6D5  1025FE89                                lblo    badParam                                                ; no, bad format number, show error
F6D9  9F76                                    stx     <dSource                                                ; save start into variable
F6DB  9DA5                                    jsr     <redLChr                                                ; advance one char
F6DD  BD89AA                                  jsr     ckComma                                         ; CkComma
F6E0  BD8E83                                  jsr     get16bN                                         ; get a 16 bit number into X
F6E3  8C4000                                  cmpx    #$4000                                          ; is it leaving space for a 16k ROM?
F6E6  1022FE78                                lbhi    badParam                                                ; yes, bad format number, show error
F6EA  9FFA                                    stx     <dDest                                          ; save destination into variable
F6EC  9DA5                                    jsr     <redLChr                                                ; advance one char
F6EE  BD89AA                                  jsr     ckComma                                         ; CkComma
F6F1  BD8E83                                  jsr     get16bN                                         ; get a 16 bit number into X
F6F4  1F10            storeLen        tfr     x,d                                                     ; pass value to D
F6F6  D3FA                                    addd    <dDest                                          ; add destination value to calculate end value
F6F8  10837FFF                                cmpd    #basini-1                                       ; does result exceed the RAM area?
F6FC  1022FE62                                lbhi    badParam                                                ; yes, bad format number, show error
F700  DD50                                    std     <FPA0                                                   ; save last byte to be written
F702  1F10                                    tfr     x,d                                                     ; get byte count again
F704  D376                                    addd    <dSource                                                ; add first one to copy
F706  1025FE58                                lbcs    badParam                                                ; if carry it goes past $FFFF, show message
                                                                                                                      ; stack control
F70A  DC50                                    ldd     <FPA0                                                   ; get last byte to write
F70C  C30032                                  addd    #50                                                     ; 50 security bytes (stack depth)
F70F  109321                                  cmpd    <$21                                                    ; is last calculated address lower than stack pointer?
F712  250D                                    blo     stkOK                                                   ; yes, so it's sure to continue
F714  DCFA                                    ldd     <dDest                                          ; get first destination byte in RAM
F716  109323                                  cmpd    <$23                                                    ; is higher than strings beginning area?
F719  2206                                    bhi     stkOK                                                   ; yes, so it's sure to continue
F71B  8EE4F6                                  ldx     #stackPrb-1                                     ; point to message
F71E  7E90E5                                  jmp     outstr                                          ; show it
                                                                                                                      ; HDW verification
F721  96F9            stkOK           lda     cpyWhat                                         ; get kind of origin to copy
F723  81EF                                    cmpa    #$ef                                                    ; is SLOT requested?
F725  2609                                    bne     seeBank                                         ; no, go to bank control
F727  17F4A9                                  lbsr    tstMPI                                          ; yes, is there an active MPI?
F72A  270D                                    beq     xsProces                                                ; yes go on     
F72C  17F3F4                                  lbsr    NotMPIPresent                           ; no, show message
F72F  39                                      rts                                                                     ; return
F730  B60114          seeBank lda     floSDC                                          ; get hdw control byte
F733  8508                                    bita    #%00001000                                      ; is there an SDC?
F735  1027F3EE                                lbeq    NotSDCPresent                           ; no, show error
                                                                                                                      ; section to copy the code to be executed into $1da
F739  8EF74C          xsProces        ldx     #xsMovCod                                       ; point to origin of code
F73C  CE01DA                                  ldu     #$1da                                                   ; point to destination
F73F  EC81            xsMov01 ldd     ,x++                                                    ; get a word
F741  EDC1                                    std     ,u++                                                    ; put at destination
F743  8CF7E9                                  cmpx    #xsLoEnd+1                                      ; got to the end?
F746  25F7                                    blo     xsMov01                                         ; no, loopback
F748  BD01DA                                  jsr     $1da                                                    ; call code at $1da
F74B  39                                      rts                                                                     ; return
                                                                                                                      ; code to be executed
F74C  1A50            xsMovCod        orcc    #dimask                                         ; disable interrupts
F74E  4F                                      clra                                                                    ; $00 is code for NO old value
F74F  9752                                    sta     oldSlot                                         ; for old slot
F751  4A                                      deca                                                                    ; $ff is code for NO old value
F752  9753                                    sta     oldBank                                         ; for old bank
F754  96F9                                    lda     <cpyWhat                                                ; get kind of element to copy
F756  8140                                    cmpa    #64                                                     ; was command a copy BANK?
F758  271D                                    beq     xsCopBnk                                                ; yes, go there
                                                                                                                      ; change to the requested slot if not already there and save old slot
F75A  B603FF                                  lda     cpyMPIcfg                                       ; get active slot number
F75D  9752                                    sta     <oldSlot                                                ; save it
F75F  8403                                    anda    #3                                                              ; to have just the number
F761  91F8                                    cmpa    <oriNum                                         ; is the requested one?
F763  2742                                    beq     startCpy                                                ; yes, go start the copy
F765  D6F8                                    ldb     <oriNum                                         ; get requested slot number
F767  3404                                    pshs    b                                                               ; save it
F769  58                                      lslb                                                                    ; move to
F76A  58                                      lslb                                                                    ; the high
F76B  58                                      lslb                                                                    ; nibble
F76C  58                                      lslb                                                                    ; so 
F76D  EAE0                                    orb     ,s+                                                     ; now both are equal
F76F  F703FF                                  stb     cpyMPIcfg                                       ; update copy
F772  F7FF7F                                  stb     MPIcfg                                          ; change of slot
F775  2030                                    bra     startCpy                                                ; done, go start the copy
F777  B60114          xsCopBnk        lda     floSDC                                          ; get hdw control byte
F77A  84C0                                    anda    #%11000000                                      ; use bits 7-6 (slot where SDC is)
F77C  44                                      lsra                                                                    ; move them to
F77D  44                                      lsra                                                                    ; bits 5-4 (lower bits of high nibble)
F77E  3402                                    pshs    a                                                               ; save high nibble
F780  44                                      lsra                                                                    ; low nibble   
F781  44                                      lsra                                                                    ; gets high nibble
F782  44                                      lsra                                                                    ; that will end  
F783  44                                      lsra                                                                    ; as zero
F784  AAE0                                    ora     ,s+                                                     ; add high nibble (now they are the same value)
F786  B103FF                                  cmpa    cpyMPIcfg                                       ; is this slot active right now?
F789  270B                                    beq     cmpBank                                         ; yes, go see if bank differs
F78B  F603FF                                  ldb     cpyMPIcfg                                       ; get actual slot
F78E  B703FF                                  sta     cpyMPIcfg                                       ; update copy
F791  B7FF7F                                  sta     MPIcfg                                          ; change of slot
F794  D752                                    stb     <oldSlot                                                ; save old slot
F796  96F8            cmpBank lda     <oriNum                                         ; compare requested bank with actually selected one 
F798  B1FF4B                                  cmpa    $ff4b                                                   ; is active now?
F79B  2708                                    beq   savBank                                           ; yes, save bank
F79D  F6FF4B                                  ldb     $ff4b                                                   ; no, get active one
F7A0  B7FF4B                                  sta     $ff4b                                                   ; change to requested
F7A3  1F98                                    tfr     b,a                                                     ; pass B to A
F7A5  9753            savBank sta     <oldBank                                                ; save old bank number
F7A7  5F              startCpy        clrb                                                                    ; 0 = not in MAP1
F7A8  B6C000                                  lda     romini                                          ; get value in $C000
F7AB  43                                      coma                                                                    ; reverse bits
F7AC  B7C000                                  sta     romini                                          ; update ram cell
F7AF  B1C000                                  cmpa    romini                                          ; retains the new value?
F7B2  2608                                    bne     noMap1                                          ; no, we are in MAP0
F7B4  43                                      coma                                                                    ; reverse again
F7B5  B7C000                                  sta     romini                                          ; restore value
F7B8  5C                                      incb                                                                    ; 1 = we are in MAP1
F7B9  B7FFDE                                  sta     $ffde                                                   ; switch to MAP0 to copy the requested ROM
F7BC  D7F8            noMap1  stb     <oriNum                                         ; re-use variable to save that value
F7BE  9E76                                    ldx     <dSource                                                ; point to source ROM
F7C0  DEFA                                    ldu     <dDest                                          ; point to destination RAM
F7C2  EC81            xsLoop01        ldd     ,x++                                                    ; get a word
F7C4  EDC1                                    std     ,u++                                                    ; put at destination
F7C6  119350                                  cmpu    <FPA0                                                   ; got to the end?
F7C9  23F7                                    bls     xsLoop01                                                ; no, loopback
                                                                                                                      ; restore old bank-slot numbers and Map Type
F7CB  9652                                    lda     <oldSlot                                                ; get old slot
F7CD  2706                                    beq     getBank                                         ; if $00, do nothing
F7CF  B703FF                                  sta     cpyMPIcfg                                       ; update copy
F7D2  B7FF7F                                  sta     MPIcfg                                          ; change to old slot number
F7D5  9653            getBank lda     <oldBank                                                ; get old bank number
F7D7  2B03                                    bmi     exscopy                                         ; if $ff, do nothing
F7D9  B7FF4B                                  sta     $ff4b                                                   ; switch to old bank number
F7DC  0DF8            exscopy tst     <oriNum                                         ; were we initially in MAP1?
F7DE  2703                                    beq     noMapChg                                                ; no, skip next
F7E0  B7FFDF                                  sta     $ffdf                                                   ; back to MAP1
F7E3  1CAF            noMapChg        andcc   #eimask                                         ; enable interrupts
F7E5  7FFF48                                  clr     $ff48                                                   ; to unsure no led stays lit on FDC
F7E8  39              xsLoEnd rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; routine to verify the status returned by the SDC to a command
                      ;         Bit 7 ($80) set on any failure.
                      ;         Bit 5 ($20) set if target file is already in use.
                      ;         Bit 4 ($10) set if target file not found.
                      ;         Bit 3 ($08) set on miscellaneous hardware errors.
                      ;         Bit 2 ($04) set if path name is invalid.                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
F7E9                  vfyStatus
F7E9  242D                                    bcc     evfy                                                    ; if no error return
F7EB  5D                       tstb                                                                   ; update flags upon result status 
F7EC  2724                     beq    timeOutError                            ; if zero, timeout
F7EE  C520                     bitb   #$20                                                    ; detected file in use?
F7F0  2610                     bne    targetInUseError                        ; yes, message
F7F2  C510                     bitb   #$10                                                    ; was file not found?
F7F4  2610                     bne    targetNotFoundError             ; no, message
F7F6  C508                               bitb         #$08                                                    ; was a hdw error?
F7F8  2610                     bne    miscHardwareError                       ; yes, message
F7FA  C504                     bitb   #$04                                                    ; was invalid pathname?
F7FC  2610                     bne    pathNameInvalidError            ; yes, message
F7FE                  unknownError                                                                    ; this is for unknown errors. Should not get here!
F7FE  8EE55C                                  ldx     #unknown-1                                      ; point to message
F801  10                       fcb    #$10                                                    ; skip next ldx 
F802                  targetInUseError                                                                ; 
F802  8EE510                   ldx    #targetInUse-1                          ; point to message
F805  10                       fcb    #$10                                                    ; skip next ldx 
F806                  targetNotFoundError                                                     ; 
F806  8EE56B                   ldx    #targetNotFound-1                       ; point to message 
F809  10                       fcb    #$10                                                    ; skip next ldx 
F80A                  miscHardwareError                                                               ; 
F80A  8EE546                                  ldx     #miscHardware-1                 ; point to message 
F80D  10                       fcb    #$10                                                    ; skip next ldx 
F80E                  pathNameInvalidError                                                    ; 
F80E  8EE534                                  ldx     #pathNameInvalid-1              ; point to message 
F811  10                       fcb    #$10                                                    ; skip next ldx 
F812                  timeOutError                                                                    ; 
F812  8EE507                                  ldx     #timeout-1                                      ; point to message 
F815  7E90E5          Exit            jmp   outstr                                            ; show message
F818  39              evfy            rts                                                                     ; return
                      
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
F819  3473            CommSDC  pshs  u,y,x,a,cc                               ; preserve resgisters
F81B  64E4                     lsr   ,s                                       ; shift carry flag out of saved cc
                                                                                                                      ; put controller in command mode
F81D  108EFF42                 ldy   #DATREGA                                 ; setup y for hardware addressing (ff42 - ff4a for coco)
F821  860B                     lda   #CMDMODE                                 ; the magic number
F823  A726                     sta   +6,y                                     ; was -10,y for coco - send to control latch (ff48 - ff40 for coco)
                                                                                                                      ; put input parameters into the hardware registers.
                                                                                                                      ; it does no harm to put random data in the
                                                                                                                      ; registers for commands which dont use them.
F825  E73F                     stb   -1,y                                     ; high byte to param reg 1 (ff41 - ff49 for coco)
F827  AFA4                     stx   ,y                                       ; low word to param regs 2 and 3 (ff42-ff43 - ff4a-ff4b for coco)
                                                                                                                      ; wait for not busy.
F829  8D61                     bsr   waitForIt                           ; run polling loop
F82B  2555                     bcs   cmdExit                             ; exit if error or timeout
                                                                                                                      ; send command to controller
F82D  A661                     lda   1,s                                 ; get preserved command code from stack
F82F  A73E                     sta   -2,y                                ; send to command register (ff40 - ff48 for coco)
                                                                                                                      ; determine if a data block needs to be sent.
                                                                                                                      ; any command which requires a data block will
                                                                                                                      ; have bit 5 set in the command code.
F831  8520                     bita  #$20                                     ; test the "send block" command bit
F833  272B                     beq   rxBlock                                  ; branch if no block to send
                                                                                                                              ; wait for ready to send
F835  8D55                     bsr   waitForIt                           ; run polling loop
F837  2549                     bcs   cmdExit                             ; exit if error or timeout
F839  30C4                     leax  ,u                                  ; move data address to x
                                                                                                                              ; send 256 bytes of data
F83B  CC2008                   ldd   #32*256+8                                ; 32 chunks of 8 bytes
F83E  EE84            txChunk  ldu   ,x                                       ; send one chunk...
F840  EFA4                     stu   ,y                                                       ; ff42-ff43 - ff4a-ff4b for coco
F842  EE02                     ldu   2,x
F844  EFA4                     stu   ,y
F846  EE04                     ldu   4,x
F848  EFA4                     stu   ,y
F84A  EE06                     ldu   6,x
F84C  EFA4                     stu   ,y
F84E  3A                       abx                                            ; point x at next chunk
F84F  4A                       deca                                           ; decrement chunk counter
F850  26EC                     bne   txChunk                                  ; loop until all 256 bytes sent
                                                                                                                      ; wait for command completion
F852  8605                     lda   #5                                       ; timeout retries
F854                  waitCmplt
F854  8D36                                    bsr   waitForIt                                 ; run polling loop
F856  C501                     bitb  #BUSY                                    ; test busy bit
F858  2728                     beq   cmdExit                                  ; exit if completed
F85A  4A                       deca                                           ; decrement retry counter
F85B  26F7                     bne   waitCmplt                                ; repeat until 0
F85D  43                       coma                                           ; set carry for timeout error
F85E  2022                     bra   cmdExit                                  ; exit
                                                                                                                      ; for commands which return a 256 byte response block the
                                                                                                                      ; controller will set the ready bit in the status register
                                                                                                                      ; when it has the data ready for transfer.   for commands
                                                                                                                      ; which do not return a response block the busy bit will
                                                                                                                      ; be cleared to indicate that the command has completed.
F860  8D26            rxBlock  bsr   longWait                                 ; run long status polling loop
F862  231E                     bls   cmdExit                                  ; exit if error, timeout or completed
F864  3041                     leax  1,u                                      ; test the provided buffer address
F866  271A                     beq   cmdExit                                  ; exit if "no buffer" ($ffff)
F868  30C4                     leax  ,u                                       ; move data address to x
                                                                                                                      ; read 256 bytes of data
F86A  CC2008                   ldd   #32*256+8                                ; 32 chunks of 8 bytes
F86D  EEA4            rxChunk  ldu   ,y                                       ; read one chunk... (ff42-ff43 - ff4a-ff4b for coco)
F86F  EF84                     stu   ,x
F871  EEA4                     ldu   ,y
F873  EF02                     stu   2,x
F875  EEA4                     ldu   ,y
F877  EF04                     stu   4,x
F879  EEA4                     ldu   ,y
F87B  EF06                     stu   6,x
F87D  3A                       abx                                            ; update x for next chunk
F87E  4A                       deca                                           ; decrement chunk counter
F87F  26EC                     bne   rxChunk                                  ; loop until all 256 bytes transferred
F881  5F                       clrb                                           ; status code for success, clear carry
                                                                                                                      ; exit
F882  69E4            cmdExit  rol   ,s                                       ; rotate carry into saved cc on stack
F884  6F26                     clr   +6,y                                     ; was -10,y for coco - end command mode (ff48 - ff40 for coco)
F886  35F3                     puls  cc,a,x,y,u,pc                            ; restore irq masks, update carry and return
                      
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
F888  8D02            longWait bsr   waitForIt                                ; enter here for doubled timeout
F88A  241B                     bcc   waitRet                                  ; return if cleared in 1st pass
F88C                  waitForIt
F88C  8E0000                                  ldx   #0                                        ; setup timeout counter
F88F  53              waitLp   comb                                           ; set carry for assumed FAIL
F890  E63E                     ldb   -2,y                                     ; read status (FF40 - FF48 for CoCo)
F892  2B13                     bmi   waitRet                                  ; return if FAILED
F894  54                       lsrb                                           ; BUSY --> Carry
F895  240E                     bcc   waitDone                                 ; branch if not busy
F897  C501                     bitb  #READY/2                                 ; test READY (shifted)
F899  260B                     bne   waitRdy                                  ; branch if ready for transfer
F89B  8D0A                     bsr   waitRet                                  ; consume some time
F89D  C681                     ldb   #$81                                     ; status = timeout
F89F  3082                     leax  ,-x                                      ; decrement timeout counter
F8A1  2704                     beq   waitRet                                  ; return if timed out
F8A3  20EA                     bra   waitLp                                   ; try again
F8A5  5F              waitDone clrb                                           ; Not Busy: status = 0, set Z
F8A6  59              waitRdy  rolb                                           ; On Ready: clear C and Z
F8A7  39              waitRet  rts                                            ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; EXPERimenTs area
                      ; to help Boot any 'bootable' disk in drive 1 if SPACE IS pressed
                      ; or start AUTOLOAD.DWL if SHIFT is NOT pressed
                      ; -------------------------------------------------------------------------------------------------------------------------------
F8A8  C601            expert  ldb     #1                                                              ; set drive number
F8AA  D7EB                                    stb     <$eb                                                    ; in system variable
F8AC  F7060A                                  stb     >$60a                                                   ; and in DOS area
F8AF  B6FF02                                  lda   $ff02                                                     ; save PIA value
F8B2  C67F                     ldb   #$7f                                                     ; value to test for Space & Shift
F8B4  F7FF02                   stb   $ff02                                                    ; put it in the PIA
F8B7  F6FF00                   ldb   $ff00                                                    ; read PIA output
F8BA  B7FF02                   sta   $ff02                                                    ; restore old value in PIA
F8BD  53                       comb                                                                   ; invert read bits
F8BE  C420                     andb  #$20                                                     ; is bit5 set?
F8C0  2605                                    bne   doBoot                                            ; yes, SPACE is pressed, BOOT
F8C2  BDC16A          noBoot  jsr   $c16a                                                     ; attempt to read a disk sector
F8C5  200B                                    bra     doDWAuto                                                ; simulate it has failed
F8C7  10CE7F36        doBoot  lds     #$7f36                                          ; set new stackpointer
F8CB  8D20                     bsr    rebReset                                                ; rebuild Reset
F8CD  1CAF                                    andcc   #$af                                                    ; enable interrupts
F8CF  7EC13D                                  jmp     $c13d                                                   ; call BOOT drive 1
F8D2  7FFF48          doDWAuto clr    $ff48                                                   ; ensure motors off
F8D5  1CAF                                    andcc #$af                                                      ; enable interrupts 
F8D7  8EB4B2                   ldx   #$b4b2                                           ; get Basic Intro message 
F8DA  BD90E5                   jsr   outstr                                           ; show it
F8DD  8EC34C                   ldx   #$c34c                                           ; get DOS intro message
F8E0  BD90E5                   jsr   outstr                                           ; show it
F8E3  8D08                                    bsr     rebReset                                                ; rebuild Reset
F8E5  170133                                  lbsr    chkshift                                                ; check for shift to call AUTOLOAD.DWL
F8E8                  retFDwl 
F8E8  1CAF                                    andcc   #$af                                                    ; enable interrupts
F8EA  7E8371                                  jmp     okprmpt                                         ; goto Basic Iterpreter
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
F8ED  8EC6B4          rebReset        ldx   #$c6b4                                            ; get new reset routine
F8F0  9F72                                    stx     <$72                                                    ; put in variables area
F8F2  8655                                    lda     #$55                                                    ; flag value for warmreset
F8F4  9771                                    sta     <$71                                                    ; update variable
F8F6  39                                      rts                                                                     ; return
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                      ; DWLOAD implementation for patched Dragon 32 ROM (upper 8KB) 2014 Tormod Volden
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                      ; Entry point from command line (DWLOAD or EXEC)
                                                                                                                      ; for later to see that we came from DWLOAD command
F8F7  0F11            dwload   clr   <bootflag                                        ; clear our autoboot flag
F8F9  8EFA05                   ldx   #dwtext-1                                        ; 
F8FC  BD90E5                   jsr   outstr                                           ; print string
F8FF  1A50            Start           orcc  #IntMasks                                 ; disable FIRQ, IRQ
F901  8EFF20                   ldx   #PIA1Base                        ; PIA1
F904  C602                     ldb   #$02
F906  E784                     stb   ,x                               ; make RS-232 output marking
                                                                                                                      ; Spin for a while so that the RS-232 bit stays hi for a time
F908  8EA000          Reset           ldx   #$a000
F90B  301F            Spin     leax  -1,x
F90D  26FC                     bne    Spin
                                                                                                                      ; Request named object from DW server
F90F  9611                     lda   <bootflag                                        ; coming from autoboot?
F911  263E                     bne   autoreq                                          ; then use default name
F913  9DA5                     jsr   <redLChr                                         ; peek at next character
F915  804E                     suba  #'N'                                                     ; DLOADN ?
F917  9710                     sta   <noexec                                          ; 0 = noexec
F919  2602                     bne   getn
F91B  9D9F                     jsr   <getNChr                                         ; read next char (the N)
F91D  BDB7AA          getn     jsr   getfnam                                          ; read file name and length into $1d1 = namebuf
F920  8E01D0                   ldx   #namebuf-1                                       ; packet buffer start
F923  6F84                     clr   ,x                                                               ; zero MSB for Y
F925  10AE81                   ldy   ,x++                                                     ; length of file name (16 bit)
F928  2729                     beq   noname                                           ; no file name given?
F92A  E61F                     ldb   -1,x                                                     ; length of name (8 bit)
F92C  6F85                     clr   b,x                                                      ; zero terminate name string (for error)
F92E  6C83                     inc   ,--x                                                     ; 1 = DriveWire OP_NAMEOBJ_MOUNT
F930  3122                     leay  2,y                                                      ; length of DW packet, name length + 2
F932  BDFB57          reqobj  jsr   DWWrite
F935  8E0012                   ldx   #dnum                                                    ; Our drive number variable
F938  6F84                     clr   ,x
F93A  3121                     leay  1,y                                                      ; read one byte back (Y is 0 after DWWrite)
F93C  BDFAD2                   jsr   DWRead                                           ; get drive number
F93F  2401                     bcc    reqOK                                                   ; if open has not failed go on
F941  39                                      rts                                                                     ; else return
F942  6D84            reqOK    tst   ,x
F944  2616                     bne   ReadDECB                                         ; successful mount
F946  0D11                     tst   <bootflag
F948  102600FC                 lbne  stealth                                          ; silent failure, BASIC OK prompt
F94C  C619                     ldb   #$19                                                     ; MO ERROR
F94E  7E8344                   jmp   syserr                                           ; system error
F951  9710            autoreq  sta   <noexec                          ; a is non null, autoexec
F953  8EFA36          noname   ldx   #autoname
F956  108E000E                 ldy   #12+2                                                    ; length of DW packet
F95A  20D6                     bra   reqobj
                                                                                                                      ; named object has been mounted, proceed to read in file
F95C  8E0000          ReadDECB        ldx   #0000
F95F  BF01CF                   stx   sector
F962  CE02D9                   ldu   #endbuf                                          ; start with empty buffer
F965  8CF96E                                  cmpx    #nextseg
F968  2104                                    brn     nextseg
F96A  10FF01CD                                sts     sstack
                                                                                                                      ; read DECB header
F96E  108E0005        nextseg  ldy   #5
F972  8E01D1                   ldx   #decbhdr                                         ; copy DECB header here
F975  BDFA58                   jsr   copybuf                                          ; moves x ahead
F978  A61B                     lda   -5,x                                             ; DECB segment header starts with zero
F97A  2753                     beq   ml                                               ; normal data segment
F97C  8155                     cmpa  #$55                                             ; Dragon DOS file?
F97E  261A                     bne   noddd
F980  3124                     leay  4,y                                              ; remaining header bytes (y is 0 here)
F982  BDFA58                   jsr   copybuf
F985  6A18                     dec   -8,x                                             ; Dragon DOS BASIC = 1
F987  2604                     bne   ddbin
F989  301F                     leax  -1,x                                             ; length at x-5
F98B  2022                     bra   ldbas
F98D  10AE1B          ddbin    ldy   -5,x                                             ; length
F990  AE19                     ldx   -7,x                                             ; load address
F992  BDFA58                   jsr   copybuf                                          ; read whole file
F995  8E01D9                   ldx   #decbhdr+8                                       ; exec address ptr + 2
F998  204B                     bra   oldsp
F99A  4C              noddd    inca
F99B  102600A6                 lbne  ErrDWL                                           ; must be $FF otherwise
F99F  3121                     leay  1,y                                              ; Y is 0 after copybuf
F9A1  10BC01CF                 cmpy  sector                                           ; only first sector can be BASIC header
F9A5  263A                     bne   endseg                                           ; otherwise end flag
F9A7  118301DE                 cmpu  #startbuf+5                                      ; must be first segment also
F9AB  2634                     bne   endseg
                                                                                                                      ; loading DECB BASIC file
                                                                                                                      ; bytes 4 and 5 are actually part of the program
F9AD  335E                     leau  -2,u                                             ; u was 5 bytes into read buffer here
                                                                                                                      ; at this point x is past 5-byte header, u is 3 bytes into first 256-bytes block
                                                                                                                      ; for Dragon DOS x is 8 bytes into header, u is 9 bytes into first 256-bytes block
F9AF  10AE1C          ldbas    ldy   -4,x                                             ; read whole BASIC program
F9B2  9E19                     ldx   <$19
F9B4  BDFA58                   jsr   copybuf
F9B7  BDF9D9                   jsr   prbang
                                                                                                                      ; set up BASIC pointers and finish
F9BA  9F1B                     stx   <$1b                                             ; end of BASIC program
F9BC  9F1D                     stx   <$1d
F9BE  9F1F                     stx   <$1f
F9C0  0D10                     tst   <noexec
F9C2  1027BD67                                lbeq  $b72d                                             ; print OK, run basvect1, basvect2, readline
F9C6  BD841F                   jsr   basvect1                                         ; BasVect1 reset BASIC stack etc
F9C9  BD83ED                   jsr   basvect2                                         ; BasVect2 initialize BASIC
F9CC  7E849F                   jmp   runbasic                                         ; run_basic
F9CF  10AE1C          ml       ldy   -4,x                                             ; DECB segment length
F9D2  AE1E                     ldx   -2,x                                             ; DECB segment loading address
F9D4  BDFA58                   jsr   copybuf
F9D7  2095                     bra   nextseg
F9D9  BD9A89          prbang   jsr   backspc                                          ; print backspace to devnum
F9DC  8621                     lda   #'!'                                             ; print bang
F9DE  7EB54A                   jmp   outchr                                           ; print to devnum
F9E1  EE1C            endseg  ldu   -4,x                                              ; new stack pointer specified?
F9E3  2603                     bne   setsp
F9E5  FE01CD          oldsp    ldu   sstack                                           ; otherwise restore original SP
F9E8  1F34            setsp    tfr   u,s
F9EA  8DED                     bsr   prbang
F9EC  1CAF                     andcc #~IntMasks                                       ; enable interrupts
F9EE  AE1E                     ldx   -2,x                                             ; exec address
F9F0  1F10                     tfr   x,d
F9F2  4C                       inca
F9F3  2708                     beq   retbas                                           ; return to basic if exec address $FFxx
F9F5  9F9D                     stx   <$9d                                             ; save BASIC EXEC address
F9F7  0D10                     tst   <noexec
F9F9  2702                     beq   retbas
F9FB  AD84                     jsr   ,x                                               ; and run loaded program
F9FD  39              retbas   rts
                                                                                                                      ; vector table for chainloaders, etc
F9FE  FAD2            dwrvec   fdb   DWRead
FA00  FB57            dwwvec   fdb   DWWrite
FA02  FA84            dwdvec   fdb   DoRead
FA04  0000                     fdb   0
FA06  44574C4F4144    dwtext   fcc   /DWLOAD/
FA0C  0D00                     fcb   $0d,0
FA0E  EEE1            fromboot ldu   ,s++                                                     ; check return address (and drop it)
FA10  1183B469                 cmpu  #$b469                                           ; ROM code location (not a copy)
FA14  263F                     bne   gocmdl                                           ; return if run from a copied code segment
FA16  8CB44F                   cmpx  #wrmStrt                                         ; coldstart sets this, warmstart doesn't
FA19  263A                     bne   gocmdl                                           ; return if warmstart
                                                                                                                      ; check for SHIFT key
FA1B  B6FF02          chkshift lda   $ff02                                                    ; save PIA
FA1E  C67F                     ldb   #$7f
FA20  F7FF02                   stb   $ff02
FA23  F6FF00                   ldb   $ff00
FA26  B7FF02                   sta   $ff02                                                    ; restore PIA
FA29  53                       comb
FA2A  C440                     andb  #$40
FA2C  2625                                    bne   ste01                                                     ; exit but enabling interrupts
FA2E  5C                       incb                                                                   ; B was 0 before
FA2F  D711                     stb   <bootflag                                        ; our autoboot flag = 1
FA31  BDF8FF                   jsr   Start
FA34  201D                                    bra     ste01                                                   ; exit but enabling interrupts
FA36  01              autoname fcb   $01                                                      ; OP_NAMEOBJ_MOUNT
FA37  0C                       fcb   12                                                               ; length of name string
FA38  4155544F4C4F41442E44574C00          fcn   /AUTOLOAD.DWL/
FA45  16BE03          ErrDWL   lbra  ioerror                                          ; BASIC IO ERROR
                      
FA48  CCF8E8          stealth  ldd    #retFDwl                                                ; get return address from DWLOAD
FA4B  10A362                                  cmpd    2,s                                                     ; is this on stack as 2nd retAdrs?
FA4E  2603                                    bne     ste01                                                   ; no, skip next one
FA50  3262                                    leas    2,s                                                     ; get rid of return address (from checkshift)
FA52  39                                      rts                                                                     ; return
                      
FA53  1CAF            ste01           andcc #~IntMasks                                        ; enable interrupts
FA55  7E8371          gocmdl   jmp   okprmpt                                          ; BASIC OK prompt
                                                                                                                      ; copy y chars from read buffer to x, updates u
FA58                  copybuf
FA58  118302D9        copyl    cmpu  #endbuf
FA5C  261D                     bne   copym
                                                                                                                      ; fill up buffer via DW - resets buffer pointer u
FA5E  3430                     pshs  x,y
FA60  BE01CF                   ldx   sector
FA63  108E01D9                 ldy   #startbuf
FA67  1F23                     tfr   y,u
FA69  8D19                     bsr   DoRead
FA6B  25D8                     bcs   ErrDWL
FA6D  26D6                     bne   ErrDWL
FA6F  3001                     leax  1,x
FA71  BF01CF                   stx   sector
FA74  862E                     lda   #'.'                                                     ; print dot
FA76  BDB54A                   jsr   outchr
FA79  3530                     puls  x,y
FA7B  A6C0            copym    lda   ,u+
FA7D  A780                     sta   ,x+
FA7F  313F                     leay  -1,y
FA81  26D5                     bne   copyl
FA83  39                       rts
                      
                      ; -------------------------------------------------------------------------------------------------------------------------------
                                                                                                                              ; below code is taken from toolshed/dwdos
FA84  9612            DoRead  lda   <dnum                                                     ; our drive number
FA86  5F                       clrb                                                                   ; LSN bits 23-16
FA87  3436                     pshs  d,x,y
FA89  86D2                     lda   #OP_READEX
FA8B  3402            ReRead   pshs  a
FA8D  30E4                     leax  ,s
FA8F  108E0005                                ldy   #$0005
FA93  1700C1                                  lbsr  DWWrite
FA96  3502                                    puls  a
FA98  AE64                                    ldx   4,s                                                       ; get read buffer pointer
FA9A  108E0100                                ldy   #256                                                      ; read 256 bytes
FA9E  CC0085                                  ldd   #133*1                                            ; 1 second timeout
FAA1  8D2F                                    bsr   DWRead
FAA3  252B                     bcs   ReadEx
FAA5  2629                     bne   ReadEx
                                                                                                                      ; Send 2 byte checksum
FAA7  3420                                    pshs  y
FAA9  30E4                                    leax  ,s
FAAB  108E0002                                ldy   #2
FAAF  1700A5                                  lbsr  DWWrite
FAB2  108E0001                                ldy   #1
FAB6  CC0085                                  ldd   #133*1
FAB9  8D17                                    bsr   DWRead
FABB  3262                                    leas  2,s
FABD  2511                                    bcs   ReadEx
FABF  260F                    bne   ReadEx
FAC1  A6E4                                    lda   ,s
FAC3  270B                                    beq   ReadEx
FAC5  81F3                                    cmpa  #E_CRC
FAC7  2606                                    bne   ReadErr
FAC9  86F2                                    lda   #OP_REREADEX
FACB  6FE4                                    clr   ,s
FACD  20BC                                    bra   ReRead  
FACF  53              ReadErr  comb
FAD0  35B6            ReadEx  puls  d,x,y,pc
                      
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
                      DWRead   clra                                   ; clear Carry (no framing error)
                               deca                                   ; clear Z flag, A = timeout msb ($ff)
                               tfr    cc,b
                               pshs  u,x,dp,b,a                       ; preserve registers, push flags and timeout msb
                               leau  ,x                                                               ; U points to received data buffer
                               ldx   #$0000                                           ; checksum to zero
                               leas   -2,s                                                    ; create a word hole for a 16 bits counter
                      loop0    ldd    #$F000                                          ; reset counter to a fixed value
                                              std     ,s                                                              ; to count 16 times 256
                      loop1    dec    1,s                                                     ; decrement low byte counter
                                              bne     loop2                                                   ; if not zero, skip next byte control
                                              inc     ,s                                                              ; increment high byte
                                              beq     notReady                                                ; if zero, exit via notReady
                      loop2           ldb   $ff49                                                     ; pol if DW is ready
                               bitb  #$02                                                     ; is ready?
                               beq   loop1                                                    ; no, loop
                               ldb   $ff4a                                                    ; get data byte
                               stb   ,u+                                                      ; put at destination buffer
                               abx                                                                    ; accumulate in checksum word
                               leay  ,-y                                                      ; decrement byte counter
                                              bne     loop0                                                   ; not all received, loopback
                                              leas    2,s                                                     ; discard word counter off the stack
                               tfr   x,y                                                      ; pass checksum to Y
                               ldb   #0
                               lda   #3
                               leas  1,s                              ; remove timeout msb from stack
                      outLoop  inca                                   ; A = status to be returned in C and Z (bit2-Z; bit0-C), now A=4 (Z=1 C=0)
                               ora   ,s                               ; place status information into the..
                               sta   ,s                               ; ..C and Z bits of the preserved CC
                               leay  ,x                               ; get checksum into Y
                               puls  cc,dp,x,u,pc                     ; restore registers and return
                      
                      notReady leas   3,s                                                     ; to remove word counter and timeout
                                              clra                                                                    ; to get a #$01 in next opcode (to set Carry flag)
                                              bra     outLoop                                         ; update flags and exit
                      
                                              ELSE
FAD2  4F              DWRead   clra                                   ; clear Carry (no framing error)
FAD3  4A                       deca                                   ; clear Z flag, A = timeout msb ($ff)
FAD4  1FA9                     tfr   cc,b
FAD6  345E                     pshs  u,x,dp,b,a                       ; preserve registers, push timeout msb
FAD8  1F8B                     tfr   a,dp                             ; set direct page to $FFxx
FADA                           setdp $ff
FADA  3384                     leau  ,x                               ; U = storage ptr
FADC  8E0000                   ldx   #0                               ; initialize checksum
FADF  8601                     lda   #$01                             ; A = serial in mask
FAE1  2032                     bra   rx0030                           ; go wait for start bit
                                                                                                                      ; Read a byte
FAE3  3341            rxByte   leau  1,u                              ; bump storage ptr
FAE5  31A2                     leay  ,-y                              ; decrement request count
FAE7  9622                     lda   <BBIN                            ; read bit 0
FAE9  44                       lsra                                   ; move bit 0 into Carry
FAEA  CCFF20                   ldd   #$ff20                           ; A = timeout msb, B = shift counter
FAED  A7E4                     sta   ,s                               ; reset timeout msb for next byte
FAEF  56                       rorb                                   ; rotate bit 0 into byte accumulator
FAF0  9622            rx0010   lda   <BBIN                            ; read bit (d1, d3, d5)
FAF2  44                       lsra
FAF3  56                       rorb
FAF4  A561                     bita  1,s                              ; 5 cycle delay
FAF6  2508                     bcs   rx0020                           ; exit loop after reading bit 5
FAF8  9622                     lda   <BBIN                            ; read bit (d2, d4)
FAFA  44                       lsra
FAFB  56                       rorb
FAFC  33C4                     leau  ,u
FAFE  20F0                     bra   rx0010
FB00  9622            rx0020   lda   <BBIN                            ; read bit 6
FB02  44                       lsra
FB03  56                       rorb
FB04  31A4                     leay  ,y                               ; test request count
FB06  2737                     beq   rx0050                           ; branch if final byte of request
FB08  9622                     lda   <BBIN                            ; read bit 7
FB0A  44                       lsra
FB0B  56                       rorb                                   ; byte is now complete
FB0C  E75F                     stb   -1,u                             ; store received byte to memory
FB0E  3A                       abx                                    ; update checksum
FB0F  9622                     lda   <BBIN                            ; read stop bit
FB11  8401                     anda  #$01                             ; mask out other bits
FB13  2737                     beq   rxExit                           ; exit if framing error
                                                                                                                      ; Wait for a start bit or timeout
FB15  9522            rx0030   bita  <BBIN                            ; check for start bit
FB17  27CA                     beq   rxByte                           ; branch if start bit detected
FB19  9522                     bita  <BBIN                            ; again
FB1B  27C6                     beq   rxByte
FB1D  C6FF                     ldb   #$ff                             ; init timeout lsb
FB1F  9522            rx0040   bita  <BBIN
FB21  27C0                     beq   rxByte
FB23  C001                     subb  #1                               ; decrement timeout lsb
FB25  9522                     bita  <BBIN
FB27  27BA                     beq   rxByte
FB29  24F4                     bcc   rx0040                           ; loop until timeout lsb rolls under
FB2B  9522                     bita  <BBIN
FB2D  27B4                     beq   rxByte
FB2F  EBE4                     addb  ,s                               ; B = timeout msb - 1
FB31  9522                     bita  <BBIN
FB33  27AE                     beq   rxByte
FB35  E7E4                     stb   ,s                               ; store decremented timeout msb
FB37  9522                     bita  <BBIN
FB39  27A8                     beq   rxByte
FB3B  25D8                     bcs   rx0030                           ; loop if timeout hasn't expired
FB3D  200D                     bra   rxExit                           ; exit due to timeout
FB3F  9622            rx0050   lda   <BBIN                            ; read bit 7 of final byte
FB41  44                       lsra
FB42  56                       rorb                                   ; byte is now complete
FB43  E75F                     stb   -1,u                             ; store received byte to memory
FB45  3A                       abx                                    ; calculate final checksum
FB46  9622                     lda   <BBIN                            ; read stop bit
FB48  8401                     anda  #$01                             ; mask out other bits
FB4A  8A02                     ora   #$02                             ; return SUCCESS if no framing error
                                                                                                                      ; Clean up, set status and return
FB4C  3261            rxExit   leas  1,s                              ; remove timeout msb from stack
FB4E  4C                       inca                                   ; A = status to be returned in C and Z
FB4F  AAE4                     ora   ,s                               ; place status information into the..
FB51  A7E4                     sta   ,s                               ; ..C and Z bits of the preserved CC
FB53  3184                     leay  ,x                               ; return checksum in Y
FB55  35D9                     puls  cc,dp,x,u,pc                     ; restore registers and return
FB57                           setdp $00
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
                      DWWrite  pshs  d,cc                             ; preserve registers
                      txByte  lda   ,x+
                               sta   $ff4a
                               leay  -1,y                     ; decrement byte counter
                               bne   txByte                   ; loop if more to send
                               puls  cc,d,pc                  ; restore registers and return
                      
                                              ELSE                                                       
FB57  340F            DWWrite  pshs  dp,d,cc                  ; preserve registers
FB59  CC04FF                   ldd   #$04ff                   ; A = loop counter, B = $ff
FB5C  1F9B                     tfr   b,dp                     ; set direct page to $FFxx
FB5E                           setdp $ff                                      
FB5E  D623                     ldb   <$ff23                   ; read PIA 1-B control register
FB60  C4F7                     andb  #$f7                     ; clear sound enable bit
FB62  D723                     stb   <$ff23                   ; disable sound output
FB64  8C                       fcb   $8c                      ; skip next instruction
FB65  D720            txByte   stb   <BBOUT                   ; send stop bit
FB67  E680                     ldb   ,x+                      ; get a byte to transmit
FB69  12                       nop                                                    
FB6A  58                       lslb                           ; left rotate the byte two positions..
FB6B  59                       rolb                           ; ..placing a zero (start bit) in bit 1
FB6C  D720            tx0020   stb   <BBOUT                   ; send bit (start bit, d1, d3, d5)
FB6E  56                       rorb                           ; move next bit into position
FB6F  1E88                     exg   a,a                                      
FB71  12                       nop                                                    
FB72  D720                     stb   <BBOUT                   ; send bit (d0, d2, d4, d6)
FB74  56                       rorb                           ; move next bit into position
FB75  33C4                     leau  ,u                                               
FB77  4A                       deca                           ; decrement loop counter
FB78  26F2                     bne   tx0020                   ; loop until 7th data bit has been sent
FB7A  D720                     stb   <BBOUT                   ; send bit 7
FB7C  CC0402                   ldd   #$0402                   ; A = loop counter, B = MARK value
FB7F  31A2                     leay  ,-y                      ; decrement byte counter
FB81  26E2                     bne   txByte                   ; loop if more to send
FB83  D720                     stb   <BBOUT                   ; leave bit banger output at MARK
FB85  358F                     puls  cc,d,dp,pc               ; restore registers and return
FB87                           setdp $00
                                              ENDIF
                      
                      ; ===============================================================================================================================
                                                                                                                      ; THIS PART WILL ONLY BE NEEDED IF WE MODIFY THE PATCHES-CODE
                                                                                                                      ; IN THIS CASE THE DOSPLUS HAS TO BE RE-PATCHED TO UPDATE LINKS/HOOKS
                                                                                                                      ; No problem as long as we just work from the init routine downwards
                                                                                                                      ; simply EXEC $reptch from $6xxx having DPlus at $4000 and resave the DOS
                                                                                                                      ; for easy of use just need to do: jmp $c005
                      ; ===============================================================================================================================
                                                                                                                      ; 1st patch. To redirect ReadAbsoluteSector and WriteAbsoluteSector to DW4 new process
FB87  338DC57B        reptch  leau    $c106,pcr                                       ; the DOS address code to be redirected here
FB8B  CC7E12                                  ldd     #$7e12                                          ; opcodes for jmp and NOP
FB8E  8EE008                                  ldx     #entry                                          ; new entry point for DOS sector READ-WRITE commands
FB91  A7C0                                    sta     ,u+                                                     ; put jmp
FB93  AFC1                                    stx     ,u++                                                    ; put the new dispatch routine
FB95  E7C4                                    stb     ,u                                                              ; put NOP
                                                                                                                      ; 2nd patch - for DSKINIT while writting to NOT directory sectors
FB97  338DC78A                                leau    $c325,pcr                                       ; point to intercept
FB9B  8EE0CA                                  ldx     #inidsk                                         ; new added code
FB9E  A7C0                                    sta     ,u+                                                     ; put jmp
FBA0  AFC4                                    stx     ,u                                                              ; put the new code address
                                                                                                                      ; 3rd patch to fill the three buffers for DSKINIT ($800-$900-$a00) writting to directory sectors
FBA2  338DCE4D                                leau    $c9f3,pcr                                       ; point to intercept
FBA6  8EE145                                  ldx     #fildir                                         ; new added code
FBA9  A7C0                                    sta     ,u+                                                     ; put jmp
FBAB  AFC4                                    stx     ,u                                                              ; put the new code address
                                                                                                                      ; 4th patch to correct track number when verifying tracks in DSKINIT to VDK files
FBAD  338DD5F3                                leau    $d1a4,pcr                                       ; point to intercept
FBB1  8EE1EE                                  ldx     #modtrk                                         ; new added code
FBB4  A7C0                                    sta     ,u+                                                     ; put jmp
FBB6  AFC4                                    stx     ,u                                                              ; put the new code address
                                                                                                                      ; 5th patch to update system table at $6a7-8-9-a for BACKUP
FBB8  338DCC7A                                leau    $c836,pcr                                       ; point to intercept
FBBC  8EE276                                  ldx     #patbck                                         ; new added code
FBBF  A7C0                                    sta     ,u+                                                     ; put jmp
FBC1  AFC4                                    stx     ,u                                                              ; put the new code address
                                                                                                                      ; 6th patch to prevent a rutine to test hdw for DW4 drives
FBC3  338DC5A9                                leau    $c170,pcr                                       ; point to intercept
FBC7  8EE29F                                  ldx     #noHdw                                          ; new added code
FBCA  A7C0                                    sta     ,u+                                                     ; put jmp
FBCC  AFC1                                    stx     ,u++                                                    ; put the new code address
FBCE  E7C4                                    stb     ,u                                                              ; put NOP
                                                                                                                      ; 7th patch for DOS reset routine
FBD0  338DCAF5                                leau    $c6c9,pcr                                       ; point to intercept
FBD4  8EE2C1                                  ldx     #dosReset                                       ; new added code
FBD7  A7C0                                    sta     ,u+                                                     ; put jmp
FBD9  AFC1                                    stx     ,u++                                                    ; put the new code address
                                                                                                                      ; 8th patch to make BOOT command RSDOS capable (Mike Miller)
FBDB  338DC56A                                leau    $c149,pcr                                       ; point to patch location
FBDF  8EE83E                                  ldx     #rsbtchk                                        ; address of new code in DPE
FBE2  A7C0                                    sta     ,u+                                                     ; patch jmp
FBE4  AFC1                                    stx     ,u++                                                    ; patch address
                      
FBE6  39                                      rts                                                                     ; return to caller
                      ; ===============================================================================================================================
                      
                                              IF drgrom > 0
                                                      if (*%256 > 0)
                                                                      rzb      256-(*%256)    ; make it ROM compatible (length multiple of 256 bytes)
                                                      endif
                                              ENDIF
                      ; -------------------------------------------------------------------------------------------------------------------------------
FBE7                  absEnd  end
