asm6809 -9 -B -v --define becker=0 --define drgrom=1 --exec=%2 --o=%1RDW4.rom %1.asm 
asm6809 -9 -B -v --define becker=0 --exec=%2 --o=EXTENDER.rom %1.asm 
asm6809 -9 -B -v --define becker=0 --define drgbin=1 --l=%1DDW4-EXTENDER.lst.asm --o=%1DDW4-EXTENDER.bin %1.asm 
copy DP50NEW.rom /B + %1RDW4.rom /B  DP50PSR-DW4.rom /B
asm6809 -9 -B -v 1dweeb.asm --o=DP50XINS
del EXTENDER.rom
del %1RDW4.rom
pause
