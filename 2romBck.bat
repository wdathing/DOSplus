asm6809 -9 -B -v --define becker=1 --define drgrom=1 --exec=%2 --o=%1RBCK.rom %1.asm 
asm6809 -9 -B -v --define becker=1 --exec=%2 --o=PATCHER.rom %1.asm 
asm6809 -9 -B -v --define becker=1 --define drgbin=1 --l=%1DBCK-PATCHER.lst.asm --o=%1DBCK-PATCHER.bin %1.asm
copy DP50NEW.rom /B + %1RBCK.rom /B  DP50PSR-BCK.rom /B
asm6809 -9 -B -v 2dweeb.asm --o=DP50BINS
del PATCHER.rom
del %1RBCK.rom
pause
