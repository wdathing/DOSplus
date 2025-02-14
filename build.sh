#!/usr/bin/env bash
asm6809 -9 -B -v --define becker=0 --define drgrom=1 --exec=$2 --o=$1RDW4.rom $1.asm 
asm6809 -9 -B -v --define becker=0 --exec=$2 --o=EXTENDER.rom $1.asm 
asm6809 -9 -B -v --define becker=0 --define drgbin=1 --l=%$DDW4-EXTENDER.lst.asm --o=$1DDW4-EXTENDER.bin $1.asm 
cat DP50NEW.ROM $1RDW4.rom >  DP50PSR-DW4.rom
#asm6809 -9 -B -v 1dweeb.asm --o=DP50XINS
rm EXTENDER.rom
rm $1RDW4.rom

