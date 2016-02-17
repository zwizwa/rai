.DELETE_ON_ERROR:

RKT := $(wildcard *.rkt)
ALL := $(RKT:.rkt=.pulse)
RAI := ..

all: $(ALL)

clean:
	rm -rf $(ALL) *~ *.elf *.bin *.sp *.o *.sp.o.* *.g.h *.pd_linux *.run *.jack compiled *.dll *.exe *.sp.pd *.il *.pulse

include ../src/config.mk
include ../src/rules.mk





