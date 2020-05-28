# Build rules for RAI project.

## CONFIGURATION

# Racket install is pulled from the environment, which is optionally
# set up by a (generated) launcher script.  It is the responsibility
# of the Makefile that includes us to set this up.

RACO   := $(WITH_ENV) raco
RACKET := $(WITH_ENV) racket

######################################################################

# FIXME: Clean up old hardcoded variables.  Most The VST and MINGW
# setups probably broke.

## Old approach on Debian dev machine (FIXME: remove)
# RACKET_VERSION=6.8
# RACKET_BIN=/usr/local/racket-$(RACKET_VERSION)/bin
# RACKET=$(RACKET_BIN)/racket
# RACO=$(RACKET_BIN)/raco

# The Steinberg VST SDK 2.4 can not be included in this distribution.
# Get it at: http://www.steinberg.net/en/company/developer.html

VST_DIR := $(HOME)/kmook/vst/vstsdk2.4
# VST_DIR := $(HOME)/vstsdk2.4
# VST_DIR := C:/home/tom/vstsdk2.4

# MinGW prefix for compiling Windows DLL/EXE

## Debian-hosted mingw32msvc
MINGW := i586-mingw32msvc-

## Cygwin-hosted mingw
#MINGW := i686-pc-mingw32-

## Native MinGW.  See build-mingw.bat
# set PATH=C:\MinGW\bin;%PATH%
# set PATH=C:\MinGW\msys\1.0\bin;%PATH%
# set PATH=C:\Progra~1\Racket\;%PATH%
# MINGW := mingw32-


RKT := $(wildcard $(RAI)/*.rkt) $(wildcard *.rkt) 

RAI_SRC := $(RAI)/src
RAI_BIN := $(RAI)/bin


LIBPROC_O := proc.o proc_sp.o

CFLAGS_OPTI  := -ffast-math -O3
CFLAGS_BASE  := -g -Wall -Wno-unused-variable -I. -I$(RAI_SRC) -I../copy
CFLAGS_DEBUG := -fPIC -std=gnu99 
CFLAGS_PD    := -I/usr/local/pd/src   # FIXME: g_canvas.h is usually not exported?
LDFLAGS      := -lm


CFLAGS   := $(CFLAGS_BASE) $(CFLAGS_DEBUG) $(CFLAGS_OPTI) $(CFLAGS_PD)
CXXFLAGS := $(CFLAGS_BASE) $(CFLAGS_OPTI)


CFLAGS_DLL  := $(CXXFLAGS) -I$(VST_DIR) -I$(VST_DIR)/public.sdk/source/vst2.x -mfpmath=sse -msse2
LDFLAGS_DLL := -luser32 -lgdi32 -lwsock32


# Test running racket code.
%.rkt.run: %.rkt $(RKT)
	echo WITH_ENV=$(WITH_ENV) RACO=$(RACO)
	$(RACO) make -v -j 6 $<
	$(RACKET) $< >$@
	cat $@

# apt-get install libjack-jackd2-dev
# Jack wrapper, standalone ELF
%.jack: %.g.h $(RAI_SRC)/prim.h $(RAI_SRC)/main_jack.c $(RAI_SRC)/proc.h $(LIBPROC_O) $(RKT)
	gcc -DPROC_FILE=\"$<\" -DCLIENT_NAME=\"$*\" $(CFLAGS) $(RAI_SRC)/main_jack.c $(LIBPROC_O) $(LDFLAGS) `pkg-config jack --cflags --libs` -o $@

# apt-get install libpulse-dev
# Pulseaudio wrapper, standalone ELF
%.pulse: %.g.h $(RAI_SRC)/prim.h $(RAI_SRC)/main_pulse.c $(RAI_SRC)/proc.h $(LIBPROC_O) $(RKT)
	gcc -DPROC_FILE=\"$<\" -DCLIENT_NAME=\"$*\" $(CFLAGS) $(RAI_SRC)/main_pulse.c $(LIBPROC_O) $(LDFLAGS) `pkg-config libpulse-simple --cflags --libs` -lpthread -o $@


# Object files containing base name of the file as a global symbol
# pointing to a struct proc_class.
%.proc.o: %.g.h $(RAI_SRC)/prim.h $(RAI_SRC)/main_sp.c
	gcc -DPROC_FILE=\"$<\" $(CFLAGS) -DPROC_BUILD_STAMP=$$(date "+%s") -DPROC_HEADER_NAME=$(notdir $*) -DPROC_NAME=\"$(notdir $*)\" -DPROC_VERSION=\"$$(date "+%Y%m%d-%H%M%S")\" -c $(RAI_SRC)/main_sp.c -o $@



# The .sp files are a dirty lowlevel hack.  Probably best to not
# depend on these for serious work, and use proper .so objects
# instead.
%.sp.elf: %.proc.o
	gcc -nostartfiles -T $(RAI_SRC)/sp.ld $< -o $@
%.sp: %.sp.elf
	objcopy --only-section=.text -O binary $< $@
%.sp.pd: %.sp
	chmod +x $(RAI_BIN)/pd_notify.sh ; $(RAI_BIN)/pd_notify.sh $< >$@


# Scheme -> C code generation.
%.g.h: %.rkt $(RKT)
	chmod +x $(RAI_BIN)/stream2c.sh
	RACKET="$(RACKET)" $(RAI_BIN)/stream2c.sh $< $@

# Scheme -> IL (intermediate language) code generation.
%.il: %.rkt $(RKT)
	chmod +x $(RAI_BIN)/stream2il.sh
	$(WITH_ENV) $(RAI_BIN)/stream2il.sh $< $@



# Static Pd wrapper.  No reload of .sp files.

PROC_LIBRARY_O := proc_library.o synth.proc.o test_pd.proc.o

proc.pd_linux: $(RAI_SRC)/prim.h $(RAI_SRC)/main_pd.c $(RAI_SRC)/proc.h $(LIBPROC_O) $(PROC_LIBRARY_O)
	gcc -DEXTERN_SETUP=proc_setup -DEXTERN_NAME=\"proc\" -DPROC_LIBRARY=proc_library $(CFLAGS) $(RAI_SRC)/main_pd.c $(LIBPROC_O) $(PROC_LIBRARY_O) $(LDFLAGS) -rdynamic -shared -o $@


# Dynamic Pd wrapper, supports re-loading of .sp files
# See also sp_test.pd
sp_host.pd_linux: $(RAI_SRC)/prim.h $(RAI_SRC)/main_pd.c $(RAI_SRC)/proc.h $(LIBPROC_O)
	gcc -DEXTERN_SETUP=sp_host_setup -DEXTERN_NAME=\"sp_host\" $(CFLAGS) $(RAI_SRC)/main_pd.c $(LIBPROC_O) $(LDFLAGS) -rdynamic -shared -o $@

# Stand-alone test for generated C.  This is run without optimization to facilitate a GDB run.
%.g.elf: %.g.h $(RAI_SRC)/prim.h $(RAI_SRC)/main_test.c
	gcc -DPROC_FILE=\"$<\" $(CFLAGS_DEBUG) $(RAI_SRC)/main_test.c $(LDFLAGS) -I$(RAI_SRC) -I. -o $@


# LV2 wrapper.
%.lv2: %.g.h $(RAI_SRC)/prim.h $(RAI_SRC)/main_lv2.c $(RAI_SRC)/proc.h $(LIBPROC_O)
	gcc -DPROC_FILE=\"$<\" -DPROC_NAME=\"$*\" $(CFLAGS) $(RAI_SRC)/main_lv2.c $(LIBPROC_O) $(LDFLAGS) -rdynamic -shared -o $@

# Axoloti patch
AXO_PATCH := $(shell readlink -f /home/tom/git/AxoStudio/patch)
%.xpatch: %.g.h $(RAI_SRC)/prim.h $(RAI_SRC)/main_axo.cpp
	@echo installing into $(AXO_PATCH)
	ln -fs $$(readlink -f $*.g.h) $(AXO_PATCH)/xpatch.g.h
	ln -fs $$(readlink -f $(RAI_SRC)/main_axo.cpp) $(AXO_PATCH)/xpatch.cpp
	ln -fs $$(readlink -f $(RAI_SRC)/prim.h) $(AXO_PATCH)/
	make -C $(AXO_PATCH)
	arm-none-eabi-objdump -d $(AXO_PATCH)/xpatch.elf
	../bin/axocl.py --load $(AXO_PATCH)/xpatch.bin

%.o: %.c $(RAI_SRC)/proc.h
	gcc $(CFLAGS) $(LDFLAGS) -o $@ -c $<
%.o: $(RAI_SRC)/%.c $(RAI_SRC)/proc.h
	gcc $(CFLAGS) $(LDFLAGS) -o $@ -c $<

%.g.run: %.g.elf
	./$< .25 0 1 >$@
	cat $@

# Stand alone c program.
%.elf: %.c $(LIBPROC_O) $(RAI_SRC)/proc.h
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $< $(LIBPROC_O)





# FIXME: This is for compiling a .dll on unix.
# This should be a separate platform
%.dll: %.g.h $(RAI_SRC)/main_vst.cpp $(LICENSE_DEPS) $(RAI_SRC)/win_debug.h $(RAI_SRC)/proc.c
	$(MINGW)gcc $(CFLAGS) -c $(RAI_SRC)/proc.c -o mingw_proc.o
	$(MINGW)g++ $(CFLAGS_DLL) $(LICENSE_CFLAGS) -Wno-main -Wno-narrowing -DPROC_FILE=\"$<\" $(RAI_SRC)/main_vst.cpp mingw_proc.o $(LDFLAGS_DLL) -shared -o $@
	rm mingw_proc.o
	$(MINGW)strip $@ ; ls -l $@

%.exe: %.c
	$(MINGW)g++ $< -lwsock32 -o $@

%.dll.dasm: %.dll
	$(MINGW)objdump -d $<



# DTACH := dtach -n /tmp/jack.run.sock
BCR2000 := make -C $(RAI) bcr2000 && $(RAI)/bcr2000 </dev/midi3 | 
# JACK_SECONDS := 1
%.jack.run: %.jack
	$(BCR2000) $(DTACH) $$(readlink -f $<) $(JACK_SECONDS)


libproc.so: $(RAI_SRC)/proc.h $(LIBPROC_O)
	gcc $(LIBPROC_O) $(LDFLAGS) -rdynamic -shared -o $@


bcr2000: $(RAI_SRC)/bcr2000.c
	gcc -o $@ $<


# TODO: combine both mechanisms

# Live coding: famd object reloading.
%.lc: %.rkt
	$(RACKET) $(RAI)/fam-sp.rkt $< $(RAI_BIN)/pd_notify.sh

# Live coding v2: lambda/param netsend from emacs (see emacs/rai.el)
# combined with full reload when elf changes.
%.lc2: %.pulse
	pulseaudio --check
	netcat -ulp 12345 | (while date; do $$(readlink -f $<) $*.rkt; make $*.pulse; done)
