RKT = $(shell ls $(RAI)/*.rkt *.rkt) 

# RACKET := /usr/local/racket-5.3.4.7/bin/racket
# RACO := /usr/local/racket-5.3.4.7/bin/raco

RACKET := racket
RACO := raco

LIBPROC_O := proc.o proc_sp.o

CFLAGS_OPTI  := -ffast-math -O3
CFLAGS_BASE  := -g -Wall -Wno-unused-variable -I. -I$(RAI) -I../copy
CFLAGS_DEBUG := -fPIC -std=gnu99 
CFLAGS_PD    := -I/usr/local/pd/src   # FIXME: g_canvas.h is usually not exported?
LDFLAGS      := -lm


CFLAGS   := $(CFLAGS_BASE) $(CFLAGS_DEBUG) $(CFLAGS_OPTI) $(CFLAGS_PD)
CXXFLAGS := $(CFLAGS_BASE) $(CFLAGS_OPTI)


CFLAGS_DLL  := $(CXXFLAGS) -I$(VST_DIR) -I$(VST_DIR)/public.sdk/source/vst2.x -mfpmath=sse -msse2
LDFLAGS_DLL := -luser32 -lgdi32 -lwsock32


# Test running racket code.
%.rkt.run: %.rkt $(RKT)
	$(RACO) make -v -j 6 $<
	$(RACKET) $< >$@
	cat $@

# Jack wrapper, standalone ELF
%.jack: %.g.h $(RAI)/prim.h $(RAI)/main_jack.c $(RAI)/proc.h $(LIBPROC_O) $(RKT)
	gcc -DPROC_FILE=\"$<\" -DCLIENT_NAME=\"$*\" $(CFLAGS) $(RAI)/main_jack.c $(LIBPROC_O) $(LDFLAGS) `pkg-config jack --cflags --libs` -o $@


# Object files containing base name of the file as a global symbol
# pointing to a struct proc_class.
%.proc.o: %.g.h $(RAI)/prim.h $(RAI)/main_sp.c
	gcc -DPROC_FILE=\"$<\" $(CFLAGS) -DPROC_BUILD_STAMP=$$(date "+%s" | tee $@.build_stamp) -DPROC_HEADER_NAME=$(notdir $*) -DPROC_NAME=\"$(notdir $*)\" -DPROC_VERSION=\"$$(date "+%Y%m%d-%H%M%S" | tee $@.version)\" -c $(RAI)/main_sp.c -o $@
	cat $@.build_stamp
	cat $@.version



# The .sp files are a dirty lowlevel hack.  Probably best to not
# depend on these for serious work, and use proper .so objects
# instead.
%.sp.elf: %.proc.o
	gcc -nostartfiles -T $(RAI)/sp.ld $< -o $@
%.sp: %.sp.elf
	objcopy --only-section=.text -O binary $< $@
%.sp.pd: %.sp
	chmod +x $(RAI)/pd_notify.sh ; $(RAI)/pd_notify.sh $< >$@


# Scheme -> C code generation.
%.g.h: %.rkt $(RKT)
	chmod +x ./stream2c.sh
	RACKET=$(RACKET) ./stream2c.sh $< $@

# Static Pd wrapper.  No reload of .sp files.

PROC_LIBRARY_O := proc_library.o synth.proc.o test_pd.proc.o

proc.pd_linux: $(RAI)/prim.h $(RAI)/main_pd.c $(RAI)/proc.h $(LIBPROC_O) $(PROC_LIBRARY_O)
	gcc -DEXTERN_SETUP=proc_setup -DEXTERN_NAME=\"proc\" -DPROC_LIBRARY=proc_library $(CFLAGS) $(RAI)/main_pd.c $(LIBPROC_O) $(PROC_LIBRARY_O) $(LDFLAGS) -rdynamic -shared -o $@


# Dynamic Pd wrapper, supports re-loading of .sp files
# See also sp_test.pd
sp_host.pd_linux: $(RAI)/prim.h $(RAI)/main_pd.c $(RAI)/proc.h $(LIBPROC_O)
	gcc -DEXTERN_SETUP=sp_host_setup -DEXTERN_NAME=\"sp_host\" $(CFLAGS) $(RAI)/main_pd.c $(LIBPROC_O) $(LDFLAGS) -rdynamic -shared -o $@

# Stand-alone test for generated C.  This is run without optimization to facilitate a GDB run.
%.g.elf: %.g.h $(RAI)/prim.h $(RAI)/main_test.c
	gcc -DPROC_FILE=\"$<\" $(CFLAGS_DEBUG) $(RAI)/main_test.c $(LDFLAGS) -I$(RAI) -o $@


# LV2 wrapper.
%.lv2: %.g.h $(RAI)/prim.h $(RAI)/main_lv2.c $(RAI)/proc.h $(LIBPROC_O)
	gcc -DPROC_FILE=\"$<\" -DPROC_NAME=\"$*\" $(CFLAGS) $(RAI)/main_lv2.c $(LIBPROC_O) $(LDFLAGS) -rdynamic -shared -o $@


%.o: %.c $(RAI)/proc.h
	gcc $(CFLAGS) $(LDFLAGS) -o $@ -c $<
%.o: $(RAI)/%.c $(RAI)/proc.h
	gcc $(CFLAGS) $(LDFLAGS) -o $@ -c $<

%.g.run: %.g.elf
	./$< .25 0 1 >$@
	cat $@

# Stand alone c program.
%.elf: %.c $(LIBPROC_O) proc.h
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $< $(LIBPROC_O)





# FIXME: This is for compiling a .dll on unix.
# This should be a separate platform
%.dll: %.g.h $(RAI)/main_vst.cpp $(LICENSE_DEPS) $(RAI)/win_debug.h $(RAI)/proc.c
	$(MINGW)gcc $(CFLAGS) -c $(RAI)/proc.c -o mingw_proc.o
	$(MINGW)g++ $(CFLAGS_DLL) $(LICENSE_CFLAGS) -DPROC_FILE=\"$<\" $(RAI)/main_vst.cpp mingw_proc.o $(LDFLAGS_DLL) -shared -o $@
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


libproc.so: $(RAI)/proc.h $(LIBPROC_O)
	gcc $(LIBPROC_O) $(LDFLAGS) -rdynamic -shared -o $@


bcr2000: $(RAI)/bcr2000.c
	gcc -o $@ $<
