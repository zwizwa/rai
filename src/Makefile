.DELETE_ON_ERROR:


ALL_BIN := \
	sp_host.elf \
	libproc.so \
	sp_host.pd_linux \


ALL :=  $(ALL_BIN)


RAI := ../


all: $(ALL)
	@echo "<$$(readlink -f test.png)>"

all_bin: $(ALL_BIN)

clean:
	rm -rf $(ALL) *~ *.elf *.bin *.sp *.o *.sp.o.* *.g.h *.pd_linux *.run *.jack compiled *.dll *.exe *.sp.pd *.il

test: all
	mzc -vk test-ai-scheme.rkt
	mzc -vk test-ai.rkt

include config.mk
include rules.mk


zwizwa: ~/www/zwizwa.be/rai/index.html

~/www/zwizwa.be/rai/index.html: doc/rai.scrbl
	cd doc ; scribble --htmls $(notdir $<)
	cp -av doc/rai/* ~/www/zwizwa.be/rai/

test-libproc.rkt.run: test_pd.sp synth.sp libproc.so

livecode:
	$(RACKET) fam-sp.rkt synth.rkt ./pd_notify.sh



