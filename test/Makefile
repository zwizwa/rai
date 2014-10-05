.DELETE_ON_ERROR:


#	ai-array-scheme.rkt.run \
#	compalg.rkt.run \

ALL_RUN := \
	test-array-scheme.rkt.run \
	test-delay.rkt.run \
	test-map.rkt.run \
	test-type.rkt.run \
	test-ai-array.rkt.run \
	test-ai-taylor.rkt.run \
	test-series.rkt.run \
	test-ai-autodiff.rkt.run \
	test-matrix-lib.rkt.run \
	test-ai-linpar.rkt.run \
	test-ai-freq.rkt.run \
	test_pd.g.run \
	test-ai.rkt.run \
	test-libproc.rkt.run \
	test-ai-stream.rkt.run \
	test-ai-proc.rkt.run \

#	test-ai-network.rkt.run \

ALL_BIN := \
	test_pd.g.h \
	test_pd.sp \
	test_pd.g.elf \
	test_jack.g.h \
	test_jack.jack \
	proc.pd_linux \
	test_pd.dll \
	synth.sp \

# ALL :=  $(ALL_BIN) $(ALL_RUN)
ALL :=  $(ALL_RUN)


RAI := ..


all: $(ALL)
	@echo "<$$(readlink -f test.png)>"

all_bin: $(ALL_BIN)

clean:
	rm -rf $(ALL) *~ *.elf *.bin *.sp *.o *.sp.o.* *.g.h *.pd_linux *.run *.jack compiled *.dll *.exe *.sp.pd *.il

test: all
	mzc -vk test-ai-scheme.rkt
	mzc -vk test-ai.rkt

include ../src/config.mk
include ../src/rules.mk


%.html: %.scrbl
	cd $(dir $<) ; scribble --htmls $(notdir $<)

zwizwa: ~/www/zwizwa.be/rai/index.html

~/www/zwizwa.be/rai/index.html: doc/rai.scrbl
	cd doc ; scribble --htmls $(notdir $<)
	cp -av doc/rai/* ~/www/zwizwa.be/rai/

test-libproc.rkt.run: test_pd.sp synth.sp libproc.so

livecode:
	$(RACKET) fam-sp.rkt synth.rkt ./pd_notify.sh



