.DELETE_ON_ERROR:


#	ai-array-scheme.rkt.run \
#	compalg.rkt.run \

ALL_RUN := \
	synth.rkt.run \
	test-ai-array.rkt.run \
	test-ai-autodiff.rkt.run \
	test-ai-c.rkt.run \
	test-ai-freq.rkt.run \
	test-ai-linpar.rkt.run \
	test-ai.rkt.run \
	test-ai-taylor.rkt.run \
	test-array-scheme.rkt.run \
	test-axo.rkt.run \
	test_benchmark.rkt.run \
	test-delay.rkt.run \
	test-lang.rkt.run \
	test-map.rkt.run \
	test-matrix-lib.rkt.run \
	test_pd1.rkt.run \
	test_pd.rkt.run \
	test-series.rkt.run \
	test-type.rkt.run \
	test_jack.rkt.run \

BITROT := \
	test-libproc.rkt.run \
	test-ai-network.rkt.run \
	test-ai-proc.rkt.run \
	test-ai-stream.rkt.run \
	test_pd.sp \
	synth.sp \
	test_pd.dll \

ALL_BIN := \
	test_pd.g.h \
	test_pd.g.elf \
	test_jack.g.h \
	test_jack.jack \
	proc.pd_linux \

# ALL :=  $(ALL_BIN) $(ALL_RUN)
ALL :=  $(ALL_RUN)


RAI := ..


all: $(ALL) bait
	@echo "<$$(readlink -f test.png)>"
	$(RACKET) --version

bitrot: $(BITROT)


all_bin: $(ALL_BIN)


# Reminders / attract attention in test build output.
bait: synth.il
	cat synth.il

clean:
	rm -rf $(ALL) *~ *.elf *.bin *.sp *.o *.sp.o.* *.g.h *.pd_linux *.run *.jack compiled *.dll *.exe *.sp.pd *.il *.pulse

test: all
	mzc -vk test-ai-scheme.rkt
	mzc -vk test-ai.rkt

WITH_ENV=../with-env.sh
include ../src/rules.mk

%.html: %.scrbl
	cd $(dir $<) ; scribble --htmls $(notdir $<)

zwizwa: ~/www/zwizwa.be/rai/index.html

~/www/zwizwa.be/rai/index.html: doc/rai.scrbl
	cd doc ; scribble --htmls $(notdir $<)
	cp -av doc/rai/* ~/www/zwizwa.be/rai/

test-libproc.rkt.run: test_pd.sp synth.sp libproc.so

# Live coding example for doodle.rkt
# livecode: doodle.lc
livecode: doodle.lc2



