.DELETE_ON_ERROR:

ALL := \
	ai-array-scheme.rkt.run \
	test-array-scheme.rkt.run \
	test-delay.rkt.run \
	test-map.rkt.run \
	test-type.rkt.run \
	test-ai-array.rkt.run \
	test-ai-stream.rkt.run \
	sp_host.elf \
	test-ai-taylor.rkt.run \
	test-series.rkt.run \
	test-ai-autodiff.rkt.run \
	test-matrix-lib.rkt.run \
	test-ai-linpar.rkt.run \
	test-ai-freq.rkt.run \
	test_pd.g.h test_pd.pd_linux  \
	test_pd.sp test_pd.g.elf test_pd.g.run \
	test-ai.rkt.run \
	librai.so \
	compalg.rkt.run \
	test_jack.g.h test_jack.jack \
	sp_host.pd_linux \
	test_pd.dll \
	test-librai.rkt.run \
	synth.sp

#	test-librai.rkt.run \
#	test_pd1.sp test_pd1.g.run \

RAI := .

all: $(ALL)
	@echo '</home/tom/test.png>'

clean:
	rm -rf $(ALL) *~ *.elf *.bin *.sp *.o *.sp.o.* *.g.h *.pd_linux *.run *.jack compiled *.dll *.exe

test: all
	mzc -vk test-ai-scheme.rkt
	mzc -vk test-ai.rkt

include config.mk
include rules.mk


%.html: %.scrbl
	cd $(dir $<) ; scribble --htmls $(notdir $<)

zwizwa: ~/www/zwizwa.be/rai/index.html

~/www/zwizwa.be/rai/index.html: doc/rai.scrbl
	cd doc ; scribble --htmls $(notdir $<)
	cp -av doc/rai/* ~/www/zwizwa.be/rai/

test-librai.rkt.run: test_pd.sp synth.sp librai.so

livecode:
	while sleep 1; do $(RACKET) livecode-synth.rkt; done



