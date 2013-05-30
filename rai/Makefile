.DELETE_ON_ERROR:

ALL := \
	test-delay.rkt.run \
	test-map.rkt.run \
	test-type.rkt.run \
	test-ai-array.rkt.run \
	test-ai-stream.rkt.run \
	main_sp_host.elf \
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
	sp_host.pd_linux

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

PLANET = $(RACO) planet
PACKAGE = rai.plt
PACKAGE_ROOT = .
include Makefile.planet


%.html: %.scrbl
	cd $(dir $<) ; scribble --html $(notdir $<)

zwizwa: ~/www/zwizwa.be/rai/rai.html

~/www/zwizwa.be/rai/rai.html: doc/rai.html
	cp -av $(dir $<)/*.png $(dir $@)
	cp -a $< $@


