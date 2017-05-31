
include src/config.mk

.PHONY: all clean remove github link

all:
	make -C src
	make -C test
	@cat README.md | grep 'white rabbit'

clean:
	make -C test clean RACO=$(RACO)
	make -C src clean
	rm -rf `find -name compiled`

remove:
	$(RACO) pkg remove rai || echo not installed

github: remove
	$(RACO) pkg install github://github.com/zwizwa/rai/master

link: remove
	cd $$(readlink -f .)/.. ; $(RACO) pkg install --link rai

deps:
	$(RACO) pkg install rsound

