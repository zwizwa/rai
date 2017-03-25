MZSCHEME=mzscheme
RACO=raco

.PHONY: all clean remove github link

all:
	make -C src
	make -C test

clean:
	make -C test clean
	make -C src clean
	rm -rf slv2/compiled
	rm -rf stream/lang/compiled
	rm -rf gui/compiled
	rm -rf doc/compiled
	rm -rf compiled


remove:
	$(RACO) pkg remove rai || echo not installed

github: remove
	$(RACO) pkg install github://github.com/zwizwa/rai/master

link: remove
	cd $$(readlink -f .)/.. ; $(RACO) pkg install --link rai

deps:
	$(RACO) pkg install rsound

