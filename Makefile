MZSCHEME=mzscheme
RACO=raco

all:
	make -C src
	make -C test
remove:
	$(RACO) pkg remove rai || echo not installed
github: remove
	$(RACO) pkg install github://github.com/zwizwa/rai/master
link: remove
	cd $$(readlink -f .)/.. ; $(RACO) pkg install --link rai

