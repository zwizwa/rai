MZSCHEME=mzscheme
RACO=raco

all:
	make -C rai all
remove:
	$(RACO) pkg remove rai
github: remove
	$(RACO) pkg install github://github.com/zwizwa/rai/master
link: remove
	cd $$(readlink -f .)/.. ; $(RACO) pkg install --link rai

