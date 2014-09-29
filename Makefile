MZSCHEME=mzscheme
RACO=raco

all:
	make -C rai all
link:
	cd $$(readlink -f .)/.. ; $(RACO) pkg install --link rai
gitbuf:
	$(RACO) pkg install github://github.com/zwizwa/rai/master
remove:
	 $(RACO) pkg remove rai

