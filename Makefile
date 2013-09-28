MZSCHEME=mzscheme
RACO=raco

all:
	make -C rai all
link:
	cd $$(readlink -f .)/.. ; $(RACO) pkg install --link rai

remove:
	 $(RACO) pkg remove rai

