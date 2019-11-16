
WITH_ENV := $(shell readlink -f ./with-env.sh)

# FIXME: for non-standard dependencies, always take them from the environment.
# include src/config.mk

.PHONY: all clean remove github link

all: $(WITH_ENV)
# FIXME: Remove these hardcoded paths.  If this breaks, run ./install-env.sh
	[ -d ../.rai.deps/.racket/6.8/pkgs/rsound ]
	[ -d ../.rai.deps/.racket/6.8/pkgs/portaudio ]
	make -C src
	make -C test
	@cat README.md | grep 'white rabbit'

$(WITH_ENV):
	./install-env.sh

clean:
	make -C test clean
	make -C src clean
	rm -rf `find -name compiled`

# remove:
# 	$(RACO) pkg remove rai || echo not installed

# github: remove
# 	$(RACO) pkg install github://github.com/zwizwa/rai/master


# link: remove
# 	cd $$(readlink -f .)/.. ; $(RACO) pkg install --link rai

# deps:
# 	$(RACO) pkg install rsound

