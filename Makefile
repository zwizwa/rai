
ENV_SH := $(shell readlink -f ./env.sh)

# FIXME: for non-standard dependencies, always take them from the environment.
# include src/config.mk

RACO   := ". $(ENV_SH) ; raco"
RACKET := ". $(ENV_SH) ; racket"

.PHONY: all clean remove github link

all: $(ENV_SH)
	make -C src
	make -C test
	@cat README.md | grep 'white rabbit'

$(ENV_SH):
	./install-env.sh

clean:
	make -C test clean RACO="$(RACO)"
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

