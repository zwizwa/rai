# DO NOT EDIT.
# Generated from upstream erl_tools/src/redo.erl build. Ask Tom.

./lib.host.a: \
	./packet_bridge.host.o \
	./proc.host.o \
	./tag_u32.host.o \

	@echo ./lib.host.a ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export A=./lib.host.a ; \
	export BUILD=./build.sh ; \
	export LC_ALL=C ; \
	export OBJECTS=\ ./tag_u32.host.o\ ./packet_bridge.host.o\ ./proc.host.o ; \
	export RAI_UC_TOOLS=./ ; \
	export TYPE=a ; \
	export UC_TOOLS=uc_tools/ ; \
	. ./env.sh ; $$BUILD 2>&1 #----------------------------------------------

../test/doodle.g.h: \
	../test/doodle.rkt \
	../bin/stream2c.sh \

	export GH=/i/exo/rai/test/doodle.g.h ; export RACKET=/i/exo/rai/with-env.sh\ racket ; export RKT=/i/exo/rai/test/doodle.rkt ; export STREAM2C=rai/bin/stream2c.sh ; $$STREAM2C $$RKT $$GH 2>&1  # FIXME: change rule to use bash_with_vars


./jack.host.ld: \
	./jack.host.ld.sh \

	@echo ./jack.host.ld ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export BUILD=./build.sh ; \
	export LC_ALL=C ; \
	export LD=./jack.host.ld ; \
	export LD_GEN=./jack.host.ld.sh ; \
	export RAI_UC_TOOLS=./ ; \
	export TYPE=ld ; \
	export UC_TOOLS=uc_tools/ ; \
	. ./env.sh ; $$BUILD 2>&1 #----------------------------------------------

./doodle.host.o: \
	./doodle.c \
	../src/../src/main_jack.c \
	../src/mod_jack_tag_u32.c \
	uc_tools/mod_send_tag_u32.c \
	uc_tools/linux/mod_tag_u32_stream.c \
	uc_tools/linux/assert_read.h \
	uc_tools/linux/assert_write.h \
	uc_tools/cbuf.h \
	uc_tools/gensym.h \
	uc_tools/infof.h \
	uc_tools/log.h \
	uc_tools/macros.h \
	uc_tools/ns_cbuf.h \
	uc_tools/os_linux.h \
	uc_tools/packet_tags.h \
	../src/../src/param_reader.h \
	uc_tools/pbuf.h \
	../src/../src/prim.h \
	../src/../src/proc.h \
	uc_tools/slip.h \
	uc_tools/tag_u32.h \
	uc_tools/uc_tools_config.h \
	uc_tools/uct_byteswap.h \
	../test/doodle.g.h \

	@echo ./doodle.host.o ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export ARCH=host ; \
	export BUILD=./build.sh ; \
	export C=./doodle.c ; \
	export CFLAGS=\ -I../test/\ -I../src/\ -Iuc_tools/\ -Iuc_tools/gdb/\ -Iuc_tools/linux/ ; \
	export D=./doodle.host.d ; \
	export FIRMWARE=doodle ; \
	export LC_ALL=C ; \
	export O=./doodle.host.o ; \
	export RAI_UC_TOOLS=./ ; \
	export TYPE=o ; \
	export UC_TOOLS=uc_tools/ ; \
	. ./env.sh ; $$BUILD 2>&1 #----------------------------------------------

./packet_bridge.host.o: \
	uc_tools/linux/packet_bridge.c \
	uc_tools/linux/assert_execvp.h \
	uc_tools/linux/assert_write.h \
	uc_tools/gensym.h \
	uc_tools/macros.h \
	uc_tools/os_linux.h \
	uc_tools/linux/packet_bridge.h \
	uc_tools/linux/raw_serial.h \
	uc_tools/linux/tcp_tools.h \
	uc_tools/uc_tools_config.h \

	@echo ./packet_bridge.host.o ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export ARCH=host ; \
	export BUILD=./build.sh ; \
	export C=uc_tools/linux/packet_bridge.c ; \
	export CFLAGS=\ -I../test/\ -I../src/\ -Iuc_tools/\ -Iuc_tools/gdb/\ -Iuc_tools/linux/ ; \
	export D=./packet_bridge.host.d ; \
	export FIRMWARE=packet_bridge ; \
	export LC_ALL=C ; \
	export O=./packet_bridge.host.o ; \
	export RAI_UC_TOOLS=./ ; \
	export TYPE=o ; \
	export UC_TOOLS=uc_tools/ ; \
	. ./env.sh ; $$BUILD 2>&1 #----------------------------------------------

./proc.host.o: \
	../src/proc.c \
	../src/proc.h \

	@echo ./proc.host.o ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export ARCH=host ; \
	export BUILD=./build.sh ; \
	export C=../src/proc.c ; \
	export CFLAGS=\ -I../test/\ -I../src/\ -Iuc_tools/\ -Iuc_tools/gdb/\ -Iuc_tools/linux/ ; \
	export D=./proc.host.d ; \
	export FIRMWARE=proc ; \
	export LC_ALL=C ; \
	export O=./proc.host.o ; \
	export RAI_UC_TOOLS=./ ; \
	export TYPE=o ; \
	export UC_TOOLS=uc_tools/ ; \
	. ./env.sh ; $$BUILD 2>&1 #----------------------------------------------

./tag_u32.host.o: \
	uc_tools/tag_u32.c \
	uc_tools/gensym.h \
	uc_tools/log.h \
	uc_tools/macros.h \
	uc_tools/os_linux.h \
	uc_tools/packet_tags.h \
	uc_tools/tag_u32.h \
	uc_tools/uc_tools_config.h \
	uc_tools/uct_byteswap.h \

	@echo ./tag_u32.host.o ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export ARCH=host ; \
	export BUILD=./build.sh ; \
	export C=uc_tools/tag_u32.c ; \
	export CFLAGS=\ -I../test/\ -I../src/\ -Iuc_tools/\ -Iuc_tools/gdb/\ -Iuc_tools/linux/ ; \
	export D=./tag_u32.host.d ; \
	export FIRMWARE=tag_u32 ; \
	export LC_ALL=C ; \
	export O=./tag_u32.host.o ; \
	export RAI_UC_TOOLS=./ ; \
	export TYPE=o ; \
	export UC_TOOLS=uc_tools/ ; \
	. ./env.sh ; $$BUILD 2>&1 #----------------------------------------------

./doodle.jack.host.elf: \
	./lib.host.a \
	./jack.host.ld \
	./doodle.host.o \

	@echo ./doodle.jack.host.elf ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export A=./lib.host.a ; \
	export ARCH=host ; \
	export BUILD=./build.sh ; \
	export ELF=./doodle.jack.host.elf ; \
	export LC_ALL=C ; \
	export LD=./jack.host.ld ; \
	export LDLIBS=-ljack\ -lm ; \
	export MAP=./doodle.host.map ; \
	export O=./doodle.host.o ; \
	export RAI_UC_TOOLS=./ ; \
	export TYPE=elf ; \
	export UC_TOOLS=uc_tools/ ; \
	. ./env.sh ; $$BUILD 2>&1 #----------------------------------------------

.PHONY: gen_clean
gen_clean:
	rm -f \
		./lib.host.a \
		../test/doodle.g.h \
		./jack.host.ld \
		./doodle.host.o \
		./packet_bridge.host.o \
		./proc.host.o \
		./tag_u32.host.o \
		./doodle.jack.host.elf
