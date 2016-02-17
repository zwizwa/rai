.PHONY: help
help:
	@echo "use make targets 'make <file.sch>' or 'make <file.pcb>'"


install: gaf gafrc

PACKAGES := gaf/packages
SYMBOLS := gaf/0.symbols
# Link to custom symbol library. Install that manually using:
# darcs get http://zwizwa.be/darcs/gaf
gaf:
	ln -s $(HOME)/gaf .

gafrc: Makefile gaf
	echo '(component-library "$(SYMBOLS)")' >gafrc

# PRJ := dist_dc_fb
PRJ := svf
# PRJ := test



PCB := $(PRJ).pcb
SCH := $(PRJ).sch
PDF := $(PRJ).pdf
GSCH2PCB := gsch2pcb -d $(PACKAGES)

# Edit schematic
%.gschem: %.sch 
	gschem -r ./gafrc $<

# Edit layout
%.pcb: %.sch
	$(GSCH2PCB) $<
	pcb $*.pcb

# SPICE
%.drc: %.sch
	gnetlist -g drc2 $< -o $@

SPICE_NET_EXT := spice
%.$(SPICE_NET_EXT): %.sch
	gnetlist -g spice-sdb $< -o $@



# PRINTING
%.ps: %.sch gafrc
	gschem -v -p -r ./gafrc -o $@ -s ./print.scm $<

%.pdf: %.ps
	ps2pdf $<

clean:
	rm -f *.log *~ *.gbr *.cnc *.new.pcb board.net board.cmd \
		*.pcb.bak* *.pcb- *.pdf *.ps *.net *.cmd *.spice


olimex:
	chmod +x ./olimex-gerber
	./olimex-gerber board ../olimex-zip/BOARD
	rm -f ../olimex.zip && cd ../olimex-zip && zip ../olimex.zip BOARD.* README.TXT


gerbv: olimex
	cd ../olimex-zip && gerbv BOARD.*
