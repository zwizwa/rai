all: rai/index.html

clean:
	rm -rf rai

%/index.html: %.scrbl
	cd $(dir $<) ; scribble --htmls $(notdir $<)

