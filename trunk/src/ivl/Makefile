SHELL=/bin/bash

PREFIX=/usr/local

.PHONY : all
all : fnf.tgt

fnf.tgt : fnf.c ivl_target.h
	gcc -Wall -O2 -shared -o fnf.tgt fnf.c

.PHONY : install
install : all
	install -D fnf.tgt  $(PREFIX)/lib/ivl/fnf.tgt
	install -D fnf.conf $(PREFIX)/lib/ivl/fnf.conf

.PHONY : uninstall
uninstall :
	-rm $(PREFIX)/lib/ivl/fnf.tgt
	-rm $(PREFIX)/lib/ivl/fnf.conf

clean:
	-rm fnf.tgt

