#!/usr/bin/make -f

VERSION   := 0.1.0

PREFIX    ?= /usr
ERL_ROOT  := $(PREFIX)/lib/erlang
LIBDIR    := /lib
DISTDIR   := uuid-$(VERSION)

BEAMFILES := $(wildcard ebin/*)

all: build

build:
	erl -make

clean:
	-rm -rf $(BEAMFILES)

dialyzer:
	dialyzer -c $(BEAMFILES)

install: build
	# create dist directory and install files
	mkdir -p $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin
	install -m0644 $(BEAMFILES) $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin

uninstall:
	-rm -rf $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/uuid-[0-9][0-9.]*


.PHONY: all build clean dialyzer install uninstall
