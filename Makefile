#!/usr/bin/make -f

APPFILE   := uuid.app
VERSION   := $(shell sed -n -e '/vsn/ {s/.*,\s*"\([0-9][0-9.]*\)".*/\1/' \
                            -e 'p' -e '}' src/$(APPFILE).src)

PREFIX    ?= /usr
ERL_ROOT  := $(PREFIX)/lib/erlang
LIBDIR    := /lib
DISTDIR   := uuid-$(VERSION)

BEAMFILES := $(wildcard ebin/*.beam) $(wildcard test/*.beam)

all: build

build: ebin/$(APPFILE)
	erl -make

ebin/$(APPFILE): src/$(APPFILE).src
	cp $< $@

clean:
	-rm -rf ebin/$(APPFILE) $(BEAMFILES)

dialyzer:
	dialyzer -c $(BEAMFILES)

test: build
	erlc -W +debug_info +compressed +strip -o test/ test/*.erl
	erl -noshell -pa ebin -pa test -eval "uuid_tests:test()" -eval "init:stop()"

install: build
	# create dist directory and install files
	mkdir -p $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin
	install -m0644 ebin/* $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin

uninstall:
	-rm -rf $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/uuid-[0-9][0-9.]*

.PHONY: all build clean dialyzer test install uninstall
