#!/usr/bin/make -f

APPFILE   := uuid.app
VERSION   := $(shell sed -n -e '/vsn/ {s/.*,[[:blank:]]*"\([0-9][0-9.]*\)".*/\1/' \
                            -e 'p' -e '}' src/$(APPFILE).src)

PREFIX    ?= /usr
ERL_ROOT  := $(PREFIX)/lib/erlang
LIBDIR    := /lib
DISTDIR   := uuid-$(VERSION)

BEAMFILES := $(wildcard ebin/*.beam) $(wildcard test/*.beam)
DIALYZER_PLT := erlang-uuid.plt

all: build

build: ebin/$(APPFILE)
	erl -make

ebin/$(APPFILE): src/$(APPFILE).src
	cp $< $@

clean:
	-rm -rf dist ebin/$(APPFILE) $(BEAMFILES) $(DIALYZER_PLT)

$(DIALYZER_PLT): build
	dialyzer --add_to_plt -r ebin --output_plt $(DIALYZER_PLT)

dialyzer: $(DIALYZER_PLT)
	dialyzer --plt $(DIALYZER_PLT) ebin/uuid.beam

test: build
	erlc -W +debug_info +compressed +strip -o test/ test/*.erl
	erl -noshell -pa ebin -pa test -eval "uuid_tests:test()" -eval "init:stop()"

dist: build
	# create dist tarball
	mkdir -p dist/$(DISTDIR)/ebin
	install -m0644 ebin/* dist/$(DISTDIR)/ebin
	(cd dist ; tar zcf $(DISTDIR).tar.gz $(DISTDIR) )

install: build
	# create dist directory and install files
	mkdir -p $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin
	install -m0644 ebin/* $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin

uninstall:
	-rm -rf $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/uuid-[0-9][0-9.]*

.PHONY: all build clean dialyzer test install uninstall
