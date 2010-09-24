#!/usr/bin/make -f

ERL  ?= erl
ERLC ?= erlc
EFLAGS ?= -W +debug_info +compressed +strip
SRC = uuid.erl
BINDIR ?= ../ebin


all: build

build: $(SRC)
	$(ERLC) $(EFLAGS) -o $(BINDIR) $(SRC)

.PHONY: all
