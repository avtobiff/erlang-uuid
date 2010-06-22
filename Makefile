#!/usr/bin/make -f


all: build

build:
	$(MAKE) -C src

clean:
	-rm -rf ebin/*

.PHONY: all build clean
