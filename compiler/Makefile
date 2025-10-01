##
# Project Title
#
# @file
# @version 0.1

SOURCES := $(wildcard src/*.ns)

bin/compiler: $(SOURCES)
	../nslang -o $@ $^

clean:
	rm bin/*

run: bin/compiler
	bin/compiler
# end
