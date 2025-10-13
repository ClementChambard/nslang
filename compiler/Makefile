##
# Project Title
#
# @file
# @version 0.1

SOURCES := $(wildcard src/*.ns)
OBJECTS := $(patsubst src/%.ns, bin/%.o, $(SOURCES))

bin/compiler: $(OBJECTS)
	@../nslang -o $@ $^
	@echo "--> linked: " $@

bin/%.o: src/%.ns
	@../nslang -o $@ $^ -c
	@echo "--> compiled: " $^

clean:
	rm bin/*

run: bin/compiler
	bin/compiler
# end
