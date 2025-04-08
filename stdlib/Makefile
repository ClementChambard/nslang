all: bin/stdlib.a bin/runtime.a

bin/stdlib.a: bin/stdlib.o bin/mem.o bin/io.o bin/file.o bin/net.o bin/str.o
	ar -crs $@ $^

bin/runtime.a:
	../nslang -c -o bin/runtime.o runtime/start.ns
	ar -crs bin/runtime.a bin/runtime.o

bin/%.o: src/%.ns
	../nslang -c -o $@ -nostdlib -Iinclude $^

clean:
	rm bin/*
