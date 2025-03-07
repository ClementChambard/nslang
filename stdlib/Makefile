
bin/stdlib.a: bin/stdlib.o bin/mem.o bin/io.o bin/file.o bin/net.o
	ar -crs $@ $^

bin/%.o: src/%.ns
	../nslang -c -o $@ -nostdlib -Iinclude $^

clean:
	rm bin/*
