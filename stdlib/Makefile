all: bin/stdlib.a bin/runtime.a

bin/stdlib.a: bin/io.o bin/mem.o bin/str.o bin/sys_gnu_linux_x86_64.O
	ar -crs $@ $^

bin/runtime.a: runtime/start.S
	nasm -f elf64 -o bin/runtime.o runtime/start.S
	ar -crs bin/runtime.a bin/runtime.o

bin/%.o: src/%.ns
	../nslang $^ -c -o $@

bin/%.O: src/%.S
	nasm -f elf64 -o $@ $^

clean:
	rm bin/*
