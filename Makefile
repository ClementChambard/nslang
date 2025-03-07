all: a.out tests/clientserver/client tests/clientserver/server tests/sdl2/sdl2

tests/clientserver/client: tests/clientserver/client.ns
	./nslang $^ -o $@

tests/clientserver/server: tests/clientserver/server.ns
	./nslang $^ -o $@

tests/sdl2/sdl2: tests/sdl2/sdl2.ns
	./nslang $^ -o $@ -lSDL2

a.out: main.ns
	./nslang $^ -o $@

run: a.out
	./a.out

run-server: tests/clientserver/server
	tests/clientserver/server

run-client: tests/clientserver/client
	tests/clientserver/client

run-sdl2: tests/sdl2/sdl2
	tests/sdl2/sdl2

clean:
	rm tests/clientserver/client
	rm tests/clientserver/server
	rm tests/sdl2/sdl2
	rm a.out
