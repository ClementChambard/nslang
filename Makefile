all: a.out tests/clientserver/client tests/clientserver/server tests/sdl3_test/sdl3_test

tests/clientserver/client: tests/clientserver/client.ns
	./nslang $^ -o $@

tests/clientserver/server: tests/clientserver/server.ns
	./nslang $^ -o $@

tests/sdl3_test/sdl3_test: tests/sdl3_test/sdl3_test.ns
	./nslang $^ -o $@ -lSDL3

a.out: main.ns
	./nslang $^ -o $@

run: a.out
	./a.out

run-server: tests/clientserver/server
	tests/clientserver/server

run-client: tests/clientserver/client
	tests/clientserver/client

run-sdl3: tests/sdl3_test/sdl3_test
	tests/sdl3_test/sdl3_test

clean:
	rm tests/clientserver/client
	rm tests/clientserver/server
	rm tests/sdl3_test/sdl3_test
	rm a.out
