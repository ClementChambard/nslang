all: a.out tests/clientserver/client tests/clientserver/server

tests/clientserver/client: tests/clientserver/client.ns
	./nslang $^ -o $@

tests/clientserver/server: tests/clientserver/server.ns
	./nslang $^ -o $@

a.out: main.ns
	./nslang $^ -o $@

run: a.out
	./a.out

run-server: tests/clientserver/server
	tests/clientserver/server

run-client: tests/clientserver/client
	tests/clientserver/client

clean:
	rm tests/clientserver/client
	rm tests/clientserver/server
	rm a.out
