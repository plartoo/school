OBJ=proxy.o client.o request.o buffer.o response.o header.o
HEAD=$(wildcard *.h)

CFLAGS=-pg -Wall -O2
LFLAGS=-lpthread

ProxyServer: $(OBJ)
	gcc -o $@ $^ $(LFLAGS)

%.o: %.c $(HEAD)
	gcc $(CFLAGS) -c -o $@ $<

test: ProxyServer
	./ProxyServer 2345

clean:
	rm -f *.o ProxyServer
