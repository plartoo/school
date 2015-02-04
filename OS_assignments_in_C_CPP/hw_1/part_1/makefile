CC=gcc
CFLAGS=-c -Wall
LDFLAGS=
SOURCES=proc_parse.c
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=proc_parse

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rf *o $(EXECUTABLE)

