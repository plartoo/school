CC=g++
CFLAGS=-c -Wall
LDFLAGS=
SOURCES=shell.cpp
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=shell

all: $(SOURCES) $(EXECUTABLE)
	
$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.c.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rf *o $(EXECUTABLE)

