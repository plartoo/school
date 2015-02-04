CC=g++
FLAGS=-g -Wall -O2
LIB=-lm -pthread
HEADERS=lib/LL.h VectorRouting.h lib/node.h lib/LLL.h
MYLIBFILES=lib/LL.cpp lib/node.cpp lib/LLL.cpp
OBJECTS=lib/linkedlist.o lib/node.o lib/linkedlinkedlist.o

all: $(HEADERS) VectorRouting.cpp node linkedlist LLlist
	$(CC) $(FLAGS) VectorRouting.cpp $(OBJECTS) -o vector $(LIB)

utest: utest.cpp utestc.cpp
	$(CC) $(FLAGS) utest.cpp -o server $(LIB)
	$(CC) $(FLAGS) utestc.cpp -o client $(LIB)

node: $(HEADERS) lib/node.cpp
	$(CC) $(FLAGS) lib/node.cpp -c -o lib/node.o

linkedlist: $(HEADERS) lib/LL.cpp
	$(CC) $(FLAGS) lib/LL.cpp -c -o lib/linkedlist.o $(LIB)
LLlist: $(HEADERS) lib/LLL.cpp 
	$(CC) $(FLAGS) lib/LLL.cpp -c -o lib/linkedlinkedlist.o $(LIB)

linkedlisttest: $(HEADERS) lltest.cpp node linkedlist LLlist
	$(CC) $(FLAGS) lltest.cpp $(OBJECTS) -o lltest $(LIB)

dvectest: distancevec.cpp linkedlist LLlist node 
	$(CC) $(FLAGS) $(OBJECTS) distancevec.cpp -o distancevect $(LIB)

clean:
	rm vector $(OBJECTS)
