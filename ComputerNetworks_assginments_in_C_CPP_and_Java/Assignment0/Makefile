# Makefile

CC = gcc
FLAGS = -Wall -g

TCPOBJS = tcpclient.o tcpserver.o tcpclient tcpserver
TCPCLIENT = hrtimer.o tcpclient.o

tcp: $(TCPCLIENT) tcpserver.o
	$(CC) $(FLAGS) -o tcpclient $(TCPCLIENT)
	$(CC) $(FLAGS) -o tcpserver tcpserver.o

tcpclient.o: tcpclient.c
	gcc $(FLAGS) tcpclient.c -c -o tcpclient.o

tcpserver.o: tcpserver.c
	gcc $(FLAGS) tcpserver.c -c -o tcpserver.o

hrtimer.o: hrtimer_x86.h hrtimer_x86.c
	gcc $(FLAGS) hrtimer_x86.c -c -o hrtimer.o

tcpclean:
	rm $(TCPOBJS)

