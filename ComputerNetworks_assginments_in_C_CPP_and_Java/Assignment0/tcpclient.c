/*
Phyo Thiha
CSC 457 - Assignment 1
Last Modified: Jan 21, 2013
Description: TCP client program to measure the round-trip time
of the message sent to a specified IP with port number. We can
also customize the lenght of the data sent.

To compile: $make
Usage: $./tcpclient <ip address> <port> <messageLengthInBytes>
Example: 
$ ./tcpclient 74.125.224.96 80 1
0 tries:	It took 268250204 cycles to send and receive 1 bytes of data.
This is approximately 0.224102 seconds.

Note: Message length cannot be more than 10000 bytes.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <strings.h>
#include <netdb.h>
#include "hrtimer_x86.h"

#define TRUE 1
#define FALSE 0
#define MAXLENGTH 10000
#define TRIES     200

int main (int argc, char** argv) {
	int sockfd, message_length, i;
	double average = 0;
	double time_taken;

	char message[MAXLENGTH];
	char received_message[MAXLENGTH];

	struct addrinfo myself;
	struct addrinfo *res;

	hrtime_t start,end;

	if(argc != 4) {
		printf("usage: client.out <IP> <Port No.> <Message Length>\n");
		exit(1);
	}

	for( i = 0 ; i < MAXLENGTH - 1; i++ ) { // fill the message bucket up
		message[i] = 'a';
	}
	message[i] = '\0'; // put a null char at the end

	message_length = atoi(argv[3]);
	message[message_length-1] = '\0';


	// Try 100 (TRIES) times and find the average time that it took
	for( i = 0 ; i < TRIES ; i++) {
		printf("Attempt %d\t=>\t", i);

		bzero(&myself, sizeof(myself));
		myself.ai_family = AF_INET;		// IPv4 protocol 
		myself.ai_socktype = SOCK_STREAM;	// TCP connection

		getaddrinfo(argv[1], argv[2], &myself, &res);

		start = gethrcycle_x86();
		// socket returns -1 if error
		if( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
			printf("socket error\n");
			exit(1);
		}

		// connect to the server
		if( connect(sockfd, res->ai_addr , res->ai_addrlen) ) {
			printf("connect error\n");
			exit(1);
		}

		//start = gethrcycle_x86(); // Amal suggested to try this
		send(sockfd, message, message_length, 0);
		recv(sockfd, received_message, message_length, 0);
		//end = gethrcycle_x86();
		shutdown(sockfd, SHUT_RDWR);
		close(sockfd);

		// stop the timer
		end = gethrcycle_x86();
		printf("CPU cycles taken: %ld\t", (long int)(end - start));
		printf("for %d byte(s) of data.\n", message_length);

		time_taken = ((double)(end-start)*0.000001)/getMHZ_x86();
		printf("Approximate time taken: \t%lg seconds.\n", time_taken);

		//printf("time_taken before: %lg\n", time_taken);
		//printf("avg before: %lg\n", average);
		average += time_taken;

		if (i > 0){ // don't take average for the first try
			//printf("avgaft: %lg\n", average);
			average = average / 2.0;
			printf("Average time taken: \t%lg seconds.\n", average);
		}

	}
	return 0;
}

