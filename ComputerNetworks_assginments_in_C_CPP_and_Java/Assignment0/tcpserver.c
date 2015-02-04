/*
Phyo Thiha
CSC 457 - Assignment 1
Last Modified: Jan 21, 2013
Description: TCP server program to measure the round-trip time
of the message sent to a specified IP with port number. When we run
this program, it'll listen on a desginated port for an incoming connection.
We use "tcpclient.c" to connect to this server to measure the round-trip
transmission delays between two network nodes.

To compile: $make
Usage: $./tcpserver <port>
Example: 
$ ./tcpserver 8000
Waiting...
Connected from 128.151.67.113
aaaaaaaaaaaaaaaaaaaaaaaaaaaaa
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

// prototypes
void process_string(char *str);

int main(int argc, char **argv) {
	int client_socket, server_socket;
	struct addrinfo myself;
	struct sockaddr_in client_address;
	socklen_t client_address_size;
	struct sockaddr_in server_address;
	char buffer[MAXLENGTH];
	unsigned short port = (unsigned short)atoi(argv[1]);

	struct addrinfo *res;
	client_address_size = sizeof(client_address);

	bzero(&myself, sizeof(myself));
	myself.ai_family = AF_INET;  // IPv4
	myself.ai_socktype = SOCK_STREAM; // TCP connection

	// make socket
	server_socket = socket(myself.ai_family, myself.ai_socktype, 0);

	bzero(&server_address, sizeof(client_address));
	
	server_address.sin_port = htons(port);
	server_address.sin_family = AF_INET;
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);

	bind(server_socket, (struct sockaddr *) &server_address, sizeof(server_address));

	while(TRUE) {
		int status;
		listen(server_socket, SOMAXCONN);

		printf("Waiting...\n");
		
		client_socket = accept(server_socket, (struct sockaddr *) &client_address, &client_address_size);
		printf("Connected from %s\n", inet_ntoa(client_address.sin_addr));

		status = recv(client_socket, buffer, MAXLENGTH, 0);
		process_string(buffer);
		send(client_socket, buffer, MAXLENGTH, 0);

		printf("%s\n", buffer);
		close(client_socket);
	}
	shutdown(server_socket, SHUT_RDWR);
	close(server_socket);

}

/* change the string from Upper Case to Lower Case and change Space char to '.'
 */
void process_string(char *str) {
	char *p;
	
	for(p = str; *p ; p++) {
		if( (*p) >= 65 && (*p) <= 90 ) {
			*p = (*p) + 32;
		}
		if( (*p) == 32 )
			*p = '.';
	}
}


