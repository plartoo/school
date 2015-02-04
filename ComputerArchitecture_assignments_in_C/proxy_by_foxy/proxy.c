/*
*
* Name: Phyo Thiha
* Date Modified: 4/2/2012
* Description: Implementation of multithreaded webserver.
*
* For more detailed description, please refer to README.
*
*/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <pthread.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>

int done;
extern void *start_client(void*);

void sigint_handler(int blah) {
	done = 0;
}

void usage(void) {
	printf("Usage: $./ProxyServer <port>\n");
}

int main(int argc, char **argv) {
	if (argc < 2) {
		usage();
		return 0;
	}

	int status;
	struct addrinfo hints;
	struct addrinfo *res;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;
	
	if ((status = getaddrinfo(NULL, argv[1], &hints, &res)) != 0) {
		fprintf(stderr, "Error: getaddrinfo: %s\n", gai_strerror(status));
		return -1;
	}

	int listen_socket = socket(res->ai_family,
	                           res->ai_socktype,
	                           res->ai_protocol);
	if (listen_socket == -1) {
		perror("socket");
		return -1;
	}

	printf("Initialized proxy on port %s\n", argv[1]);

	int opt=1;
	if (setsockopt(listen_socket, SOL_SOCKET,
	               SO_REUSEADDR, &opt, sizeof(int)) == -1) {
		perror("Error: setsockopt");
		return -1;
	}

	if (bind(listen_socket, res->ai_addr, res->ai_addrlen) == -1) {
		perror("Error: bind");
		return -1;
	}

	if (listen(listen_socket, 20) == -1) {
		perror("Error: listen");
		return -1;
	}
	
	struct sockaddr_storage get_addr;
	socklen_t get_addr_size;
	int new_socket;

	pthread_t thread;

	signal(SIGINT, sigint_handler);

	done = 1;

	while (done) {
		get_addr_size = sizeof(get_addr);
		new_socket = accept(listen_socket,
		                    (struct sockaddr *)&get_addr,
		                    &get_addr_size);

		if (new_socket == -1) {
			perror("Error: accept");
			return -1;
		}

		pthread_create(&thread, NULL, start_client, (void*)(size_t)new_socket);
	}


	freeaddrinfo(res);
	pthread_exit(NULL);
}
