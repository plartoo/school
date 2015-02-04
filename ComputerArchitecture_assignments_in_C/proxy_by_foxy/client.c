#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "request.h"
#include "response.h"

// stop_client must be defined before start_client
// this closes the respective sockets and release the resources
void *stop_client(int client_sock, int server_sock,
                      struct request *req, struct response *resp) {
	if (client_sock > 0) {
		close(client_sock);
	}
	if (server_sock > 0) {
		close(server_sock);
	}
	free_request(req);
	free_response(resp);
	pthread_exit(NULL);
}

// spawns a pthread, initializes the socket
// makes a real web request and process the response
void *start_client(void *data) {
	int client_socket = (size_t)data;
	pthread_detach(pthread_self());

	struct request req = {0};
	struct response resp = {0};

	if (read_request(client_socket, &req) == -1) {
		fprintf(stderr, "Error: Connecting to the server failed.\n");
		send_bad_request_response(client_socket, "HTTP/1.1");
		stop_client(client_socket, 0, &req, &resp);
	}

	// besides GET, everything else is illegal
	if (strcmp(req.type, "GET") != 0) {
		fprintf(stderr, "Error: Only GET is legal at this moment. You provided: %s\n", req.type);
		send_bad_method_response(client_socket, req.version);
		stop_client(client_socket, 0, &req, &resp);
	}


	int server_socket = make_server_connection(&req);

	if (server_socket == -1) {
		fprintf(stderr, "Error: No host provided.\n");
		send_bad_request_response(client_socket, req.version);
		stop_client(client_socket, server_socket, &req, &resp);
	} else if(server_socket == -2) {
		fprintf(stderr, "Error: Connection to the server failed.\n");
		send_bad_gateway_response(client_socket, req.version);
		stop_client(client_socket, server_socket, &req, &resp);
	}

	if (send_request(server_socket, &req) == -1) {
		fprintf(stderr, "Error: Sending request failed.\n");
		send_bad_gateway_response(client_socket, req.version);
		stop_client(client_socket, server_socket, &req, &resp);
	}

	if (read_response(server_socket, &resp) == -1) {
		fprintf(stderr, "Error: Reading response failed.\n");
		send_bad_gateway_response(client_socket, req.version);
		stop_client(client_socket, server_socket, &req, &resp);
	}
	if (send_response(client_socket, &resp) == -1) {
		fprintf(stderr, "Error: Sending response failed.\n");
		stop_client(client_socket, server_socket, &req, &resp);
	}

	stop_client(client_socket, server_socket, &req, &resp);
	pthread_exit(NULL);
}



