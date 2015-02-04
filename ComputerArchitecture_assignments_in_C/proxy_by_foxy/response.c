#include <pthread.h>
#include <netdb.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>

#include "response.h"

// Actual GET is done here
int make_server_connection(struct request *req) {
	char *host = NULL;
	struct header *h = req->headers;
	while (h) {
		if (strcmp_ci(h->type, "host") == 0) {
			host = h->value;
			break;
		}
		h = h->next;
	}

	if (host == NULL) {
		return -1;
	}

	char host_line[25];
	snprintf(host_line, 25, "%s", host);
	char status_line[80];
	snprintf(status_line, 79, "Host: %-25s Page: %s", host_line, req->page);
	puts(status_line);

	int status;
	struct addrinfo hints;
	struct addrinfo *res;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	
	if ((status = getaddrinfo(host, "80", &hints, &res)) != 0) {
		fprintf(stderr, "Error: getaddrinfo: %s\n", gai_strerror(status));
		return -2;
	}

	int server_socket = socket(res->ai_family,
	                           res->ai_socktype,
	                           res->ai_protocol);
	if (server_socket == -1) {
		perror("Error: socket");
		freeaddrinfo(res);
		return -2;
	}

	if (connect(server_socket, res->ai_addr, res->ai_addrlen) == -1) {
		perror("Error: connect");
		freeaddrinfo(res);
		return -2;
	}

	freeaddrinfo(res);

	return server_socket;
}

// wait for the actual web server's response
int read_response(int sock, struct response* resp) {
	struct buffer buf;

	setup_buffer(sock, &buf);

	char *line;
	if (readline_buffer(&line, &buf) < 0) {
		return -1;
	}
	
	char *hold, *read;
	if ((read = strtok_r(line, " \t", &hold)) == NULL) {
		free(line);
		return -1;
	}
	resp->version = string_copy(read);

	if ((read = strtok_r(NULL, "", &hold)) == NULL) {
		free(line);
		return -1;
	}
	resp->response = string_copy(read);

	free(line);

	struct header *lastheader;	// header information stored here
	lastheader = resp->headers = NULL;

	int chunked = 0;
	int length = 0;

	if (readline_buffer(&line, &buf) < 0) {
		return -1;
	}
	while (strlen(line) > 0) {
		struct header *new_header = malloc(sizeof(*new_header));

		if (new_header == NULL) {
			return -1;
		}

		new_header->type = NULL;
		new_header->value = NULL;

		new_header->next = NULL;
		if(lastheader == NULL){
			lastheader = resp->headers = new_header;
		} else {
			lastheader->next = new_header;
			lastheader = new_header;
		}

		if ((read = strtok_r(line, ": \t", &hold)) == NULL) {
			free(line);
			return -1;
		}
		new_header->type = string_copy(read);

		if ((read = strtok_r(NULL, "", &hold)) == NULL) {
			free(line);
			return -1;
		}
		new_header->value = string_copy(read+1);

		if (strcmp_ci(new_header->type, "content-length") == 0) {
			length = atoi(new_header->value);
		} else if (strcmp_ci(new_header->type, "transfer-encoding") == 0) {
			if (strcmp_ci(new_header->value, "chunked") == 0) {
				chunked = 1;
			}
		} else if (strcmp_ci(new_header->type, "server") == 0) {
			free(new_header->value);
			new_header->value = string_copy("ProxyServer");
		}

		free(line);
		if (readline_buffer(&line, &buf) < 0) {
			return -1;
		}
	}
	free(line);

	if (!chunked && length > 0) {
		char *data = malloc(length);
		if (data == NULL) {
			return -1;
		}
		if (read_buffer(data, length, &buf) < 0) {
			free(data);
			return -1;
		}
		resp->length = length;
		resp->data = data;
		return 0;
	} else if (chunked) {
		int chunk_length;
		char *data=NULL;
		int curr_length=0;
		do {
			int len = readline_buffer(&line, &buf);
			if (len < 0) {
				if (data) {
					free(data);
				}
				return -1;
			}
			chunk_length = strtoul(line, 0, 16);
			data = realloc(data, curr_length + len);
			memcpy(data+curr_length,line,len-2);
			curr_length += len;
			
			data[curr_length-2]='\r';
			data[curr_length-1]='\n';
			free(line);

			data = realloc(data, curr_length + chunk_length + 2);
			if (read_buffer(data + curr_length, chunk_length + 2, &buf) < 0) {
				free(data);
				return -1;
			}
			curr_length += chunk_length + 2;
		} while(chunk_length != 0);
		resp->data = data;
		resp->length = curr_length;
		return chunk_length;
	} else {
		int read_length;
		char *data;
		if (read_whole_buffer(&data, &read_length, &buf) < 0) {
			return -1;
		}
		resp->data = data;
		resp->length = read_length;
		return 0;
	}

	return 0;
}

// relay the response to user of our proxy server
int send_response(int sock, struct response *resp) {
	int length = strlen(resp->version) + strlen(resp->response) + 4;
	char buffer[length];

	sprintf(buffer, "%s %s\r\n", resp->version, resp->response);
	if (send_all(sock, buffer, strlen(buffer)) < 0) {
		return -1;
	}

	struct header *h = resp->headers;
	while (h) {
		if (send_header(sock, h) < 0) {
			return -1;
		}
		h = h->next;
	}

	if (send_all(sock, "\r\n", 2) < 0) {
		return -1;
	}
	
	if (send_all(sock, resp->data, resp->length) < 0) {
		return -1;
	}

	return 0;
}

// free up everything we used
void free_response(struct response *resp) {
	if (resp->version) {
		free(resp->version);
	}
	if (resp->response) {
		free(resp->response);
	}
	if (resp->data) {
		free(resp->data);
	}	

	struct header *h = resp->headers, *hold;
	while (h) {
		if (h->type) {
			free(h->type);
		}
		if (h->value) {
			free(h->value);
		}
		hold = h->next;
		free(h);
		h = hold;
	}
}

// Miscellaneous error handling methods below
void send_bad_method_response(int socket, char *version) {
	char resp[128];

	sprintf(resp, "%s 405 Method Not Allowed\r\n", version);
	send(socket, resp, strlen(resp), 0);

	sprintf(resp, "Allow: GET\r\n");
	send_all(socket, resp, strlen(resp));
}

void send_bad_request_response(int socket, char *version) {
	char resp[128];

	sprintf(resp, "%s 400 Bad Request\r\n", version);
	send_all(socket, resp, strlen(resp));
}

void send_bad_gateway_response(int socket, char *version) {
	char resp[128];

	sprintf(resp, "%s 502 Bad Gateway\r\n", version);
	send_all(socket, resp, strlen(resp));
}

