#ifndef RESPONSE_H
#define RESPONSE_H

#include "request.h"
#include "buffer.h"

struct response {
	char *version;
	char *response;
	struct header *headers;
	int length;
	char *data;
};

int make_server_connection(struct request*);

void send_bad_method_response(int, char*);
void send_bad_request_response(int, char*);
void send_bad_gateway_response(int, char*);

int read_response(int, struct response*);
int send_response(int, struct response*);
void free_response(struct response*);

#endif
