#ifndef REQUEST_H
#define REQUEST_H

#include "buffer.h"
#include "header.h"

struct request {
	char *type;
	char *page;
	char *version;
	struct header *headers;
};


int read_request(int, struct request*);
int send_request(int, struct request*);
void free_request(struct request*);

#endif
