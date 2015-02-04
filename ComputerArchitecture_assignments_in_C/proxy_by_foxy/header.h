#ifndef HEADER_H
#define HEADER_H

#include <pthread.h>
#include <netdb.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include "buffer.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>


struct header {
	char *type;
	char *value;
	struct header *next;
};

int send_header(int, struct header*);

#endif
