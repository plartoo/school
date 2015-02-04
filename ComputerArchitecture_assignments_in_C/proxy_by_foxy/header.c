#include "header.h"

int send_header(int socket, struct header *header) {
	int size = strlen(header->type) + strlen(header->value) + 4;
	char buffer[size];

	sprintf(buffer, "%s: %s\r\n", header->type, header->value);
	return send_all(socket, buffer, size);
}

