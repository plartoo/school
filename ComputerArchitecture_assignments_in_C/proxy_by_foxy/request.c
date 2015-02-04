#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "request.h"

// read the request from a specified socket
int read_request(int sock, struct request *req) {
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
	req->type = string_copy(read);

	if ((read = strtok_r(NULL, " \t", &hold)) == NULL) {
		free(line);
		return -1;
	}
	req->page = string_copy(read);

	if ((read = strtok_r(NULL, "", &hold)) == NULL) {
		free(line);
		return -1;
	}
	req->version = string_copy(read);

	free(line);

	req->headers = NULL;

	if (readline_buffer(&line, &buf) < 0) {
		return -1;
	}
	while (strlen(line) > 0) {
		struct header *new_header = malloc(sizeof(*new_header));
		if (new_header == NULL) {
			free(line);
			return -1;
		}
		new_header->type = NULL;
		new_header->value = NULL;

		new_header->next = req->headers;
		req->headers = new_header;

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

		free(line);
		if (readline_buffer(&line, &buf) < 0) {
			return -1;
		}
	}

	free(line);
	
	return 0;
}

// sends a request to specified socket
int send_request(int sock, struct request *req) {
	int length = strlen(req->type) + strlen(req->page) + strlen(req->version) + 5;
	char buffer[length];

	sprintf(buffer, "%s %s %s\r\n", req->type, req->page, req->version);
	if (send_all(sock, buffer, strlen(buffer)) < 0) {
		return -1;
	}

	struct header *h = req->headers;
	while (h) {
		if (send_header(sock, h) < 0) {
			return -1;
		}
		h = h->next;
	}

	if (send_all(sock, "\r\n", 2) < 0) {
		return -1;
	}

	return 0;
}

// free up header and request info
void free_request(struct request *req) {
	if (req->type) {
		free(req->type);
	}
	if (req->page) {
		free(req->page);
	}
	if (req->version) {
		free(req->version);
	}

	struct header *h = req->headers, *hold;
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
