#include "buffer.h"

/* buffer.c - manages all buffer sending/storing/reading duties */

char *string_copy(char *str) {
	char *cpy = malloc(strlen(str) + 1);
	strcpy(cpy, str);
	return cpy;
}

// case insensitively compares the string
int strcmp_ci(char *a, char *b) {
	while (*a && *b) {
		if (tolower(*a) != tolower(*b)) {
			return 1;
		}
		a++;
		b++;
	}
	return (*a == *b)?0:1;
}

void setup_buffer(int socket, struct buffer* buf) {
	buf->socket = socket;
	buf->pos = 0;
	buf->data = 0;
}

// receives whatever is coming in from the socket (could be in different sized)
int do_recv(int socket, char *buffer, size_t size) {
	struct timeval tv;
	fd_set recvfds;

	tv.tv_sec = 10;
	tv.tv_usec = 0;

	FD_ZERO(&recvfds);
	FD_SET(socket, &recvfds);

	select(socket+1, &recvfds, NULL, NULL, &tv);

	if (FD_ISSET(socket, &recvfds)) {
		return recv(socket, buffer, size, 0);
	} else {
		return 0;
	}
}

// reads ALL contents in the buffer
int read_whole_buffer(char **out, int *length, struct buffer *buf) {
	*out = NULL;
	*length = 0;

	if (buf->data - buf->pos > 0) {
		*out = realloc(*out, buf->data - buf->pos);
		if (*out == NULL) {
			return -1;
		}
		memcpy(*out + *length, buf->buffer + buf->data, buf->data - buf->pos);
		*length = buf->data - buf->pos;
	}

	while (1) {
		size_t recvd = do_recv(buf->socket, buf->buffer, BUF_SIZE);
		if (recvd == 0) {
			return 0;
		} else if (recvd == -1) {
			perror("recv");
			return -1;
		}

		*out = realloc(*out, *length + recvd);
		if (*out == NULL) {
			return -1;
		}
		memcpy(*out + *length, buf->buffer, recvd);
		*length += recvd;
	}
}

// read buffer by line
int readline_buffer(char **out, struct buffer *buf) {
	int carr = 0, nl = 0;
	int last = 0;
	*out = NULL;
	int size = 0;
	int len = 0;
	while (1) {
		memmove(buf->buffer, buf->buffer + buf->pos, buf->data - buf->pos);
		buf->data -= buf->pos;
		buf->pos = 0;

		last = buf->pos;
		int i;
		for (i = buf->pos; i<buf->data; ++i) {
			len++;
			int curr = buf->buffer[i];
			if (curr == 10) {
				nl = 1;
			} else if (curr == 13) {
				nl = 0;
				carr = 1;
			} else {
				nl = 0;
				carr = 0;
				last = i+1;
			}
			if (carr && nl) {
				*out = realloc(*out, size + last - buf->pos + 1);
				if (*out == NULL) {
					return -1;
				}
				memcpy((char*)((size_t)*out + (size_t)size),
				       buf->buffer + buf->pos, last - buf->pos);
				(*out)[size + last - buf->pos] = 0;
				buf->pos = i+1;
				return len;
			}
		}

		*out = realloc(*out, size + i - buf->pos);
		if (*out == NULL) {
			return -1;
		}
		memcpy((char*)((size_t)*out + (size_t)size),
		       buf->buffer + buf->pos, i - buf->pos);
		size += i - buf->pos;

		buf->pos = i;

		if (buf->data < 1024) {
			size_t recvd = do_recv(buf->socket, buf->buffer + buf->data,
					       BUF_SIZE - buf->data);
			if (recvd == -1) {
				perror("recv");
				if (*out != NULL) {
					free(*out);
				}
				return -1;
			}
			if (recvd == 0) {
				if (*out != NULL) {
					free(*out);
				}
				return -1;
			}
			buf->data += recvd;
		}
	}
}


// reads the buffer by defined size
int read_buffer(char *out, int size, struct buffer *buf) {
	if (buf->pos > 0) {
		memmove(buf->buffer, buf->buffer + buf->pos, buf->data - buf->pos);
		buf->data -= buf->pos;
		buf->pos = 0;
	}

	while (buf->data - buf->pos == 0) {
		size_t recvd = do_recv(buf->socket, buf->buffer + buf->data,
				       BUF_SIZE - buf->data);
		if (recvd == 0) {
			return -1;
		} else if (recvd == -1) {
			perror("recv");
			return -1;
		}
		buf->data += recvd;
	}

	if (buf->data - buf->pos >= size) {
		memcpy(out, buf->buffer + buf->pos, size);
		buf->pos += size;
		return 0;
	} else {
		size_t write_size = buf->data - buf->pos;
		memcpy(out, buf->buffer + buf->pos, write_size);
		buf->pos += write_size;
		return read_buffer(out + write_size, size - write_size, buf);
	}
}

// sends everything in the buffer
int send_all(int sock, char *buf, int length) {
	while (length > 0) {
		int sent = send(sock, buf, length, MSG_NOSIGNAL);
		if (sent < 0) {
			return -1;
		}
		buf += sent;
		length -= sent;
	}
	return 0;
}




