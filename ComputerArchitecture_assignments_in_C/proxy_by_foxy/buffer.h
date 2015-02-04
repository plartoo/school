#ifndef BUFFER_H
#define BUFFER_H

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>

#define BUF_SIZE 1024

struct buffer {
	int socket;
	char buffer[BUF_SIZE];
	size_t pos;
	size_t data;
};

char *string_copy(char*);
int strcmp_ci(char*, char*);

void setup_buffer(int, struct buffer*);
int read_buffer(char*, int, struct buffer*);
int readline_buffer(char **, struct buffer*);
int read_whole_buffer(char**, int*, struct buffer*);
int send_all(int, char*, int);

#endif
