#ifndef _plusproxy
#define _plusproxy

#define TRUE 1
#define FALSE 0

typedef struct {
	int socketfd;
	char* url;
} thread_args;
class MySocket;

#endif
