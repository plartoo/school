/*
CSC 457/Assignment#1
Name - Phyo Thiha
Date Modified - Jan 31, 2013
Description: Implementation of proxy server. For detail of the design,
please refer to README and comments in this file.

Files relevant:
ProxyServer.cpp - Core of the proxy server program.
ProxyServer.h - The header for the file above. This contains a struct definition.
Makefile - To compile the code

Code Location: under "/localdisk/phyo_net/Assignment1" 
at "f13.cs.rochester.edu"

To compile-
$ make

To run/test - 
1. Run the program on the test machine (one of CS dept. machines)
$ ./ProxyServer <port_number_that_is_not_used>
2. Setup browser configs to use proxy server and test away. :)
===============================================================================
*/

#include <iostream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pthread.h>
#include <errno.h>
#include <signal.h>
#include <sys/poll.h>
#include "ProxyServer.h"

#define MAXLENGTH 70000
#define HTTP_SERVER_PORT "80"
#define MAX_REQUEST_LENGTH 100000
#define BIGMAX 100000
#define BADREQUEST (char*)"HTTP/1.1 400 Bad Request\r\n\r\n"
#define BAD_METHOD (char*)"HTTP/1.1 405 Method Not Allowed\r\n\r\n"
#define BAD_METHOD_LENGTH 50
#define BAD_REQ_LENGTH 30
#define IMAGE_WAIT 2000
#define NORMAL_WAIT 5000

using namespace std;

static int parse(char*,char*,char*, char*, char*, char*);
void *serve_client( void* arg );

// This class will do the tasks responsible for/tied to a socket for our proxy
class Socket {
	int sockfd;
	struct pollfd poll_struct;
public:
	Socket(int, int);
	int recv(char* buffer, int length) ;
	int send(char* message, int length) ;
	int poll(int);
	void setPollEvent(int);
	~Socket(){
		if(sockfd != -1) {
			::close(sockfd);
		}
	}
	int getSocketFD() {
		return sockfd;
	}

	/*
	 * An FD should be a non negative, so setting this to -1
	 * will be a check for calling close multiple times
	 */
	void close() {
		if(sockfd != -1) {
			::close(sockfd);
			sockfd = -1;
		}
	}
};


Socket::Socket(int fd, int pollevent = POLLOUT) {
	using std::cout;
	this->sockfd = fd;
	this->poll_struct.fd = fd;
	this->poll_struct.events = pollevent;
}

int Socket::recv(char* buffer, int length) {
	int n = ::recv(sockfd, buffer, length, 0);
	if( n < 0 ) {
		std::cout << "Error while receiving message." << endl;
	}
	return n;
}

int Socket::send(char* message, int length) {
	if(sockfd == -1)
		return -1;
	int n = ::send(sockfd, message, length, 0);
	if( n < 0 ) {
		std::cout << "Error while sending message." << endl;
	}
	return n ;
}

// Normal poll function inside the class
int Socket::poll(int wait_time = NORMAL_WAIT) {
	return ::poll(&this->poll_struct, 1, wait_time);
}

void Socket::setPollEvent(int e){
	this->poll_struct.events = e;
}

// This class will store server socket info and do connection tasks with it
class ServerSocket {
	int sockfd;
	struct sockaddr_in client_address;
	socklen_t client_address_size;
public:
	ServerSocket(char*);
	int accept() throw(int);
	~ServerSocket() {
		if(sockfd != -1)
			::close(sockfd);
	}
	struct sockaddr_in getClientAddress() {
		return client_address;
	}
	int close() {
		int status =  ::close(sockfd);
		sockfd = -1;
		return status;
	}
};

// This sets up all the things needed for the proxy server to work
ServerSocket::ServerSocket(char* arg_port) {
	struct addrinfo hint;
	struct sockaddr_in server_address;
	unsigned short port = (unsigned short)atoi(arg_port);

	socklen_t client_address_size;
	client_address_size = sizeof(client_address);

	::bzero(&hint, sizeof(hint));

	// don't really need struct hint unless you want to know about the address of yourself
	hint.ai_family = AF_INET;  // IPv4
	hint.ai_socktype = SOCK_STREAM; // TCP connection
	sockfd = socket(hint.ai_family, hint.ai_socktype, 0);

	bzero(&server_address, sizeof(server_address));

	server_address.sin_port = htons(port);
	server_address.sin_family = AF_INET;
	server_address.sin_addr.s_addr = htonl(INADDR_ANY);

	if(bind(this->sockfd, (struct sockaddr *) &server_address, sizeof(server_address)) < 0) {
		perror("bind");
		exit(1);
	}

	listen(this->sockfd, 4);
}

/*
 * This function is a wrapper function for accept.
 * This will throw an int -1 when there is an error while accepting
 */
int ServerSocket::accept() throw(int) {
	int client_socket = ::accept(sockfd, (struct sockaddr *) &client_address, &client_address_size);
	if(client_socket < 0) {
		printf("Failed to accept client.\n");
		throw 1;
	}
	return client_socket;
}

/*
 * The function that the thread will use.
 * This receives the message from the browser,
 * parse the message, then send the actual request to the
 * web server.
 * The response from the server will be directly sent back to the browser.
 *
 * Since the image loading takes quite slow, a poll function is used to 
 * define a "timeout".
 * The value is defined above under define WAIT
 * The poll is patient enought to wait WAIT number of milliseconds.
 */
void *serve_client( void* arg ) {
	struct addrinfo hint;
	struct addrinfo *res;
	thread_args* t_arg = (thread_args *) arg;
	int client_socket = t_arg->socketfd;
	char client_request[MAXLENGTH];
	char sendbuffer[MAXLENGTH];
	char receivebuffer[MAXLENGTH];
	char request[MAX_REQUEST_LENGTH];
	char domain[MAX_REQUEST_LENGTH];
	char file_on_server[MAX_REQUEST_LENGTH];
	char type[MAX_REQUEST_LENGTH];
	char rest[BIGMAX];
	char *p;
	int n;

	Socket webbrowser(client_socket, POLLOUT);
	try {
		n = webbrowser.recv(client_request, MAXLENGTH);
	} catch(int e) {
		cout << "There was an error -1" << endl;
		webbrowser.close();
		cout << "thread exiting due to an error receiving data from browser" << endl;
		pthread_exit(NULL);
	}
	// adding this \0 will help when we do string parsing later
	client_request[n] = '\0';

	// This checks whether there is a meaningful message
	for(int i = 0 ; client_request[i] != ' ' ; i++) {
		if(client_request[i] == '\0') {
			// when user composed an invalid request
			webbrowser.send(BADREQUEST, BAD_REQ_LENGTH);
			webbrowser.close();
			cout << "thread exiting due to no request" << endl;
			pthread_exit(NULL);
		}
	}

	int status = parse(client_request, request, domain, file_on_server, type, rest);
	if(status == 0) {
		// when the user asks for 'bad' method (aside from GET)
		webbrowser.send(BAD_METHOD, BAD_METHOD_LENGTH);
		webbrowser.close();
		cout << "thread exiting possibly not GET\n" ;
		pthread_exit(NULL);
	}

	// connect to the server
	bzero(&hint, sizeof(hint));
	hint.ai_family = AF_INET;  // IPv4 protocol 
	hint.ai_socktype = SOCK_STREAM; // TCP connection

	if (getaddrinfo(domain, HTTP_SERVER_PORT, &hint, &res) != 0) {
		webbrowser.close();
		cout << "thread exiting couldn't resolve address.\n" ;
		pthread_exit(NULL);
	}

	int target_server_socket = socket(AF_INET, SOCK_STREAM, 0);
	if(target_server_socket < 0) {
		send(client_socket, BADREQUEST, BAD_REQ_LENGTH, 0);
		webbrowser.close();
		cout << "error while making webserver socket\n";
		pthread_exit(NULL);
	}

	Socket webserver(target_server_socket, POLLIN);
	if( connect(target_server_socket, res->ai_addr , res->ai_addrlen) ) {
		send(client_socket, BADREQUEST, BAD_REQ_LENGTH, 0);
		webbrowser.close();
		webserver.close();
		cout << "connect error\n";
		pthread_exit(NULL);
	}

	// Copying what we need to send to the server
	p = sendbuffer;
	strcpy(p, request);
	p += strlen(request);
	*p = ' ';
	p++;

	strcpy(p, file_on_server);
	p += strlen(file_on_server);
	*p = ' ';
	p++;

	strcpy(p, type);
	p += strlen(type);
	*p = '\n';
	p++;

	strcpy(p, rest);
	p += strlen(rest);
	
	cout << "SEND THIS REQUEST to the server: " << sendbuffer << endl;

	if (webserver.send(sendbuffer, strlen(sendbuffer)) < 0) {
		cout << "write error" << endl;
		pthread_exit(NULL);
	}

	/* 
	 * Determining the wait time for the poll function.
	 * It seems like images take fewer time to respond.
	 * NORMAL page load wait time -> 5000;IMAGE page load wait time -> 2000
	 */
	string s(rest);
	int waittime = s.find("Accept: image") == string::npos ? NORMAL_WAIT : IMAGE_WAIT;

	// keep polling from the server until we read zero bytes
	while(true) {
		int n;

		int rv = webserver.poll(waittime);

		if (rv == -1) {
			perror("poll");
		}
		else if (rv == 0){
				cout << "!!!!!!!!!!webserver time out!!!!!!!!!!" << endl;
				break;
		}
		else { 
			cout << "webserver receiving data...." << endl;
			n = webserver.recv(receivebuffer, MAXLENGTH);
			if(n < 0) {
				printf("Error: %d, %s\n", errno, strerror(errno));
				break;
			}
			else if( n == 0 ) {
				cout << ":::::::::::received NO incoming data" << endl;
				break;
			}
			else {
				/* 
				 * This checks if the client socket is still able to communicate
				 * This is necessary because the browser might have quitted to
				 * listen to the message.
				 */
				cout << ":::::::::::received SOME incoming data" << endl;
				if(webbrowser.poll(waittime) > 0) {
					if(webbrowser.send(receivebuffer, n) < n) 
						break;
				} // poll checking on browser
			} // end all the checks from the server
		} // poll checking on webserver
	}

	webserver.close();
	webbrowser.close();
	cout << "END: thread exiting" << endl;
	pthread_exit(NULL);
}

/*
 * Parse and allocates enough memory for each char* buffer
 */
static int parse(char* client_request, char* request, char* domain, char* file, char* httptype, char* rest) {

	int i, j, offset;
	char address[MAX_REQUEST_LENGTH];

	// e.g., "client_request" is "GET http://cs.rochester.edu/~pthiha HTTP/1.1"
	// then copy 'GET' from 'client_request'
	for(i = 0; client_request[i] != ' ' ; i++) {
		request[i] = client_request[i];
	}
	request[i] = '\0';

	// if request is GET then process; otherwise error
	if(request[0] == 'G' && request[1] == 'E' && request[2] == 'T') {
		std::cout << "Valid GET request" << endl;

		// to skip 'http://' part of the client_address
		i += 8;

		// Copy everything between "http://" and " HTTP/1.1"
		// e.g., "google.com/xxxxxx"
		j = 0;
		while(client_request[i] != ' ') { 
			address[j] =client_request[i];
			i++;
			j++;
		}
		address[j] = '\0';

		// Possible cases: 
		// "google.com/index.php" or "google.com" or "google.com/"
		// Therefore, search for existence of '/'
		string s(address);
		unsigned found = s.find("/");

		// e.g., if address is "google.com/" or "google.com/index.php"
		if (found != std::string::npos){
			// then take "google.com" as domain (NOT including '/')
			j = 0;
			offset = 0;
			while(address[j] != '/'){
				domain[offset] = address[j];
				offset++;
				j++;
			}
			domain[offset] = '\0';

			// and the rest of the address string as file
			// e.g., "/index.php" from "google.com/index.php"
			offset = 0;
			while(address[j] != '\0') {
				file[offset] = address[j];
				offset++;
				j++;
			}
			file[offset] = '\0';

		}
		else {	// e.g., if address is "google.com" then
			// take use it as domain and set '/' as file
			j = 0;
			offset = 0;
			while(address[j] != '\0'){
				domain[offset] = address[j];
				offset++;
				j++;
			}
			domain[offset] = '\0';
			
			char slash[]="/";
			strcpy(file, slash);
		}

		// skip ' ' before 'HTTP/1.1'
		i++;
		offset = 0;
		while(client_request[i] != '\n') {
			httptype[offset] = client_request[i];
			i++;
			offset++;
		}
		httptype[offset] = '\0';
		
		// skip \n
		i++;
		offset = 0;

		/* The \0 should be there because it is set in the 
		 * serve_client function, right after the message is received
		 */
		while(client_request[i] != '\0') {
			rest[offset] = client_request[i];
			i++;
			offset++;
		}

		// HTTP/1.1 needs 'host' argument
		// This is essential for testing with Telnet to work
		string t(rest);
		found = t.find("Host");
		if (found != std::string::npos){
			std::cout << "Found letter host in request, so moving on" << endl;
		}
		else{ // we'll manually add in 'host: xxxx' ourselves
			char *p;
			char h[]="Host:";
			p = rest;
			strcpy(p, h);
			p += strlen(h);
			*p = ' '; p++;	// add space character between "Host:" and host value
			strcpy(p, domain);
			p += strlen(domain);

			// yeah, this is a bit messy, but I'm getting
			// tired of parsing strings in C++
			// Finally add "Connection: close" to get 
			// rid of HTTP1.1's persistent connection
			char c[]="\nConnection: close";
			strcpy(p, c);
			p += strlen(c);
			*p = '\r';
			p++;
			*p = '\n';
			p++;
			*p = '\r';
			p++;
			*p = '\n';
		}

		std::cout << "::::::::::Request parsing results" << endl;
		std::cout << "parse/request: " << request << endl;
		std::cout << "parse/domain: " << domain << endl;
		std::cout << "parse/file: " << file << endl;
		std::cout << "parse/httptype: " << httptype << endl;
		std::cout << "parse/rest: " << rest << endl;

		std::cout << "Finished parsing" << endl;
		return 1;

	}
	else {
		std::cout << "ERROR, wrong requst type" << endl;
		return 0;
	}

}

/*
 * Main loop that runs the proxy and feed client the content
 */
int main(int argc, char** argv) {
	ServerSocket proxy_server(argv[1]);
	signal(SIGPIPE,SIG_IGN);

	while(TRUE) {
		int status;
		int client_sock;
		pthread_t th;
		thread_args ar;

		std::cout << "Waiting..." << endl;
		try {
			client_sock = proxy_server.accept();
		} catch (int e) {
			continue;
		}
		ar.socketfd = client_sock;

		status = pthread_create( &th, NULL, serve_client, (void*)&ar);
		if (status != 0) {
			std::cout << "ERROR in thread creation" << endl;
		}
		pthread_detach(th);
	}
}
