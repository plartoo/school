#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <string.h>
#include <sstream>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <math.h>
#include "VectorRouting.h"
#include "lib/node.h"
#include "lib/LL.h"
#include "lib/LLL.h"

#define BUFFER_LENGTH 10000
#define SHORT_BUFFER_LENGTH 512
#define PORT_NUMBER "48620"

using namespace std;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

/*
 * Function for the receiving thread.
 * thread_arg is defined in the header file
 */
void* receiving_thread(void* args) {
	thread_arg argument_struct = *((thread_arg*)args);
	int sock = (int)argument_struct.socketfd;
	LinkedLinkedList* distance_vector = argument_struct.distance_vector;
	LinkedList* routing_table = argument_struct.routing_table;

	char buffer[BUFFER_LENGTH];
	struct sockaddr_in from;
	socklen_t fromlen;
	fromlen = sizeof(from);

	while(true) {
		int nbytes = ::recvfrom(sock, buffer, BUFFER_LENGTH, 0,
				(struct sockaddr*)&from, &fromlen);
		if(nbytes < 0) {
			std::cout << "Error while receiving." << endl;
		}
		else {
			// cout << "Received message!!!!" << endl;
			buffer[nbytes] = '\0';
			bool updated = update_table(buffer, *distance_vector, *routing_table);
			if(updated) {
				sendTable(*distance_vector, *routing_table, routing_table->name.c_str());
			}
		}
	}
	return 0;
}

/*
 * Return true if the table has changed
 */
static bool update_table(char* str,LinkedLinkedList& distance_vector, LinkedList& routing_table) {
	int number_of_neighbors = 0;
	/* 
	 * This stores the parts of str for sscanf
	 */
	string buffer;

	/*
	 * This stores the name of each node.
	 */
	char name[BUFFER_LENGTH];

	/*
	 * Making string into stream to make sscanf easier
	 */
	stringstream stream(str);

	/*
	 * Refer to the format of the string sent in the Write up.
	 */
	getline(stream, buffer);
	sscanf(buffer.c_str(), "%s", name);

	/*
	 * Must block the data structure while editting
	 */
	pthread_mutex_lock( &mutex1 );
	Node *n = distance_vector.find(name, name);
	if(n == NULL) {
		cout << "Cannot update the table." << endl;
		cout << "There is no entry for : " << name << endl;
		cout << "This means, you just got an info from your non-neighbor" << endl;
		return -1;
	}

	/*
	 * This list is the list I want to update
	 */
	LinkedList* list = distance_vector.findList(name);

	getline(stream, buffer);
	sscanf(buffer.c_str(), "%d", &number_of_neighbors);
	for(int i = 0; i < number_of_neighbors ; i++) {
		/* buffer for keeping destination and hop */
		char dest[SHORT_BUFFER_LENGTH];
		char hop[SHORT_BUFFER_LENGTH];
		float distance = INFINITY;

		/* dest, hop, distance */
		getline(stream, buffer);
		sscanf(buffer.c_str(), "%s %s %f", dest, hop, &distance);

		/* 
		 * Steps to determine what to do.
		 * 1. if dest is you, ignore.
		 * 2. if a loop is made in the network, then set it to infinity. A loop can only be detected between 2 nodes but that is better than nothing.
		 * 3. if there is a dest name in the list, then update (4)
		 * 		else make new node for each list in distance vector
		 * 		and set them to infinity except the one you are working on (the value for this is 4).
		 * 4. Vec[Src.name, Src.dest] = Src.Vec[Src.name,Scr.name] + distance
		 */

		Node *destination = list->find(dest);
	
		// 1
		if( strcmp(dest, routing_table.name.c_str()) != 0 ) {
			// 3
			if(destination == NULL) {
				// this next line will set things to infinity automatically
				distance_vector.addToEachList(dest);
				destination = list->find(dest);
				destination->data = n->data + distance;
			}
			else {
				// 4
				if( strcmp(hop, routing_table.name.c_str()) != 0 ) {
					destination->data = n->data + distance;
				}
				// 2
				else {
					destination->data = INFINITY;
				}
			}
		}
	}

	/*
	 * Find if new table can be created. 
	 * If so, this the flag is set so that this info can be resent to the neighbors
	 */
	LinkedList new_table(routing_table.name);
	buildTable(distance_vector, new_table);
	bool b;
	if(new_table == routing_table) {
		b = false;
	}
	else {
		b = true;
	}

	pthread_mutex_unlock( &mutex1 );
	return b;
}


/*
 * This updates the distance vector, by looking at the file.
 * If the distance vector is not made yet, then this builds the vector from the
 * input file.
 * If using mutex, then it should be locked before calling this function.
 *
 * The filename should be the host name + .dat
 */
static void makeDistanceVector(string hostname, LinkedLinkedList& list) {
	string inputfilename = hostname + ".dat";
	char buffer[BUFFER_LENGTH];

	int fd = open(inputfilename.c_str(), O_RDONLY);
	int status = flock(fd, LOCK_EX);
	while(status) {
		cout << "Cannot lock the input file." << endl;
		cout << "Retrying" << endl;
		sleep(1);
		status = flock(fd, LOCK_EX);
		return ;
	}
	// the program assumes that the buffer is long enough to read in the file.
	read(fd, buffer, BUFFER_LENGTH);
	flock(fd, LOCK_UN);
	close(fd);

	stringstream stream(buffer);
	string line;
	getline(stream, line);

	bool first_time = false;
	int neighbors;
	sscanf(line.c_str(), "%d\n", &neighbors);
	for(int i = 0  ; i < neighbors ; i++) {
		getline(stream, line);
		char name[SHORT_BUFFER_LENGTH];
		float distance;
		sscanf(line.c_str(), "%s %f\n", name, &distance);

		/*
		 * The node not existing means NULL.
		 * And when the function below returns NULL, then it
		 * must be the first time to build this data structure.
		 * Therefore the 'if' below is for when this is called the first time.
		 */
		LinkedList* linkedlist = list.findList(name);
		if(linkedlist == NULL) {
			linkedlist = list.addList(name);
			list.addToEachList(name, INFINITY);
			first_time = true;
		}

		// change the value after being inserted
		Node* n = linkedlist->find(name);
		n->data = distance;
	}

	/* 
	 * add all the nodes in the first list to the other lists
	 * The first list must be guaranteed to have all the neighboring nodes.
	 */
	if(first_time) {
		Node* listnode = (list.getHead())->getHead();
		while(listnode != NULL) {
			list.addToEachList(listnode->name);
			listnode = listnode->next;
		}
	}
} 

/*
 * this assumes that the distance vector named 'list' here is
 * already built
 */
static void buildTable(LinkedLinkedList& list, LinkedList& table) {
	if( list.getHead()  == NULL) {
		return ;
	}

	int number_of_possible_destination = (list.getHead())->getsize();
	string *dests= new string[number_of_possible_destination];

	Node* n = (list.getHead())->getHead();
	for(int i = 0; n != NULL; i++) {
		dests[i] = n->name;
		n = n->next;
	}
	
	/*
	 * foreach column, find the name and get the min
	 */
	for(int i = 0 ; i < number_of_possible_destination ; i++) {
		float min = INFINITY;
		string minname = "";
		string minrowname = "";
		LinkedList* row = list.getHead();
		int size = list.getsize();
		for(int j = 0 ; j < size ; j++) {
			Node* n = row->find(dests[i]);
			if( n->data < min ) {
				min = n->data;
				minname = n->name;
				minrowname = row->name;
			}
			// go to next row
			row = row->next;
		}

		/*
		 * Note:
		 * 	The minname is the node destination.
		 * 	So minname should be equal to dests[i].
		 * 	The important part is that the next hop is the
		 * 	row's name.
		 */
		table.add(minname, dests[i], minrowname, min);
	}
	delete [] dests;
}


/*
 * The argument of this function, 'table' assumes that there is a table already made
 */
static void sendTable(LinkedLinkedList& distance_vector, LinkedList& table,const char* name_of_this_host) {
	/*
	 * 1. Make the table into a sendable format.
	 * 2. Get distance Vector and find out who I need to send to.
	 * 3. send the string over to the neighbors.
	 */
	char message[BUFFER_LENGTH];
	pthread_mutex_lock( &mutex1 );
	makeDistanceVector(table.name , distance_vector);

	// copy the name of the table and make one  this will be used for build table and to be compared with the original table
	string na = table.name;
	LinkedList newtable(na);

	buildTable(distance_vector, newtable);

	if(!(newtable == table)) {
		table = newtable;
	}

	/*
	 * Format the string into message
	 * The format is
	 * <SOURCE NAME>
	 * <NUMBER OF NODES IN TABLE>
	 * <DEST1> <NEXT HOP1> <DISTANCE1>
	 * <DEST2> <NEXT HOP2> <DISTANCE2>
	 * ...
	 */

	int size = table.getsize();
	Node* n = table.getHead();
	int position = sprintf(message, "%s\n%d\n", name_of_this_host, size);

	/* 1. Make the table into a sendable format. */
	for(int i = 0 ; i < size ; i++) {
		position += sprintf(&message[position], "%s %s %f\n", 
				n->dest.c_str(), n->hop.c_str(), n->data);
		n = n->next;
	}
	message[position] = '\0';

	/* 2. Get distance Vector and find out who I need to send to. */
	LinkedList* row = distance_vector.getHead();
	for(int i = 0 ; i < distance_vector.getsize() ; i++) {
		struct addrinfo hint;
		struct addrinfo *res;
		bzero(&hint, sizeof(hint));
		hint.ai_family = AF_INET;  // IPv4 protocol 
		hint.ai_socktype = SOCK_DGRAM; // UDP connection

		if (getaddrinfo(row->name.c_str(), (char*)PORT_NUMBER, &hint, &res) != 0) {
			cout << "\t\tError while trying to get address." << endl;
			row = row->next;
			continue;
		}


		/* 3. send the string over to the neighbors. */
		int s = socket(AF_INET,SOCK_DGRAM, 0);
		if (sendto(s, message, ::strlen(message)+1, 0, 
				(struct sockaddr *)res->ai_addr, res->ai_addrlen) == -1) {
			cout << "\t\tError while sending." << endl;
		}

		row = row->next;
		close(s);
	}
	pthread_mutex_unlock( &mutex1 );
}



int main(int argc, char** argv) {
	// must initialize this with the input file
	struct sockaddr_in socket_address;

	if(argc != 2) {
		cout <<"Usage: ./vector <FileName>\n";
		::exit(1);
	}

	/*
	 * Finding the name of this host.
	 * This is used by various places
	 */
	string name_of_this_host(argv[1]);
	int i = name_of_this_host.find(".dat");
	name_of_this_host = name_of_this_host.substr(0, i);

	/* 
	 * Initialize the table and vector
	 */
	LinkedLinkedList distanceVector;
	LinkedList table(name_of_this_host);

	unsigned short port = (unsigned short)atoi((char*)PORT_NUMBER);

	bzero(&socket_address, sizeof(socket_address));
	socket_address.sin_family = AF_INET;
	socket_address.sin_addr.s_addr = htonl(INADDR_ANY);
	socket_address.sin_port = htons(port);

	int sock = socket(AF_INET, SOCK_DGRAM, 0); // DGRAM meaning datagram for UDP

	if(bind(sock, (struct sockaddr *) &socket_address, sizeof(socket_address)) ) {
		cout << "error binding." << endl;
		exit(1);
	}

	// for the last two arguments, you need to have a struct and an int
	// it is only required if you want to know the sender's information such as their IP address.
	pthread_t thread;
	thread_arg arg;
	arg.socketfd = sock;
	arg.distance_vector = &distanceVector;
	arg.routing_table = &table;

	int status = pthread_create( &thread, NULL, receiving_thread, (void*)&arg);
	if(status != 0) {
		cout << "Error while making receiving thread." << endl;
		close(sock);
		exit(1);
	}
	pthread_detach(thread);

	/* this variable is only for the purpose of proper formatting */
	int session_number = 0;
	while(true) {
		sendTable(distanceVector, table, name_of_this_host.c_str());
		cout << "\n## sequence number " << session_number << endl;
		cout << "Shortest path from host: " << name_of_this_host << endl;
		table.printRoutingTable();
		session_number++;
		sleep(10);
	}
	close(sock);
}

