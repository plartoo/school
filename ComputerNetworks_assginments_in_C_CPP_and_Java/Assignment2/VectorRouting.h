#ifndef _VR
#define _VR
#include <string>
#include "lib/LL.h"
#include "lib/LLL.h"
#include <fstream>

/*
 * Return true if the table has changed
 */
static bool update_table(char*, LinkedLinkedList&, LinkedList&);

/*
 * This updates the distance vector, by looking at the file.
 * If the distance vector is not made yet, then this builds the vector from the
 * input file.
 * If using mutex, then it should be locked before calling this function.
 *
 * The filename should be the host name + .dat
 */
static void makeDistanceVector(std::string, LinkedLinkedList&);

/*
 * The argument of this function, 'table' assumes that there is a table already made
 */
static void sendTable(LinkedLinkedList& distance_vector, LinkedList& table,const char* name_of_this_host) ;

static void buildTable(LinkedLinkedList &list, LinkedList& table) ;
void* receiving_thread(void* args);

/*
 * pthread arugment struct.
 */
typedef struct {
	int socketfd;
	LinkedLinkedList* distance_vector;
	LinkedList* routing_table;
} thread_arg;

#endif

