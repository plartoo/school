#ifndef _MYLINKEDLINKEDLIST
#define _MYLINKEDLINKEDLIST
#include "LL.h"
#include <string>
#include <math.h>
class LinkedLinkedList {
	LinkedList* head;
	int size;
public:
	LinkedLinkedList();
	~LinkedLinkedList();
	/*
	 * This method is explicitly for adding a list.
	 * Do not use this to add a new node in the graph that was found.
	 */
	LinkedList* addList(std::string name);
	/*
	 * This adds a node to each list named <name> from the parameter.
	 * The node will not be added if there is a node in the list
	 */
	int addToEachList(std::string, float data=INFINITY);
	LinkedList* findList(std::string name);
	Node* find(std::string hopname, std::string destname);
	LinkedList* getHead() {
		return head;
	}
	void removeList(std::string name);
	void printAll();
	int getsize() {
		return this->size;
	}
};
#endif
