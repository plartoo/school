#ifndef _MYLINKEDLIST
#define _MYLINKEDLIST
#include <string>
#include "node.h"

class LinkedList {
	Node* head;
	int size;
public:
	/*
	 * The pointer to the next linked list.
	 * So, this data structure allows list of lists.
	 */
	LinkedList* next;

	std::string name;
	LinkedList(std::string); 
	~LinkedList(); 
	Node* getHead() const {
		return head;
	}

	/*
	 * These add method will not add the node with same name
	 */
	int add(std::string name, float distance);
	int add(std::string dest, std::string hop, float distance);
	int add(std::string name, std::string dest, std::string hop, float distance);
	void remove(std::string);
	bool operator==(const LinkedList& rhs);
	LinkedList& operator=(const LinkedList& rhs);
	/*
	 * Linear searches through the list.
	 * Returns NULL if not found.
	 */
	Node* find(std::string);
	void printAll();
	void printTable();
	/*
	 * Use this only for routing tables.
	 */
	void printRoutingTable();
	int getsize() const {
		return size;
	}
};
#endif
