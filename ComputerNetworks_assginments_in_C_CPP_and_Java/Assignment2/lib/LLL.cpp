#include "node.h"
#include "LL.h"
#include "LLL.h"
#include <iostream>
#include <math.h>

using namespace std;
/****************
 * LinkedLinkedList method definitions
 **************/
LinkedLinkedList::LinkedLinkedList() {
	head = NULL;
	size = 0;
}
LinkedLinkedList::~LinkedLinkedList() {
	LinkedList* list = head;
	if(list != NULL) {
		LinkedList* next = list->next;
		delete list;
		list = next;
	}
}

/* 
 * The method must add a node at the end of each LinkedList
 * because this is a square table structure.
 */
LinkedList* LinkedLinkedList::addList(string name) {
	size++;
	if(head == NULL) {
		head = new LinkedList(name);
		//head->add(INFINITY, name);
		return head;
	}
	else {
		LinkedList* list = head->next;
		LinkedList* prev = head;
		// make a new linked list
		while(list != NULL) {
			prev = list;
			LinkedList* next = list->next;
			list = next;
		}
		list = new LinkedList(name);
		// connect the list
		prev->next = list;
		return list;
	}
}

int LinkedLinkedList::addToEachList(string name, float val) {
	int count = 0;
	if(head == NULL)
		return 0;
	else {
		LinkedList* list = head;
		while(list!=NULL) {
			count += list->add(name, val);
			list = list->next;
		}
		// cout << "Adding " << name << " to each list." << endl;
	}
	return count;
}

LinkedList* LinkedLinkedList::findList(string name) {
	LinkedList* list = head;
	while(list != NULL && list->name != name) {
		list = list->next;
	}
	return list;
}

Node* LinkedLinkedList::find(string hopname, string destname) {
	LinkedList* row = this->findList(hopname);
	if (row == NULL) {
		return NULL;
	}
	return row->find(destname);
}

void LinkedLinkedList::removeList(string name) {
	if(head == NULL) {
		return;
	}
	else {
		LinkedList* list = head;
		while(list != NULL && list->name != name) {
			list = list->next;
		}
		if (list != NULL) {
			delete list;
		}
		return;
	}
}

void LinkedLinkedList::printAll() {
	LinkedList* list = head;
	while(list != NULL) {
		list->printAll();
		list = list->next;
	}
}

