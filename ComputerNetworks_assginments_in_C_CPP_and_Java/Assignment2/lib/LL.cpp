#include "LL.h"
#include <math.h>
#include <iostream>

/**********************
 * LinkedList definition
 *********************/
using namespace std;
LinkedList::LinkedList(string name) {
	head = NULL;
	// this 'next' variable is only used by the LinkedLinkedList class
	next = NULL;
	this->name = name;
	size = 0;
}

LinkedList::~LinkedList() {
	if (head != NULL) {
		Node* n = head;
		while(n != NULL) {
			Node* next = n->next;
			delete n;
			n = next;
		}
	}
}

int LinkedList::add(string name,float data=INFINITY) {
	// if head is null add a node, if not find the node that has next as NULL
	if(head == NULL) {
		size++;
		head = new Node(name, data);
		return 1;
	}
	else {
		Node* node = head;
		while(node->name != name && node->next != NULL) {
			node = node->next;
		}

		// there exists a same name node
		if(node->name == name) {
			return 0;
		}
		else {
			node->next = new Node(name, data);
			size++;
		}
	}
	return 1;
}

// same as above but more parameters for the node
int LinkedList::add(string dest, string hop, float data) {
	// if head is null add a node, if not find the node that has next as NULL
	if(head == NULL) {
		size++;
		head = new Node(dest,hop, data);
		return 1;
	}
	else {
		Node* node = head;
		while(node->dest != dest && node->next != NULL) {
			node = node->next;
		}

		if(node->dest == dest)
			return 0;
		else {
			node->next = new Node(dest, hop, data);
			size++;
		}
	}
	return 1;
}

int LinkedList::add(string name, string dest,string hop, float distance) {
	if(head == NULL) {
		size++;
		head = new Node(name,dest,hop, distance);
		return 1;
	}
	else {
		Node* node = head;
		while(node->dest != dest && node->next != NULL) {
			node = node->next;
		}

		if(node->dest == dest)
			return 0;
		else {
			node->next = new Node(name,dest, hop, distance);
			size++;
		}
	}
	return 1;
}

void LinkedList::remove(string name) {
	/*
	 * Check if the head is the that needs to be deleted,
	 * if so delete,
	 * if not keep track of the previous node while searching.
	 */
	if(head == NULL)
		return;
	if(head->name == name) {
		Node* node = this->head;
		this->head = head->next;
		delete node;
		size--;
		return;
	}

	Node* prev = this->head;
	Node* node = (this->head)->next;
	while(node != NULL) {
		if(node->name == name) {
			prev->next = node->next;
			delete node;
			size--;
			return;
		}
		else {
			prev = node;
			node = node->next;
		}
	}
	return;
}
/*
 * The list class is equivalent when all the nodes are equivalent
 * the ordering does not matter
 */
bool LinkedList::operator==(const LinkedList& rhs) {
	Node *rhsnode = rhs.getHead();

	int size1 = rhs.getsize();
	int size2 = this->getsize();
	if(size1 != size2) {
		return false;
	}

	while(rhsnode != NULL) {
		Node* n = this->find(rhsnode->name);
		if (n == NULL) {
			return false;
		}
		if(n->dest != rhsnode->dest ||
				n->data != rhsnode->data ||
				n->hop != rhsnode->hop) {
			return false;
		}
		rhsnode = rhsnode->next;
	}
	return true;
}

LinkedList& LinkedList::operator=(const LinkedList& rhs) {
	Node *n = this->head;
	while(n != NULL) {
		Node *next = n->next;
		delete n;
		n = next;
	}
	this->head = NULL;
	this->size = 0;
	this->next = rhs.next;
	this->name = rhs.name;
	n = rhs.getHead();
	while(n != NULL) {
		this->add(n->name,n->dest, n->hop, n->data);
		n = n->next;
	}
	return *this;
}

Node* LinkedList::find(string name) {
	Node* n = head;
	while(n != NULL) {
		if(n->name == name)
			return n;
		n = n->next;
	}
	return NULL;
}

void LinkedList::printAll() {
	Node* n = head;
	cout << this->name << ": ";
	while(n != NULL) {
		cout << n->name << ", " << n->data << "; ";
		n = n->next;
	}
	cout << endl;
}

void LinkedList::printRoutingTable() {
	cout << "To\t\t\t" << "Next hop\t\t\t" << "Cost" << endl;
	cout << "==\t\t\t" << "========\t\t\t" << "====" << endl;
	for(Node* n = head; n != NULL ; n = n->next) {
		cout << n->dest << "\t" << n->hop << "\t\t" << n->data << endl;
	}
}

void LinkedList::printTable() {
	Node* n = head;
	cout << this->name << ": ";
	while(n != NULL) {
		cout << n->dest << ", " << n->hop << ", " << n->data << "; ";
		n = n->next;
	}
	cout << endl;
}

