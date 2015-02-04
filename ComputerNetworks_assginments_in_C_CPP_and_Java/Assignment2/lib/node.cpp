#include "node.h"

/*******************
 * Node methods definition
 ******************/
using namespace std;
Node::Node(string name, float data) {
	next = NULL;
	this->data = data;
	this->name = name;
	this->dest = "";
	this->hop  = "";
}

Node::Node(string dest, string hop, float data) {
	next = NULL;
	this->dest = dest;
	this->hop  = hop;
	this->data = data;
	this->name = "";
}
Node::Node(std::string name ,std::string dest , std::string hop, float data) {
	next = NULL;
	this->name = name;
	this->dest = dest;
	this->hop  = hop;
	this->data = data;
}

Node* Node::getNext() {
	return next;
}

