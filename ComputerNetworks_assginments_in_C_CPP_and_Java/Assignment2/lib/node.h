#ifndef _MYNODE
#define _MYNODE
#include <string>

class Node {
public:
	std::string dest;
	std::string hop;
	std::string name;
	float data;
	Node* next;
	Node* getNext();
	Node(std::string name, float);
	Node(std::string, std::string, float);
	Node(std::string,std::string, std::string, float);
};

#endif
