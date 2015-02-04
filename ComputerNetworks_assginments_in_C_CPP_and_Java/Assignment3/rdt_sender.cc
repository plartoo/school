/*
 * FILE: rdt_sender.cc
 * DESCRIPTION: Reliable data transfer sender.
 * VERSION: 0.2
 * AUTHOR: Kai Shen (kshen@cs.rochester.edu)
 * NOTE: This implementation assumes there is no packet loss, corruption, or 
 *       reordering.  You will need to enhance it to deal with all these 
 *       situations.  In this implementation, the packet format is laid out as 
 *       the following:
 *       
 *       |<-  1 byte  ->|<-             the rest            ->|
 *       | payload size |<-             payload             ->|
 *
 *       The first byte of each packet indicates the size of the payload
 *       (excluding this single-byte header)
 */
/* the structure of the packet is changed the comment above is not right */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <list>
#include <iostream>

#include "rdt_struct.h"
#include "rdt_sender.h"
#include "MyMacros.h"
using namespace std;

/* declared later */
class Node;

static void resend();
static int sendNext();
static void makePacketFromNode(struct packet* pkt, const Node* n);
static unsigned short InternetChecksum(struct packet* p);
static unsigned long headerSum(struct packet* p);
static unsigned long bodySum(struct packet* p);

/* does checksum on p.
 * return true if not corrupted
 */
static bool check(struct packet *p);

/* convenient to start seq# from 1 for the receiver side 
 * and getting a reply
 */
static unsigned short seq_number = (unsigned short)1;
/* the value that stores how many packets are being sent now */
static int now_sending = 0;

/* debugging variables */
// static unsigned long all_count_for_debug = 0;
// static unsigned long number_of_chars_to_be_sent = 0;

class Node {
public:
	char *message;
	int size;
	unsigned short acknumber;
	unsigned short seqnumber;
	double time;
	bool sending;

	Node(int size = 0) {
		message = new char[size];
		this->size = size;
		sending = false;
	}

	~Node() {
		delete message;
	}
	Node& operator=(const Node &n);
};

Node& Node::operator=(const Node &n) {
	delete this->message;
	this->message = new char[n.size];
	strncpy(this->message, n.message, n.size);
	this->size = n.size;
	this->acknumber = n.acknumber;
	this->seqnumber = n.seqnumber;
	this->time = n.time;
	this->sending = n.sending;

	return (*this);
}


class MyPacketList : public list<Node> {
public:
	MyPacketList() : list<Node>::list() {}
	/* Takes a message with its size and makes packets.
	 * Those packets will be stored into the list
	 */
	void addPackets(char *message, int size);
};

void MyPacketList::addPackets(char* message, int remaining_message_length) {
	// number_of_chars_to_be_sent += remaining_message_length; // debug purpose

	/* offset of the message variable */
	int position = 0;
	/* The malloced node will be freed at pop_first method */
	while(remaining_message_length > 0) {
		if(remaining_message_length >= MAX_BODY_SIZE) {
			Node* n = new Node( MAX_BODY_SIZE );
			strncpy(n->message, message+position, MAX_BODY_SIZE);
			position += MAX_BODY_SIZE;

			n->seqnumber = seq_number;
			seq_number++;

			push_back(*n);
			remaining_message_length -= MAX_BODY_SIZE;
		}
		else {
			Node* n = new Node( remaining_message_length );
			strncpy(n->message, message+position, remaining_message_length);
			position += remaining_message_length;

			n->seqnumber = seq_number;
			seq_number++;

			push_back(*n);
			remaining_message_length = 0;
		}
	}
}

MyPacketList listPacket;

/* sender initialization, called once at the very beginning */
void Sender_Init()
{
    fprintf(stdout, "At %.2fs: sender initializing ...\n", GetSimulationTime());
}

/* sender finalization, called once at the very end.
   you may find that you don't need it, in which case you can leave it blank.
   in certain cases, you might want to take this opportunity to release some 
   memory you allocated in Sender_init(). */
void Sender_Final()
{
    fprintf(stdout, "At %.2fs: sender finalizing ...\n", GetSimulationTime());
	// cout << all_count_for_debug << endl;
	// cout << number_of_chars_to_be_sent  << endl;
}

/* event handler, called when a message is passed from the upper layer at the 
   sender */
void Sender_FromUpperLayer(struct message *msg)
{
	listPacket.addPackets(msg->data, msg->size);
	sendNext();
}

/* event handler, called when a packet is passed from the lower layer at the 
   sender */
void Sender_FromLowerLayer(struct packet *pkt)
{
	static unsigned short Last_ACK_Number = (unsigned short)0;
	/* this is for counting the number of duplicate ACKs */
	static int same_ACK_count = 0;

/* macro for checking if resende is necessary */
#define TRIPLE_ACK_CHECK(val) \
	if(val >= 3) {\
		resend();\
		val = 0;\
	}

	/* Internet Checksum check to see if this packet is worth accepting */
	bool corrupted = !check(pkt);
	if(corrupted) {
		now_sending--;
		/* corrupted => assume same ACK */
		same_ACK_count++;
		TRIPLE_ACK_CHECK(same_ACK_count);
		sendNext();
		return;
	}

	unsigned int ACK_number = (unsigned short)GET_ACK_NUMBER(pkt);

	/* if the receiver got the lastACK + 1 then it is good 
	 * If the ACK# was not lastAKC + 1 then it might have been two cases that still is acceptable
	 * 	1. the ACK# was too small but overflow happened
	 * 	2. the ACK# was too big but it was within the range
	 * 	Both of these cases are covered by the else if CONDITIONs
	 *
	 * 	Note that for this to work, ACK_number must be unsigned
	 */
	if (Last_ACK_Number == ACK_number) {
		now_sending--;
		same_ACK_count++;
		TRIPLE_ACK_CHECK(same_ACK_count);
		sendNext();
	}
	else if(((unsigned short)(Last_ACK_Number + 1)) == ACK_number) {
		// cout << "in Sender_fromlower layer: ack in sequence" <<endl;
		same_ACK_count = 0;
		Last_ACK_Number++;
		if(listPacket.size() > 0) {
			listPacket.pop_front();
		}
		sendNext();

		/* getting the next timeout time */
		Node& n = listPacket.front();
		double x = n.time + TIME_OUT_TIME - (GetSimulationTime() - n.time);
		Sender_StartTimer(x);
	}
	/*
	 * Note:
	 *  The two else ifs below has same computation except for the conditions.
	 *  I think this is the most reasonable and readable implementation.
	 *
	 *  I am doing this because
	 *  if(a)
	 *   if(b)
	 *  else if(c)
	 *   if(d)
	 *  is not equivalent to 
	 *  if(a||b)
	 *   if(b||d)
	 *  
	 *  I beleive that combining the conditions for this situation will complicate
	 *  things too much.
	 */
	/* overflow situation or just too old ACK*/
	else if(((unsigned)ACK_number) < ((unsigned)Last_ACK_Number)) {
		/* When there is an overflow do this 
		 * else just ignore it, the info is too old
		 */
		if(ACK_number < WINDOW_SIZE &&
				(unsigned)(Last_ACK_Number + WINDOW_SIZE) < WINDOW_SIZE) {
			same_ACK_count = 0;
			while((Last_ACK_Number != ACK_number) && now_sending > 0) {
				Last_ACK_Number++;
				if(listPacket.size() > 0) {
					listPacket.pop_front();
				}
				now_sending--;
			}

			sendNext();

			/* getting the next timeout time */
			Node& n = listPacket.front();
			double x = n.time + TIME_OUT_TIME - (GetSimulationTime() - n.time);
			Sender_StartTimer(x);
		}
		else {
			/* do not decrement now_sending here because it has been in the if case */
			same_ACK_count = 0;
			sendNext();
		}
	}
	/*  if too big but in good range, then accept */
	else if(((unsigned short)(Last_ACK_Number + (unsigned short)1)) < ACK_number) {
		/* 
		 * good range 
		 * The OR part says that if Last_ACK_Number overflows, then it is automatically ok 
		 */
		if(ACK_number <= ((unsigned)(Last_ACK_Number + now_sending)) ||
				(unsigned)(Last_ACK_Number + now_sending) < WINDOW_SIZE) {
			same_ACK_count = 0;
			while(Last_ACK_Number != ACK_number && now_sending > 0) {
				Last_ACK_Number++;
				if(listPacket.size() > 0) {
					listPacket.pop_front();
				}
				now_sending--;
			}

			sendNext();

			/* getting the next timeout time */
			Node& n = listPacket.front();
			double x = n.time + TIME_OUT_TIME - (GetSimulationTime() - n.time);
			Sender_StartTimer(x);
		}
		else {
			same_ACK_count = 0;
			sendNext();
		}
	}
}

/* This function will set all the nodes to 
 * node.sending = false;
 * so that when sendNext is called, it will resend the node from the beginning
 */
static void resend() {
	MyPacketList::iterator it;
	for(it = listPacket.begin(); it != listPacket.end() && it->sending == true ; it++) {
		it->sending = false;
	}
	/* stop timer for now */
	Sender_StopTimer();
	sendNext();
}

static int sendNext() {
	struct packet parray[WINDOW_SIZE];

	/* Find the unsent packets */
	MyPacketList::iterator it;
	int count = 0;
	int number_of_packets_being_sent = 0;
	for(it = listPacket.begin(); (it != listPacket.end()) && number_of_packets_being_sent < WINDOW_SIZE; it++) {
		/* skip the ones that are being sent now */
		if( !it->sending ) {
			/* Set the values then make packet. The order is crucial */
			it->sending = true;
			it->time = GetSimulationTime();
			makePacketFromNode(parray + count, &(*it));

			/* debug print */
/*
			struct packet *debugp = parray + count;
			cout << "Sending: The size for sequence number "<< GET_SEQ_NUMBER(debugp) << " is "  << ((unsigned short)GET_SIZE(debugp)) << endl;
			all_count_for_debug += GET_SIZE(debugp);
*/
			/* end debug section */

			/* start timer if necessary because it might have been after timeout or resend */
			if( !Sender_isTimerSet() ) {
				Sender_StartTimer(TIME_OUT_TIME);
			}
			Sender_ToLowerLayer(parray + count);
			
			/* updates */
			count++;
		}
		number_of_packets_being_sent++;
	}
	now_sending = number_of_packets_being_sent;
	return 1;
}

/* the node should contain all the info necessary for a packet */
static void makePacketFromNode(struct packet* pkt, const Node* n) {
	unsigned short seq_num = n->seqnumber;
	SET_SEQ(pkt, seq_num);
	SET_SIZE(pkt, n->size);
	SET_ACK(pkt, 0);

	/* copy the characters */
	strncpy(pkt->data + HEADER_SIZE, n->message, n->size);

	/* do checksum and set */
	unsigned short sum = InternetChecksum(pkt);
	SET_CHECKSUM(pkt, sum);
}

static unsigned short InternetChecksum(struct packet *p) {
	unsigned long sum = 0;
	sum += headerSum(p);

	/* body sum */
	sum += bodySum(p);

	/* add the overflowed bit */
	sum = ((sum >> 16) & 0xFFFF) + (sum & 0xFFFF);
	/* it is possible that the previous one overflowed */
	sum = ((sum >> 16) & 0xFFFF) + (sum & 0xFFFF);

	unsigned short checksumvalue = sum & 0xFFFF;
	/* take one's complement */
	checksumvalue = ~checksumvalue;
	return checksumvalue;
}

static unsigned long headerSum(struct packet *p) {
	unsigned long sum = 0;
	sum += ((int)p->data[SIZE_POSITION] << 8) & 0xFF00;
	sum += (int)p->data[SEQ_NUMBER_POSITION] & 0xFF;
	sum += ((int)p->data[SEQ_NUMBER_POSITION+1] << 8) & 0xFF00;
	sum += (int)p->data[ACK_NUMBER_POSITION] & 0xFF;
	sum += ((int)p->data[ACK_NUMBER_POSITION+1] << 8) & 0xFF00;
	return sum;
}

static unsigned long bodySum(struct packet *p) {
	unsigned long sum = 0;
	int size = GET_SIZE(p);
	// HEADER SIZE is 7 so start from adding with mask 0xFF rather than 0xFF00
	for(int i = HEADER_SIZE; i < size + HEADER_SIZE ; i+=2) {
		sum += ((unsigned long)p->data[i]) & 0xFF;
	}
	for(int j = HEADER_SIZE + 1; j < size + HEADER_SIZE ; j+=2) {
		sum += (((unsigned long)p->data[j])<<8) & 0xFF00;
	}
	return sum;
}

static bool check(struct packet *p) {
	/* sanity check */
	if(GET_SIZE(p) > MAX_BODY_SIZE) {
/*
		cout << "CORRUPTED" << endl;
		cout << "The size is too big" << endl;
		cout << "The ACK number could be " << GET_SEQ_NUMBER(p) << endl;
*/
		return false;
	}

	/* add the entire thing */
	unsigned long sum = 0;
	sum += bodySum(p);
	sum += headerSum(p);
	sum += GET_CHECKSUM(p);

	/* add the overflowed bit */
	sum = ((sum >> 16) & 0xFFFF) + (sum & 0xFFFF);
	/* it is possible that the previous one also overflowed */
	sum = ((sum >> 16) & 0xFFFF) + (sum & 0xFFFF);

	if(sum == 0xFFFF) {
		return true;
	}
	else {
/*
		cout << "CORRUPTED" << endl;
		cout << "The ACK number could be " << GET_SEQ_NUMBER(p) << endl;
		printf("%x\n", (unsigned)sum);
*/
		return false;
	}
}

/* event handler, called when the timer expires */
void Sender_Timeout() {
	/* stop timer just in case */
	Sender_StopTimer();
	// cout << "\t\tTimeout" << endl;
	// cout << "\t\tTimed out at " << GetSimulationTime() << endl;
	resend();
}

