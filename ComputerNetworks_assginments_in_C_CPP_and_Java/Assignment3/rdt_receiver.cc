/*
 * FILE: rdt_receiver.cc
 * DESCRIPTION: Reliable data transfer receiver.
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>

#include "rdt_struct.h"
#include "rdt_receiver.h"
#include "MyMacros.h"

static void makePacketForSender(struct packet *p, int ack_number);

/* 
 * This will take in a message struct and a packet list and finds all the packets
 * that can be coalesced to make a message.
 * This returns the position the packet list circular should point to next.
 * The last argument is very non elegant but C has no way to return two values so it gets modfied in the function
 */
static int makeMessageFromPacketList(struct message *m, const struct packet *p, int pos, unsigned short *expected_number);

// static unsigned long count_for_debug = 0;

static unsigned short InternetChecksum(struct packet* p);
static unsigned long headerSum(struct packet* p);
static unsigned long bodySum(struct packet* p);
static bool check(struct packet *p);
static void copyPacket(struct packet *destination, const struct packet *source);

struct packet packet_list[WINDOW_SIZE];

using namespace std;

/* receiver initialization, called once at the very beginning */
void Receiver_Init() {
    fprintf(stdout, "At %.2fs: receiver initializing ...\n", GetSimulationTime());
	for(int i = 0; i < WINDOW_SIZE ; i++) {
		SET_SEQ((packet_list+i), 0);
	}
}

/* receiver finalization, called once at the very end.
   you may find that you don't need it, in which case you can leave it blank.
   in certain cases, you might want to use this opportunity to release some 
   memory you allocated in Receiver_init(). */
void Receiver_Final() {
    fprintf(stdout, "At %.2fs: receiver finalizing ...\n", GetSimulationTime());
	// cout << count_for_debug << endl;
}

/* event handler, called when a packet is passed from the lower layer at the 
   receiver */
void Receiver_FromLowerLayer(struct packet *pkt) {
	static unsigned short expected_SEQ_number = (unsigned short)1;
	static int position = 0;
	unsigned short seq_number = GET_SEQ_NUMBER(pkt);
	struct packet pack;

	/* checksum */
	if(!check(pkt)) {
		// send off an ACK that says that it was bad 
		makePacketForSender(&pack, expected_SEQ_number - 1);
		Receiver_ToLowerLayer(&pack);
		return;
	}

	/* too old info */
	if(seq_number < expected_SEQ_number) {
		// cout << "mis match " << seq_number << "!=" << expected_SEQ_number << endl;
		/* overflow but if good buffer it*/
		if(seq_number < WINDOW_SIZE &&
				(unsigned short)(expected_SEQ_number + (unsigned short)WINDOW_SIZE) < WINDOW_SIZE) {
			/* Taking the difference 
			 * the maximum expected_SEQ_number can be is 0xFFFF */
			int diff = 0x0000FFFF - expected_SEQ_number;
			diff += seq_number;
			int index = (position + diff) % WINDOW_SIZE ;
			copyPacket(packet_list + index, pkt);
			makePacketForSender(&pack, expected_SEQ_number- 1);
		}
		else {
			makePacketForSender(&pack, seq_number);
		}
		Receiver_ToLowerLayer(&pack);
		/* return here. The receiver should get message in order */
		return;
	}
	/* out of order info */
	else if(seq_number > expected_SEQ_number) {
		int diff = seq_number - expected_SEQ_number;
		/* if in reasonable size but it does not make sense not to be in this range... */
		if(diff < WINDOW_SIZE) {
			int index = (position + diff) % WINDOW_SIZE ;
			copyPacket(packet_list + index, pkt);
		}
		makePacketForSender(&pack, expected_SEQ_number - 1);
		Receiver_ToLowerLayer(&pack);

		return;
	}
	/* ACCEPT in order finally */
	else {
		copyPacket(packet_list + position, pkt);
	}

	// cout << "Received: The size for sequence number "<< seq_number << " is "  << GET_SIZE(pkt)<< endl;
	/* copy the data and send to upper layer */
	struct message m;

	/* make the message but try to combine as much consecutive packets as possible */
	position = makeMessageFromPacketList(&m, packet_list, position, &expected_SEQ_number);
    Receiver_ToUpperLayer(&m);

    /* don't forget to free the space */
    if (m.data!=NULL) delete m.data;
}

static int makeMessageFromPacketList(struct message *m, const struct packet *list, int pos, unsigned short *expect) {
	unsigned short seq = GET_SEQ_NUMBER((list+pos));
	struct packet pk;
	int message_length = 0;
	int position = pos;
	int i ;
	for(i = 0 ; i < WINDOW_SIZE ; i++) {
		if(seq == GET_SEQ_NUMBER((list+position))) {
			message_length += GET_SIZE((list+position));
			/* send acks for each packet */
			makePacketForSender(&pk, seq);
			Receiver_ToLowerLayer(&pk);
		}
		else {
			break;
		}
		/* update */
		seq++;
		position = (position + 1) % WINDOW_SIZE;
	}

	*expect = (unsigned short)(seq);
	m->size = message_length;
	m->data = new char[message_length];
	int index = 0;
	position = pos;
	for(int j = 0 ; j < i ; j++) {
		strncpy(m->data + index, list[position].data + HEADER_SIZE , GET_SIZE((list+position)));
		index += GET_SIZE((list+position));
		position = (position + 1) % WINDOW_SIZE;
	}
	return position;
}

/* might add checksum later */
static void makePacketForSender(struct packet *p, int ack_number) {
	SET_SIZE(p, 0);
	SET_SEQ(p, 0);
	SET_ACK(p, ack_number);

	unsigned short checksum = InternetChecksum(p);
	SET_CHECKSUM(p, checksum);
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
		cout << "The SEQ number could be " << GET_SEQ_NUMBER(p) << endl;
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
		cout << "The SEQ number could be " << GET_SEQ_NUMBER(p) << endl;
		printf("%x\n", (unsigned)sum);
*/
		return false;
	}
}

static void copyPacket(struct packet *destination, const struct packet *source) {
	int size = GET_SIZE(source) + HEADER_SIZE;
	for(int i = 0  ; i < size ; i++ ) {
		destination->data[i] = source->data[i];
	}
}

